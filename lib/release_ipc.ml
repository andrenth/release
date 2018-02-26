open Printf
open Lwt.Infix
open Release_util

module B = Release_buffer

type connection = Lwt_unix.file_descr * Lwt_mutex.t
type handler = connection -> unit Lwt.t

let create_connection sock =
  (sock, Lwt_mutex.create ())

let control_socket path handler =
  let pid = Unix.getpid () in
  Lwt.catch
    (fun () ->
      let sock = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      let addr = Lwt_unix.ADDR_UNIX path in
      Release_socket.accept_loop sock addr
        (fun sock -> handler (create_connection sock)))
    (function
    | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
        Lwt_log.error_f
          "control socket `%s' already exists (%d)" path pid >>= fun () ->
        exit 1
    | Unix.Unix_error (e, _, _) ->
        let err = Unix.error_message e in
        Lwt_log.error_f
          "control socket `%s' error (%d): %s" path pid err >>= fun () ->
        exit 1
    | e ->
        let err = Printexc.to_string e in
        Lwt_log.error_f
          "control socket `%s' error (%d): %s" path pid err >>= fun () ->
        Lwt.fail e)

module type Types = sig
  type request
  type response
end

module type Ops = sig
  include Types

  val string_of_request : request -> string
  val request_of_string : string -> request

  val string_of_response : response -> string
  val response_of_string : string -> response
end

module Marshal = struct
  module Make (T : Types) : Ops
    with type request := T.request and type response := T.response =
  struct
    type request = T.request
    type response = T.response

    let string_of_request r = Marshal.to_string r []
    let request_of_string s = Marshal.from_string s 0

    let string_of_response r = Marshal.to_string r []
    let response_of_string s = Marshal.from_string s 0
  end
end

module type S = sig
  type request
  type response

  module Server : sig
    val read_request : ?timeout:float
                    -> connection
                    -> [`Request of request | `EOF | `Timeout] Lwt.t

    val write_response : connection -> response -> unit Lwt.t

    val handle_request : ?timeout:float
                      -> ?eof_warning:bool
                      -> connection
                      -> (request -> response Lwt.t)
                      -> unit Lwt.t
  end

  module Client : sig
    val read_response : ?timeout:float
                     -> connection
                     -> [`Response of response | `EOF | `Timeout] Lwt.t

    val write_request : connection -> request -> unit Lwt.t

    val make_request : ?timeout:float
                    -> connection
                    -> request
                    -> ([`Response of response | `EOF | `Timeout] ->
                          'a Lwt.t)
                    -> 'a Lwt.t
  end
end

module Make (O : Ops) : S
  with type request := O.request and type response := O.response =
struct
  (*
   * Do the header operations by hand to avoid forcing a
   * dependency on Release_bytes.
   *
   * IPC header is a 4-byte field containing the payload
   * size, stored in network byte order.
   *)

  let header_length = 4

  let read_byte_at i buf =
    int_of_char (B.get buf i)

  exception Overflow

  let close_connection conn =
    let fd, mtx = conn in
    Lwt_mutex.with_lock mtx (fun () -> Lwt_unix.close fd)

  let read_header =
    Release_bytes.Big_endian.read_int

  let write_header =
    Release_bytes.Big_endian.write_int

  let read_header_and_payload ?timeout fd =
    Release_io.read ?timeout fd header_length >>= function
    | `Timeout | `EOF as other ->
        Lwt.return other
    | `Data b ->
        try Release_io.read ?timeout fd (read_header b)
        with Overflow -> Lwt.fail (Failure "IPC header length overflow")

  let read ?timeout conn =
    let (sock, mutex) = conn in
    Lwt_mutex.with_lock mutex
      (fun () -> read_header_and_payload ?timeout sock)

  let write conn buf =
    let len = B.length buf in
    let buf' = B.create (len + header_length) in
    write_header len buf';
    B.blit buf 0 buf' header_length len;
    let fd, mtx = conn in
    Lwt_mutex.with_lock mtx (fun () -> Release_io.write fd buf')

  let request_of_buffer buf =
    O.request_of_string (B.to_string buf)

  let buffer_of_request req =
    B.of_string (O.string_of_request req)

  let response_of_buffer buf =
    O.response_of_string (B.to_string buf)

  let buffer_of_response resp =
    B.of_string (O.string_of_response resp)

  module Server = struct
    let read_request ?timeout sock =
      read ?timeout sock >>= function
      | `Data buf -> Lwt.return (`Request (request_of_buffer buf))
      | `Timeout | `EOF as other -> Lwt.return other

    let write_response sock resp =
      write sock (buffer_of_response resp)

    let handle_request ?timeout ?(eof_warning = true) conn handler =
      let rec handle_req () =
        read_request ?timeout conn >>= function
        | `Timeout ->
            close_connection conn >>= fun () ->
            Lwt_log.error "read from a slave shouldn't timeout" >>= fun () ->
            exit 1
        | `EOF ->
            if eof_warning then Lwt_log.error "got EOF on IPC socket"
            else Lwt.return_unit >>= fun () ->
            close_connection conn
        | `Request req ->
            Lwt.catch
              (fun () ->
                handler req >>= fun resp ->
                write_response conn resp >>= fun () ->
                handle_req ())
              (fun e ->
                let err = Printexc.to_string e in
                Lwt_log.error_f "request handler exception: %s" err
                >>= fun () ->
                close_connection conn) in
      handle_req ()
  end

  module Client = struct
    let read_response ?timeout conn =
      read ?timeout conn >>= function
      | `Data buf -> Lwt.return (`Response (response_of_buffer buf))
      | `Timeout | `EOF as other -> Lwt.return other

    let write_request conn req =
      write conn (buffer_of_request req)

    let make_request ?timeout conn req handler =
      write_request conn req >>= fun () ->
      read_response ?timeout conn >>= fun res ->
      handler res
  end
end
