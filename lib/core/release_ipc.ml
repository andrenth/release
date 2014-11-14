open Lwt
open Printf

module B = Release_buffer

type handler = (Lwt_unix.file_descr -> unit Lwt.t)

let control_socket path handler =
  Lwt.catch
    (fun () ->
      let sockaddr = Lwt_unix.ADDR_UNIX path in
      Release_socket.accept_loop Lwt_unix.SOCK_STREAM sockaddr handler)
    (function
    | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
        Lwt_log.error_f "control socket `%s' already exists" path >>= fun () ->
        exit 1
    | e ->
        Lwt.fail e)

module type Ops = sig
  type request
  type response

  val string_of_request : request -> string
  val request_of_string : string -> request

  val string_of_response : response -> string
  val response_of_string : string -> response
end

module type S = sig
  type request
  type response

  module Server : sig
    val read_request : ?timeout:float
                    -> Lwt_unix.file_descr
                    -> [`Request of request | `EOF | `Timeout] Lwt.t

    val write_response : Lwt_unix.file_descr -> response -> unit Lwt.t

    val handle_request : ?timeout:float
                      -> ?eof_warning:bool
                      -> Lwt_unix.file_descr
                      -> (request -> response Lwt.t)
                      -> unit Lwt.t
  end

  module Client : sig
    val read_response : ?timeout:float
                     -> Lwt_unix.file_descr
                     -> [`Response of response | `EOF | `Timeout] Lwt.t

    val write_request : Lwt_unix.file_descr -> request -> unit Lwt.t

    val make_request : ?timeout:float
                    -> Lwt_unix.file_descr
                    -> request
                    -> ([`Response of response | `EOF | `Timeout] -> 'a Lwt.t)
                    -> 'a Lwt.t
  end
end

module Make (O : Ops) : S
  with type request = O.request and type response = O.response =
struct
  type request = O.request
  type response = O.response

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

  let read_header buf =
    let res = ref 0 in
    for b = 1 to header_length do
      let pos = b - 1 in
      let byte = read_byte_at pos buf in
      let r = !res lor (byte lsl (32 - 8 * b)) in
      if r < !res then
        raise Overflow
      else
        res := r
    done;
    !res

  let write_byte b buf =
    B.add_char buf (char_of_int (b land 255))

  let write_header len buf =
    for b = 4 downto 1 do
      let shift = 8 * (b - 1) in
      write_byte (len lsr shift) buf
    done

  let read ?timeout fd =
    Release_io.read ?timeout fd header_length >>= function
    | `Timeout | `EOF as other ->
        return other
    | `Data b ->
        try Release_io.read ?timeout fd (read_header b)
        with Overflow -> Lwt.fail (Failure "IPC header length overflow")

  let write fd buf =
    let len = B.length buf in
    let buf' = B.create (len + header_length) in
    write_header len buf';
    B.blit buf 0 buf' header_length len;
    Release_io.write fd buf'

  let request_of_buffer buf =
    O.request_of_string (B.to_string buf)

  let buffer_of_request req =
    B.of_string (O.string_of_request req)

  let response_of_buffer buf =
    O.response_of_string (B.to_string buf)

  let buffer_of_response resp =
    B.of_string (O.string_of_response resp)

  module Server = struct
    let read_request ?timeout fd =
      read ?timeout fd >>= function
      | `Data buf -> return (`Request (request_of_buffer buf))
      | `Timeout | `EOF as other -> return other

    let write_response fd resp =
      write fd (buffer_of_response resp)

    let handle_request ?timeout ?(eof_warning = true) fd handler =
      let rec handle_req () =
        read_request ?timeout fd >>= function
        | `Timeout ->
            Lwt_unix.close fd >>= fun () ->
            Lwt.fail (Failure "read from slave shouldn't timeout")
        | `EOF ->
            if eof_warning then Lwt_log.warning "got EOF on IPC socket"
            else return_unit >>= fun () ->
            Lwt_unix.close fd
        | `Request req ->
            Lwt.catch
              (fun () ->
                handler req >>= fun resp ->
                write_response fd resp)
              (fun e ->
                let err = Printexc.to_string e in
                Lwt_log.error_f "request handler exception: %s" err
                >>= fun () ->
                Lwt_unix.close fd) >>= fun () ->
        handle_req () in
      handle_req ()
  end

  module Client = struct
    let read_response ?timeout fd =
      read ?timeout fd >>= function
      | `Data buf -> return (`Response (response_of_buffer buf))
      | `Timeout | `EOF as other -> return other

    let write_request fd req =
      write fd (buffer_of_request req)

    let make_request ?timeout fd req handler =
      write_request fd req >>= fun () ->
      read_response ?timeout fd >>= fun res ->
      handler res
  end
end
