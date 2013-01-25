open Lwt
open Printf

type handler = (Lwt_unix.file_descr -> unit Lwt.t)

let control_socket path handler =
  try_lwt
    let sockaddr = Lwt_unix.ADDR_UNIX path in
    Release_socket.accept_loop Lwt_unix.SOCK_STREAM sockaddr handler
  with Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
    lwt () = Lwt_log.error_f "control socket `%s' already exists" path in
    exit 1

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

module Make (O : Ops) (B : Release_buffer.S) : S
  with type request = O.request and type response = O.response =
struct
  type request = O.request
  type response = O.response
  type buffer = B.t

  module Io = Release_io.Make (B)

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

  module Master_lock = struct
    let r = Lwt_mutex.create ()
    let w = Lwt_mutex.create ()
  end

  module Slave_lock = struct
    let r = Lwt_mutex.create ()
    let w = Lwt_mutex.create ()
  end

  let read ?timeout fd =
    match_lwt Io.read ?timeout fd header_length with
    | `Timeout | `EOF as other ->
        return other
    | `Data b ->
        try Io.read ?timeout fd (read_header b)
        with Overflow -> raise_lwt (Failure "IPC header length overflow")

  let write fd buf =
    let len = B.length buf in
    let buf' = B.create (len + header_length) in
    write_header len buf';
    B.blit buf 0 buf' header_length len;
    Io.write fd buf'

  let request_of_buffer buf =
    O.request_of_string (B.to_string buf)

  let buffer_of_request req =
    B.of_string (O.string_of_request req)

  let response_of_buffer buf =
    O.response_of_string (B.to_string buf)

  let buffer_of_response resp =
    B.of_string (O.string_of_response resp)

  let with_locks locks f =
    lwt () = Lwt_list.iter_s Lwt_mutex.lock locks in
    Lwt.finalize
      f
      (fun () ->
        List.iter Lwt_mutex.unlock (List.rev locks);
        Lwt.return ())

  module Server = struct
    let read_request' ?timeout fd =
      match_lwt read ?timeout fd with
      | `Data buf -> return (`Request (request_of_buffer buf))
      | `Timeout | `EOF as other -> return other

    let read_request ?timeout fd =
      Lwt_mutex.with_lock Master_lock.r
        (fun () -> read_request' ?timeout fd)

    let write_response' fd resp =
      write fd (buffer_of_response resp)

    let write_response fd resp =
      Lwt_mutex.with_lock Master_lock.w
        (fun () -> write_response' fd resp)

    let handle_request ?timeout ?(eof_warning = true) fd handler =
      let handle_req () =
        match_lwt read_request' ?timeout fd with
        | `Timeout ->
            lwt () = Lwt_unix.close fd in
            raise_lwt (Failure "read from slave shouldn't timeout")
        | `EOF ->
            lwt () =
              if eof_warning then Lwt_log.warning "got EOF on IPC socket"
              else return_unit in
            Lwt_unix.close fd
        | `Request req ->
            try_lwt
              lwt resp = handler req in
              write_response' fd resp
            with e ->
              let err = Printexc.to_string e in
              lwt () = Lwt_log.error_f "request handler exception: %s" err in
              Lwt_unix.close fd in
      let rec req_loop () =
        lwt () = with_locks [Master_lock.r; Master_lock.w] handle_req in
        req_loop () in
      req_loop ()
  end

  module Client = struct
    let read_response' ?timeout fd =
      match_lwt read ?timeout fd with
      | `Data buf -> return (`Response (response_of_buffer buf))
      | `Timeout | `EOF as other -> return other

    let read_response ?timeout fd =
      Lwt_mutex.with_lock Slave_lock.r
        (fun () -> read_response' ?timeout fd)

    let write_request' fd req =
      write fd (buffer_of_request req)

    let write_request fd req =
      Lwt_mutex.with_lock Slave_lock.w
        (fun () -> write_request' fd req)

    let make_request ?timeout fd req handler =
      with_locks [Slave_lock.r; Slave_lock.w]
        (fun () ->
          lwt () = write_request' fd req in
          lwt res = read_response' ?timeout fd in
          handler res)
  end
end
