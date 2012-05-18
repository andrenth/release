open Lwt

type handler = (Lwt_unix.file_descr -> unit Lwt.t)

type buffer = Release_buffer.t

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

  val read : ?timeout:float
          -> Lwt_unix.file_descr
          -> [`Data of Release_buffer.t | `EOF | `Timeout] Lwt.t

  val write : Lwt_unix.file_descr -> Release_buffer.t -> unit Lwt.t

  val make_request : ?timeout:float
                  -> Lwt_unix.file_descr
                  -> request
                  -> ([`Response of response | `EOF | `Timeout] -> 'a Lwt.t)
                  -> 'a Lwt.t

  val handle_request : ?timeout:float
                    -> Lwt_unix.file_descr
                    -> (request -> response Lwt.t)
                    -> unit Lwt.t
end

module Make (O : Ops) : S
    with type request = O.request and type response = O.response =
struct
  type request = O.request
  type response = O.response

  let read ?timeout fd =
    match_lwt Release_io.read ?timeout fd 1 with
    | `Data b ->
        let siz = Release_bytes.read_byte b in
        Release_io.read ?timeout fd siz
    | other ->
        return other

  let write fd buf =
    let len = Release_buffer.length buf in
    let buf' = Release_buffer.create (len + 1) in
    Release_bytes.write_byte len buf';
    Release_buffer.blit buf 0 buf' 1 len;
    Release_io.write fd buf'

  let request_of_buffer buf =
    O.request_of_string (Release_buffer.to_string buf)

  let buffer_of_request req =
    Release_buffer.of_string (O.string_of_request req)

  let response_of_buffer buf =
    O.response_of_string (Release_buffer.to_string buf)

  let buffer_of_response resp =
    Release_buffer.of_string (O.string_of_response resp)

  let make_request ?timeout fd req handler =
    lwt () = write fd (buffer_of_request req) in
    lwt res = read ?timeout fd in
    handler
      (match res with
      | `Timeout -> `Timeout
      | `EOF -> `EOF
      | `Data resp -> `Response (response_of_buffer resp))

  let handle_request ?timeout fd handler =
    let rec handle_req () =
      match_lwt read ?timeout fd with
      | `Timeout ->
          raise_lwt (Failure "read from slave shouldn't timeout")
      | `EOF ->
          lwt () = Lwt_log.notice "got EOF on IPC socket" in
          Lwt_unix.close fd
      | `Data req ->
          let _resp_t =
            try_lwt
              let s = Release_buffer.to_string req in
              lwt resp = handler (O.request_of_string s) in
              write fd (buffer_of_response resp)
            with e ->
              let err = Printexc.to_string e in
              lwt () = Lwt_log.error_f "request handler exception: %s" err in
              Lwt_unix.close fd in
          handle_req () in
    handle_req ()
end
