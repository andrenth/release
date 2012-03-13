open Lwt

type handler = (Lwt_unix.file_descr -> unit Lwt.t)

let setup_control_socket path handler =
  try_lwt
    let sock = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
    Lwt_unix.setsockopt sock Lwt_unix.SO_REUSEADDR true;
    let sock_addr = Lwt_unix.ADDR_UNIX path in
    Lwt_unix.bind sock sock_addr;
    Lwt_unix.listen sock 10;
    let rec accept () =
      lwt cli_sock, _ = Lwt_unix.accept sock in
      let timeout_t =
        lwt () = Lwt_unix.sleep 10.0 in
        lwt () = Lwt_log.warning_f "timeout on control socket" in
        Lwt_unix.close cli_sock in
      let handler_t =
        lwt () = handler cli_sock in
        Lwt_unix.close cli_sock in
      ignore (Lwt.pick [handler_t; timeout_t]);
      accept () in
    accept ()
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
          -> [`Data of string | `EOF | `Timeout] Lwt.t

  val write : Lwt_unix.file_descr -> string -> unit Lwt.t

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

  let write fd s =
    let siz = String.length s in
    let buf = Buffer.create siz in
    Release_bytes.write_byte siz buf;
    Buffer.add_string buf s;
    Release_io.write fd (Buffer.contents buf)

  let make_request ?timeout fd req handler =
    lwt () = write fd (O.string_of_request req) in
    lwt res = read ?timeout fd in
    handler
      (match res with
      | `Timeout -> `Timeout
      | `EOF -> `EOF
      | `Data resp -> `Response (O.response_of_string resp))

  let handle_request ?timeout fd handler =
    match_lwt read ?timeout fd with
    | `Timeout ->
        raise_lwt (Failure "read from slave shouldn't timeout")
    | `EOF ->
        lwt () = Lwt_log.notice "got EOF on IPC socket" in
        Lwt_unix.close fd
    | `Data req ->
        lwt resp = handler (O.request_of_string req) in
        write fd (O.string_of_response resp)
end
