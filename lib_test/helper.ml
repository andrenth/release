open Lwt
open Printf
open Ipc

let handle_sigterm _ =
  ignore_result (Lwt_log.notice "got sigterm");
  exit 0

let control_handler fd =
  let handler req =
    let s = ControlIpcOps.string_of_request req in
    lwt () = Lwt_log.notice_f "got control request: %s" s in
    return (ControlIpcOps.response_of_string (String.uppercase s)) in
  ControlIpc.handle_request ~timeout:5. fd handler

let main fd =
  let ipc_t =
    Release_ipc.setup_control_socket "/helper.socket" control_handler in
  lwt () = Lwt.join [ipc_t] in
  exit 0

let () =
  Random.self_init ();
  Release.me ~syslog:false ~user:"andre" ~main:main ()
