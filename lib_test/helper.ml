open Lwt
open Printf
open Ipc

let handle_sigterm _ =
  let log_t =
    Lwt_log.notice "got sigterm, exiting" in
  let ctrl_t =
    Lwt_unix.unlink "/helper.socket" in
  Lwt_main.run (log_t >> ctrl_t);
  exit 0

let control_handler fd =
  let handler req =
    let s = ControlIpcOps.string_of_request req in
    lwt () = Lwt_log.notice_f "got control request: %s" s in
    return (ControlIpcOps.response_of_string (String.uppercase s)) in
  lwt () = ControlIpc.handle_request ~timeout:5. fd handler in
  return ()

let main fd =
  let ipc_t =
    Release_ipc.control_socket "/helper.socket" control_handler in
  lwt () = Lwt.join [ipc_t] in
  exit 0

let () =
  ignore (Lwt_unix.on_signal Sys.sigterm handle_sigterm);
  Random.self_init ();
  Release.me ~syslog:false ~user:"andre" ~main:main ()
