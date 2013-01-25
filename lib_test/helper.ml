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
  lwt () = ControlIpc.Server.handle_request ~timeout:5. fd handler in
  return ()

let main fd =
  let ctrl_ipc_t =
    Release_ipc.control_socket "/helper.socket" control_handler in
  let bcast_ipc_t =
    let rec bcast_ipc () =
      lwt () = match_lwt SlaveIpc.Client.read_response fd with
      | `Response (SlaveIpcOps.Broadcast s) ->
          Lwt_log.notice_f "got broadcast: %s" s
      | _ ->
          Lwt_log.error "helper IPC error" in
      bcast_ipc () in
    bcast_ipc () in
  ctrl_ipc_t <&> bcast_ipc_t

let () =
  ignore (Lwt_unix.on_signal Sys.sigterm handle_sigterm);
  Random.self_init ();
  Release.me ~syslog:false ~user:"andre" ~main:main ()
