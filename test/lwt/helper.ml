open Lwt
open Printf
open Ipc

open Release_lwt

let socket_path =
  sprintf "%s/_build/helper_lwt.socket" (Unix.getcwd ())

let handle_sigterm _ =
  let log_t =
    Lwt_log.notice "got sigterm, exiting" in
  Lwt_main.run log_t;
  exit 0

let control_handler fd =
  let handler req =
    let s = ControlIpcOps.string_of_request req in
    Lwt_log.notice_f "got control request: %s" s >>= fun () ->
    return (ControlIpcOps.response_of_string (String.uppercase_ascii s)) in
  ControlIpc.Server.handle_request ~timeout:5. fd handler >>= fun () ->
  return ()

let main fd =
  let ctrl_ipc_t =
    Release.IPC.control_socket socket_path control_handler in
  let bcast_ipc_t =
    let rec bcast_ipc () =
      SlaveIpc.Client.read_response fd >>= function
      | `Response (SlaveIpcOps.Broadcast s) ->
          Lwt_log.notice_f "got broadcast: %s" s
      | `Response (SlaveIpcOps.Resp1 i | SlaveIpcOps.Resp2 i) ->
          Lwt_log.notice_f "got unexpected IPC response: %d" i
      | _ ->
          Lwt_log.error "helper IPC error" >>= fun () ->
          Lwt_unix.sleep 1.0 >>= fun () ->
      bcast_ipc () in
    bcast_ipc () in
  ctrl_ipc_t <&> bcast_ipc_t

let () =
  Lwt_log.default := Logger.syslog;
  ignore (Lwt_unix.on_signal Sys.sigterm handle_sigterm);
  Random.self_init ();
  Release.me ~logger:Logger.syslog ~main:main ()
