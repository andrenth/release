module Std_unix = Unix
open Core
open Async
open Ipc

open Release_async

let logger = Logger.syslog

let socket_path =
  sprintf "%s/_build/helper_async.socket" (Std_unix.getcwd ())

let handle_sigterm _ =
  Log.info logger "got sigterm, exiting";
  don't_wait_for (exit 0)

let control_handler fd =
  let handler req =
    let s = ControlIpcOps.string_of_request req in
    Log.info logger "got control request: %s" s;
    return (ControlIpcOps.response_of_string (String.uppercase s)) in
  ControlIpc.Server.handle_request ~timeout:5. fd handler >>= fun () ->
  return ()

let sleep sec =
  Clock.after (Time.Span.of_sec sec) >>| fun _ -> ()

let main fd =
  let ctrl_ipc_t =
    Release.IPC.control_socket socket_path control_handler in
  let bcast_ipc_t =
    let rec bcast_ipc () =
      SlaveIpc.Client.read_response fd >>= function
      | `Response (SlaveIpcOps.Broadcast s) ->
          Log.info logger "got broadcast: %s" s;
          return ()
      | `Response (SlaveIpcOps.Resp1 i | SlaveIpcOps.Resp2 i) ->
          Log.info logger "got unexpected IPC response: %d" i;
          return ()
      | _ ->
          Log.error logger "helper IPC error";
          sleep 1.0 >>= fun () ->
      bcast_ipc () in
    bcast_ipc () in
    Deferred.all_unit [ctrl_ipc_t; bcast_ipc_t] >>= fun () ->
    exit 0

let () =
  Signal.handle [Signal.term] handle_sigterm;
  Random.self_init ();
  Release.me ~logger ~main ()
