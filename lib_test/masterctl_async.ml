open Core.Std
open Async.Std
open Ipc_async
open Release_async

let response_handler res =
  (match res with
  | `Response (ControlIpcOps.Broadcast_sent) -> printf "- bcast sent\n%!"
  | `Response r -> printf "< %s\n%!" (ControlIpcOps.string_of_response r)
  | `Timeout -> printf "@ timeout\n%!"
  | `EOF -> printf "# EOF\n%!"
  | _ -> printf "! bad response\n%!");
  return ()

let () =
  let _request_t =
    let addr = Socket.Address.Unix.create "_build/master_async.socket" in
    let sock = Socket.create Socket.Type.unix in
    Socket.connect sock addr >>= fun sock ->
    let str = "example" in
    printf "> %s\n%!" str;
    let conn = Release.IPC.create_connection sock in
    let req = ControlIpcOps.request_of_string str in
    let req_t = ControlIpc.Client.make_request conn req response_handler in
    let req = ControlIpcOps.Broadcast "mybroadcast" in
    let bcast_t = ControlIpc.Client.make_request conn req response_handler in
    Deferred.all_unit [req_t; bcast_t] >>= fun () ->
    exit 0 in
  never_returns (Scheduler.go ())
