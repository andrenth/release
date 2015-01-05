open Core.Std
open Async.Std
open Ipc_async

let response_handler res =
  (match res with
  | `Response (ControlIpcOps.Broadcast_sent) -> printf "- bcast sent"
  | `Response r -> printf "< %s" (ControlIpcOps.string_of_response r)
  | `Timeout -> printf "@ timeout"
  | `EOF -> printf "# EOF"
  | _ -> printf "! bad response");
  return ()

let () =
  let _request_t =
    let addr = Socket.Address.Unix.create "_build/master_async.socket" in
    let sock = Socket.create Socket.Type.unix in
    Socket.connect sock addr >>= fun sock ->
    let str = "example" in
    printf "> %s" str;
    let req = ControlIpcOps.request_of_string str in
    let fd = Socket.fd sock in
    let req_t = ControlIpc.Client.make_request fd req response_handler in
    let bcast = "mybroadcast" in
    let req = ControlIpcOps.Broadcast bcast in
    let bcast_t = ControlIpc.Client.make_request fd req response_handler in
    Deferred.all_unit [req_t; bcast_t] in
  never_returns (Scheduler.go ())
