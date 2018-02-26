open Lwt
open Printf
open Ipc
open Release_lwt

let response_handler res =
  match res with
  | `Response (ControlIpcOps.Broadcast_sent) -> Lwt_io.printlf "- bcast sent"
  | `Response r -> Lwt_io.printlf "< %s" (ControlIpcOps.string_of_response r)
  | `Timeout -> Lwt_io.printl "@ timeout"
  | `EOF -> Lwt_io.printl "# EOF"
  | _ -> Lwt_io.printl "! bad response"

let () =
  let request_t =
    let sock = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let path = sprintf "%s/_build/master_lwt.socket" (Unix.getcwd ()) in
    let sock_addr = Lwt_unix.ADDR_UNIX path in
    Lwt_unix.connect sock sock_addr >>= fun () ->
    let str = "example" in
    Lwt_io.printlf "> %s" str >>= fun () ->
    let conn = Release.IPC.create_connection sock in
    let req = ControlIpcOps.request_of_string str in
    let req_t = ControlIpc.Client.make_request conn req response_handler in
    let bcast = "mybroadcast" in
    let req = ControlIpcOps.Broadcast bcast in
    let bcast_t = ControlIpc.Client.make_request conn req response_handler in
    req_t <&> bcast_t in
  Lwt_main.run request_t
