open Lwt
open Printf
open Ipc

let response_handler res =
  match res with
  | `Response (ControlIpcOps.Broadcast_sent) -> Lwt_io.printlf "- bcast sent"
  | `Response r -> Lwt_io.printlf "< %s" (ControlIpcOps.string_of_response r)
  | _ -> Lwt_io.printl "! bad response"

let () =
  let request_t =
    let sock = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let sock_addr = Lwt_unix.ADDR_UNIX "_build/master.socket" in
    lwt () = Lwt_unix.connect sock sock_addr in
    let str = "example" in
    lwt () = Lwt_io.printlf "> %s" str in
    let req = ControlIpcOps.request_of_string str in
    let req_t = ControlIpc.Client.make_request sock req response_handler in
    let bcast = "mybroadcast" in
    let req = ControlIpcOps.Broadcast bcast in
    let bcast_t = ControlIpc.Client.make_request sock req response_handler in
    req_t <&> bcast_t in
  Lwt_main.run request_t
