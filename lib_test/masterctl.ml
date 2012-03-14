open Lwt
open Printf
open Ipc

let response_handler res =
  match res with
  | `Response r -> Lwt_io.printlf "< %s" (ControlIpcOps.string_of_response r)
  | _ -> Lwt_io.printl "! bad response"

let () =
  if Unix.geteuid () <> 0 then begin
    prerr_endline "masterctl must be run as root";
    exit 1
  end;
  let request_t =
    let sock = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0 in
    let sock_addr = Lwt_unix.ADDR_UNIX "/tmp/master.socket" in
    lwt () = Lwt_unix.connect sock sock_addr in
    let str = "example" in
    lwt () = Lwt_io.printlf "> %s" str in
    let req = ControlIpcOps.request_of_string str in
    ControlIpc.make_request sock req response_handler in
  Lwt_main.run request_t
