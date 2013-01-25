open Lwt
open Printf
open Ipc

let handle_sigterm _ =
  ignore_result (Lwt_log.notice "got sigterm");
  exit 0

let ipc_request fd req =
  SlaveIpc.Client.write_request fd req

let rec consume_ipc fd =
  lwt () = match_lwt SlaveIpc.Client.read_response fd with
  | `Response (SlaveIpcOps.Broadcast s) ->
      Lwt_log.notice_f "got broadcast message: %s" s
  | `Response (SlaveIpcOps.Resp1 i | SlaveIpcOps.Resp2 i) ->
      Lwt_log.notice_f "got response: %d" i
  | `Timeout | `EOF ->
      Lwt_log.error "direct IPC error" in
  consume_ipc fd

let rec produce_ipc fd =
  ignore (Lwt_unix.on_signal Sys.sigterm handle_sigterm);
  let pid = Unix.getpid () in
  let t1 =
    lwt () = Lwt_log.notice_f "t1 (%d)" pid in
    lwt () = Lwt_unix.sleep (float_of_int (Random.int 3)) in
    ipc_request fd (SlaveIpcOps.Req1 pid) in
  let t2 =
    lwt () = Lwt_log.notice_f "t2 (%d)" pid in
    lwt () = Lwt_unix.sleep (float_of_int (Random.int 3)) in
    ipc_request fd (SlaveIpcOps.Req2 pid) in
  lwt () = Lwt.join [t1; t2] in
  if Random.float 1.0 < 0.5 then
    lwt () = Lwt_log.notice_f "exiting (%d)" (Unix.getpid ()) in
    exit 0
  else
    produce_ipc fd

let main fd =
  let read_t = consume_ipc fd in
  let write_t = produce_ipc fd in
  read_t <?> write_t

let sleep _ =
  (* Lwt.join [fst (Lwt.wait ())] *)
  Lwt.join [Lwt_unix.sleep (float_of_int (Random.int 60)) >>= fun() ->
    Lwt_log.notice_f "exiting (%d)" (Unix.getpid ())]

let () =
  Random.self_init ();
  Release.me ~syslog:false ~user:"andre" ~main:main ()
