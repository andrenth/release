open Lwt
open Printf
open Ipc_lwt

open Release_lwt

let handle_sigterm _ =
  ignore_result (Lwt_log.notice "got sigterm");
  exit 0

let ipc_lock = Lwt_mutex.create ()

let ipc_request fd req =
  Lwt_mutex.with_lock ipc_lock
    (fun () -> SlaveIpc.Client.write_request fd req)

let rec consume_ipc fd =
  SlaveIpc.Client.read_response fd >>= begin function
  | `Response (SlaveIpcOps.Broadcast s) ->
      Lwt_log.notice_f "got broadcast message: %s" s
  | `Response (SlaveIpcOps.Resp1 i | SlaveIpcOps.Resp2 i) ->
      Lwt_log.notice_f "got response: %d" i
  | `Timeout | `EOF ->
      Lwt_log.error "direct IPC error" >>= fun () ->
      Lwt_unix.sleep 1.0
  end >>= fun () ->
  consume_ipc fd

let rec produce_ipc fd =
  let pid = Unix.getpid () in
  let t1 =
    Lwt_log.notice_f "t1 (%d)" pid >>= fun () ->
    Lwt_unix.sleep (float_of_int (Random.int 3)) >>= fun () ->
    ipc_request fd (SlaveIpcOps.Req1 pid) in
  let t2 =
    Lwt_log.notice_f "t2 (%d)" pid >>= fun () ->
    Lwt_unix.sleep (float_of_int (Random.int 3)) >>= fun () -> 
    ipc_request fd (SlaveIpcOps.Req2 pid) in
  Lwt.join [t1; t2] >>= fun () ->
  if false then
    Lwt_log.notice_f "exiting (%d)" (Unix.getpid ()) >>= fun () ->
    exit 0
  else
    produce_ipc fd

let check_env () =
  Lwt.catch
    (fun () ->
      let env = Sys.getenv "RELEASE" in
      Lwt_log.notice_f "environment check: RELEASE=%s" env)
    (function
    | Not_found ->
        Lwt_log.notice_f "environment RELEASE is empty"
    | e ->
        Lwt.fail e)

let main fd =
  check_env () >>= fun () ->
  ignore (Lwt_unix.on_signal Sys.sigterm handle_sigterm);
  let read_t = consume_ipc fd in
  let write_t = produce_ipc fd in
  read_t <?> write_t

let sleep _ =
  (* Lwt.join [fst (Lwt.wait ())] *)
  Lwt.join [Lwt_unix.sleep (float_of_int (Random.int 60)) >>= fun() ->
    Lwt_log.notice_f "exiting (%d)" (Unix.getpid ())]

let () =
  Lwt_log.default := Logger_lwt.syslog;
  Random.self_init ();
  Release.me ~logger:Logger_lwt.syslog ~main ()
