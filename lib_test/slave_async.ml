open Core.Std
open Async.Std
open Ipc_async

open Release_async

let logger = Logger_async.syslog

let handle_sigterm _ =
  Log.info logger "got sigterm";
  Pervasives.exit 0

let ipc_lock = Sequencer.create ()

let ipc_request fd req =
  Throttle.enqueue ipc_lock
    (fun () -> SlaveIpc.Client.write_request fd req)

let sleep sec =
  Clock.after (Time.Span.of_sec sec) >>| fun _ -> ()

let rec consume_ipc fd =
  SlaveIpc.Client.read_response fd >>= begin function
  | `Response (SlaveIpcOps.Broadcast s) ->
      Log.info logger "got broadcast message: %s" s;
      return ()
  | `Response (SlaveIpcOps.Resp1 i | SlaveIpcOps.Resp2 i) ->
      Log.info logger "got response: %d" i;
      return ()
  | `Timeout | `EOF ->
      Log.info logger "direct IPC error";
      sleep 1.0
  end >>= fun () ->
  consume_ipc fd

let rec produce_ipc fd =
  let pid = Pid.to_int (Unix.getpid ()) in
  let t1 =
    Log.info logger "t1 (%d)" pid;
    sleep (Float.of_int (Random.int 3)) >>= fun () ->
    ipc_request fd (SlaveIpcOps.Req1 pid) in
  let t2 =
    Log.info logger "t2 (%d)" pid;
    sleep (Float.of_int (Random.int 3)) >>= fun () ->
    ipc_request fd (SlaveIpcOps.Req2 pid) in
  Deferred.all_unit [t1; t2] >>= fun () ->
  if false then begin
    Log.info logger "exiting (%d)" (Pid.to_int (Unix.getpid ()));
    exit 0
  end else
    produce_ipc fd

let check_env () =
  match Sys.getenv "RELEASE" with
  | Some env -> Log.info logger "environment check: RELEASE=%s" env
  | None -> Log.info logger "environment RELEASE is empty"

let main fd =
  check_env ();
  Signal.handle [Signal.term] handle_sigterm;
  let read_t = consume_ipc fd in
  let write_t = produce_ipc fd in
  Deferred.any [read_t; write_t] >>= fun () ->
  exit 0

let () =
  Random.self_init ();
  Release.me ~logger ~main ()
