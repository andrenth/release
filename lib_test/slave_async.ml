open Core.Std
open Async.Std
open Ipc_async

open Release_async

module Log = Future.Logger

let handle_sigterm _ =
  Thread_safe.run_in_async_wait_exn (fun () -> Log.info "got sigterm");
  Pervasives.exit 0

let ipc_lock = Mutex.create ()

let with_lock m f =
  Mutex.lock m;
  Monitor.protect f ~finally:(fun () -> Mutex.unlock m; return ())

let ipc_request fd req =
  with_lock ipc_lock
    (fun () -> SlaveIpc.Client.write_request fd req)

let sleep sec =
  Clock.after (Time.Span.of_sec sec) >>| fun _ -> ()

let rec consume_ipc fd =
  SlaveIpc.Client.read_response fd >>= begin function
  | `Response (SlaveIpcOps.Broadcast s) ->
      Log.info_f "got broadcast message: %s" s
  | `Response (SlaveIpcOps.Resp1 i | SlaveIpcOps.Resp2 i) ->
      Log.info_f "got response: %d" i
  | `Timeout | `EOF ->
      Log.info "direct IPC error" >>= fun () ->
      sleep 1.0
  end >>= fun () ->
  consume_ipc fd

let rec produce_ipc fd =
  let pid = Pid.to_int (Unix.getpid ()) in
  let t1 =
    Log.info_f "t1 (%d)" pid >>= fun () ->
    sleep (Float.of_int (Random.int 3)) >>= fun () ->
    ipc_request fd (SlaveIpcOps.Req1 pid) in
  let t2 =
    Log.info_f "t2 (%d)" pid >>= fun () ->
    sleep (Float.of_int (Random.int 3)) >>= fun () ->
    ipc_request fd (SlaveIpcOps.Req2 pid) in
  Deferred.all_unit [t1; t2] >>= fun () ->
  if false then
    Log.info_f "exiting (%d)" (Pid.to_int (Unix.getpid ())) >>= fun () ->
    exit 0
  else
    produce_ipc fd

let check_env () =
  match Sys.getenv "RELEASE" with
  | Some env -> Log.info_f "environment check: RELEASE=%s" env
  | None -> Log.info "environment RELEASE is empty"

let main fd =
  check_env () >>= fun () ->
  Signal.handle [Signal.term] handle_sigterm;
  let read_t = consume_ipc fd in
  let write_t = produce_ipc fd in
  Deferred.any [read_t; write_t]

let sleep _ =
  Deferred.all_unit [sleep (Float.of_int (Random.int 60)) >>= fun () ->
  Log.info_f "exiting (%d)" (Pid.to_int (Unix.getpid ()))]

let () =
  Random.self_init ();
  Release.me ~syslog:false ~main ()
