open Lwt
open Printf
open Ipc

let handle_sigterm _ =
  ignore_result (Lwt_log.notice "got sigterm");
  exit 0

let ipc_request =
  let mtx = Lwt_mutex.create () in
  fun fd req ->
    let response_handler = function
      | `Timeout ->
          Lwt_log.notice "timeout reading from master"
      | `EOF ->
          Lwt_log.notice "unexpected EOF from master"
      | `Response r ->
          Lwt_log.notice_f "response: %s" (IpcOps.string_of_response r) in
    lwt () = Lwt_mutex.lock mtx in
    lwt () = Ipc.make_request ~timeout:5. fd req response_handler in
    Lwt_mutex.unlock mtx;
    return ()

let main fd =
  ignore (Lwt_unix.on_signal Sys.sigterm handle_sigterm);
  let pid = Unix.getpid () in
  let t1 =
    lwt () = Lwt_log.notice_f "t1 (%d)" pid in
    lwt () = Lwt_unix.sleep (float_of_int (Random.int 3)) in
    ipc_request fd (IpcOps.Req1 pid) in
  let t2 =
    lwt () = Lwt_log.notice_f "t2 (%d)" pid in
    lwt () = Lwt_unix.sleep (float_of_int (Random.int 3)) in
    ipc_request fd (IpcOps.Req2 pid) in
  lwt () = Lwt.join [t1; t2] in
  lwt () = Lwt_log.notice_f "exiting (%d)" (Unix.getpid ()) in
  exit 0

let sleep _ =
  (* Lwt.join [fst (Lwt.wait ())] *)
  Lwt.join [Lwt_unix.sleep (float_of_int (Random.int 60)) >>= fun() ->
    Lwt_log.notice_f "exiting (%d)" (Unix.getpid ())]

let () =
  Random.self_init ();
  Release.slave ~syslog:false ~user:"andre" ~main:main ()
