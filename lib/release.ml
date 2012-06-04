open Lwt
open Printf

let fork () =
  lwt () = Lwt_io.flush_all () in
  match Lwt_unix.fork () with
  | 0 ->
      return 0
  | pid ->
      Lwt_sequence.iter_node_l Lwt_sequence.remove Lwt_main.exit_hooks;
      return pid

let daemon f =
  let grandchild () =
    lwt () = Lwt_unix.chdir "/" in
    lwt dev_null = Lwt_unix.openfile "/dev/null" [Lwt_unix.O_RDWR] 0 in
    let close_and_dup fd =
      lwt () = Lwt_unix.close fd in
      Lwt_unix.dup2 dev_null fd;
      return () in
    let descrs = [Lwt_unix.stdin; Lwt_unix.stdout; Lwt_unix.stderr] in 
    lwt () = Lwt_list.iter_p close_and_dup descrs in
    lwt () = Lwt_unix.close dev_null in
    f () in
  let child () =
    ignore (Unix.setsid ());
    Sys.set_signal Sys.sighup Sys.Signal_ignore;
    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
    match_lwt fork () with
    | 0 -> grandchild ()
    | _ -> exit 0 in
  ignore (Unix.umask 0);
  match_lwt fork () with
  | 0 -> child ()
  | _ -> exit 0

let read_lock_file path =
  try_lwt
    Lwt_io.with_file Lwt_io.input path (fun ch ->
      match_lwt Lwt_io.read_line_opt ch with
      | None ->
          return None
      | Some l ->
          try_lwt
            return (Some (int_of_string l))
          with _ ->
            return None)
  with
  | Unix.Unix_error (Unix.ENOENT, _, _) ->
      return None
  | Unix.Unix_error (e, _, _) ->
      let err = Unix.error_message e in
      lwt () = Lwt_log.error_f "cannot read lock file: %s: %s" path err in
      return None

let write_pid path =
  try_lwt
    Lwt_io.with_file Lwt_io.output path (fun ch ->
      Lwt_io.fprintf ch "%d\n" (Unix.getpid ()))
  with Unix.Unix_error (e, _, _) ->
    let err = Unix.error_message e in
    Lwt_log.error_f "cannot create lock file %s: %s" path err

let create_lock_file path =
  match_lwt read_lock_file path with
  | None ->
      write_pid path
  | Some pid ->
      try_lwt
        Unix.kill pid 0;
        Lwt_log.error_f "there is a running instance of %s already: %d"
          Sys.argv.(0) pid
      with Unix.Unix_error (Unix.ESRCH, _, _) ->
        lwt () =
          Lwt_log.warning_f "found a stale pid in lock file %s: %d" path pid in
        write_pid path

let remove_lock_file path =
  match_lwt read_lock_file path with
  | None ->
      Lwt_log.warning_f "couldn't find lock file %s" path
  | Some pid ->
      let mypid = Unix.getpid () in
      if pid = mypid then
        Lwt_unix.unlink path
      else
        Lwt_log.warning_f
          "pid mismatch in lock file: found %d, mine is %d; not removing"
          pid mypid

let remove_control_socket (path, _) =
  Lwt_unix.unlink path

let check_user test msg =
  if not (test (Unix.getuid ())) then
    lwt () = Lwt_log.error_f "%s %s" Sys.argv.(0) msg in
    exit 1
  else
    return ()

let check_root () =
  check_user ((=) 0) "must be run by root"

let check_nonroot () =
  check_user ((<>) 0) "cannot be run as root"

let try_exec run path =
  let can_exec st =
    let kind = st.Lwt_unix.st_kind in
    let perm = st.Lwt_unix.st_perm in
    kind = Lwt_unix.S_REG && perm land 0o100 <> 0 in
  try_lwt
    lwt st = Lwt_unix.lstat path in
    if can_exec st then
      run path
    else
      lwt () = Lwt_log.error_f "cannot execute `%s'" path in
      exit 126
  with Unix.Unix_error (e, _, _) ->
    let err = Unix.error_message e in
    lwt () = Lwt_log.error_f "cannot stat `%s': %s" path err in
    exit 126

let posix_signals =
  [| "SIGABRT"; "SIGALRM"; "SIGFPE"; "SIGHUP"; "SIGILL"; "SIGINT";
     "SIGKILL"; "SIGPIPE"; "SIGQUIT"; "SIGSEGV"; "SIGTERM"; "SIGUSR1";
     "SIGUSR2"; "SIGCHLD"; "SIGCONT"; "SIGSTOP"; "SIGTSTP"; "SIGTTIN";
     "SIGTTOU"; "SIGVTALRM"; "SIGPROF" |]

let signame s =
  let i = -s - 1 in
  if i >= 0 && i < Array.length posix_signals then
    posix_signals.(i)
  else
    string_of_int s

module ConnSet = Set.Make(struct
  type t = Lwt_unix.file_descr
  let compare = compare
end)

let slave_connection_set = ref ConnSet.empty
let slave_connection_set_mtx = Lwt_mutex.create ()

let add_slave_connection fd =
  lwt () = Lwt_mutex.lock slave_connection_set_mtx in
  slave_connection_set := ConnSet.add fd !slave_connection_set;
  Lwt_mutex.unlock slave_connection_set_mtx;
  return ()

let remove_slave_connection fd =
  lwt () = Lwt_mutex.lock slave_connection_set_mtx in
  slave_connection_set := ConnSet.remove fd !slave_connection_set;
  Lwt_mutex.unlock slave_connection_set_mtx;
  return ()

let slave_connections () =
  ConnSet.elements !slave_connection_set

let handle_process master_fd reexec proc =
  let log fmt = ksprintf (fun s -> Lwt_log.notice_f "process %s" s) fmt in
  let pid = proc#pid in
  lwt () = log "creating child process %d" pid in
  lwt () = match_lwt proc#status with
  | Lwt_unix.WEXITED 0 -> log "%d exited normally" pid
  | Lwt_unix.WEXITED s -> log "%d exited with status %s" pid (signame s)
  | Lwt_unix.WSIGNALED s -> log "%d signaled to death by %s" pid (signame s)
  | Lwt_unix.WSTOPPED s -> log "%d stopped by signal %s" pid (signame s) in
  lwt () = remove_slave_connection master_fd in
  reexec ()

let setup_ipc ipc_handler =
  let master_fd, slave_fd =
    Lwt_unix.socketpair Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.set_close_on_exec master_fd;
  let _ipc_t = ipc_handler master_fd in
  master_fd, slave_fd

let getenv k =
  try
    let v = Sys.getenv k in
    Some (sprintf "%s=%s" k v)
  with Not_found ->
    None

let restrict_env () =
  let allowed = ["TZ"] in
  let setenv env k =
    Option.may_default env (fun v -> v::env) (getenv k) in
  Array.of_list (List.fold_left setenv ["PATH=/bin:/usr/bin"] allowed)

let rec exec_process path ipc_handler check_death_rate =
  lwt () = check_death_rate () in
  let master_fd, slave_fd = setup_ipc ipc_handler in
  lwt () = add_slave_connection master_fd in
  let run_proc path =
    let reexec () =
      exec_process path ipc_handler check_death_rate in
    Lwt_process.with_process_none
      ~stdin:(`FD_move (Lwt_unix.unix_file_descr slave_fd))
      ~env:(restrict_env ())
      (path, [| path |])
      (handle_process master_fd reexec) in
  let _slave_t =
    try_exec run_proc path in
  return ()

let num_exec_tries = 10

let check_death_rate time tries () =
  let now = Unix.time () in
  if now -. !time > 1. then begin
    tries := num_exec_tries;
    time := now
  end;
  match !tries with
  | 0 ->
      lwt () = Lwt_log.error "slave process dying too fast" in
      exit 1
  | _ ->
      decr tries;
      return ()

let init_exec_slave max_tries =
  let tries = ref max_tries in
  let time = ref 0. in
  fun path ipc_handler ->
    exec_process path ipc_handler (check_death_rate time tries)

let handle_sigterm lock_file control _ =
  let log_t =
    Lwt_log.notice "got sigterm, exiting" in
  let ctrl_t =
    Option.either return remove_control_socket control in
  let lock_t =
    remove_lock_file lock_file in
  Sys.set_signal Sys.sigterm Sys.Signal_ignore;
  Unix.kill 0 15;
  Lwt_main.run (log_t >> (ctrl_t <&> lock_t));
  exit 143


let curry f (x, y) = f x y

let master_slaves ?(background = true) ?(syslog = true) ?(privileged = true)
                  ?control ?main ~lock_file ~slaves () =
  if syslog then Lwt_log.default := Lwt_log.syslog ~facility:`Daemon ();
  let create_slaves (path, ipc_handler, n) =
    for_lwt i = 1 to n do
      let exec_slave = init_exec_slave num_exec_tries in
      exec_slave path ipc_handler
    done in
  let work () =
    ignore (Lwt_unix.on_signal Sys.sigterm (handle_sigterm lock_file control));
    lwt () = create_lock_file lock_file in
    let idle_t, _idle_w = Lwt.wait () in
    let control_t =
      Option.either return (curry Release_ipc.control_socket) control in
    lwt () = Lwt_list.iter_p create_slaves slaves in
    let main_t =
      Option.either return (fun f -> f slave_connections) main in
    main_t <&> control_t <&> idle_t in
  let main_t =
    lwt () = if privileged then check_root () else check_nonroot () in
    if background then daemon work else work () in
  Lwt_main.run main_t

let master_slave ~slave =
  let (path, ipc_handler) = slave in
  master_slaves ~slaves:[path, ipc_handler, 1]

let lose_privileges user =
  lwt () = check_root () in
  try_lwt
    Release_privileges.drop user
  with Release_privileges.Release_privileges_error err ->
    lwt () = Lwt_log.error err in
    exit 1

let me ?(syslog = true) ?user ~main () =
  if syslog then Lwt_log.default := Lwt_log.syslog ~facility:`Daemon ();
  let main_t =
    lwt () = Option.either check_nonroot lose_privileges user in
    let ipc_fd = Lwt_unix.dup Lwt_unix.stdin in
    lwt () = Lwt_unix.close Lwt_unix.stdin in
    lwt () = Lwt_log.info "starting up" in
    lwt () = main ipc_fd in
    lwt () = Lwt_log.info "stopping" in
    return () in
  Lwt_main.run main_t
