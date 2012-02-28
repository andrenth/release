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
  lwt st = Lwt_unix.lstat path in
  if can_exec st then
    run path
  else
    lwt () = Lwt_log.error_f "cannot execute `%s'" path in
    exit 126

let init_slave_death_rate () =
  let num_exec_tries = 10 in
  let tries = ref num_exec_tries in
  let time = ref 0. in
  fun () ->
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

let exec_slave =
  let check_slave_death_rate = init_slave_death_rate () in
  let rec exec_slave path ipc_handler =
    lwt () = check_slave_death_rate () in
    let master_fd, slave_fd =
      Lwt_unix.socketpair Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
    Lwt_unix.set_close_on_exec master_fd;
    let _ipc_t = ipc_handler master_fd in
    let proc_handler proc =
      let log = Lwt_log.notice_f in
      let pid = proc#pid in
      lwt () = log "creating child process %d" pid in
      lwt () = match_lwt proc#status with
      | Lwt_unix.WEXITED 0 -> log "process %d exited normally" pid
      | Lwt_unix.WEXITED s -> log "process %d exited with status %d" pid s
      | Lwt_unix.WSIGNALED s -> log "process %d signaled to death by %d" pid s
      | Lwt_unix.WSTOPPED s -> log "process %d stopped by signal %d" pid s in
      exec_slave path ipc_handler in
    let run_slave path =
      let fd = Lwt_unix.unix_file_descr slave_fd in
      let cmd = (path, [| path |]) in
      Lwt_process.with_process_none ~stdin:(`FD_move fd) cmd proc_handler in
    let _slave_t =
      try_exec run_slave path in
    return () in
  exec_slave

let handle_sigterm _ =
  ignore (Lwt_unix.on_signal Sys.sigterm ignore);
  ignore_result (Lwt_log.notice "got sigterm");
  Unix.kill 0 15;
  exit 143

let master_slaves ?(background = true) ?(syslog = true) ?(privileged = true)
                  ~num_slaves ~lock_file ~ipc_handler ~exec () =
  if syslog then Lwt_log.default := Lwt_log.syslog ~facility:`Daemon ();
  let create_slaves () =
    for_lwt i = 1 to num_slaves do
      exec_slave exec ipc_handler
    done in
  let work () =
    ignore (Lwt_unix.on_signal Sys.sigterm handle_sigterm);
    lwt () = create_lock_file lock_file in
    Lwt_main.at_exit (fun () -> remove_lock_file lock_file);
    let idle_t, idle_w = Lwt.wait () in
    lwt () = create_slaves () in
    idle_t in
  let main_t =
    lwt () = if privileged then check_root () else check_nonroot () in
    if background then Release.daemon work else work () in
  Lwt_main.run main_t

let master_slave = master_slaves ~num_slaves:1

let lose_privileges user =
  lwt () = check_root () in
  try_lwt
    Release_privileges.drop user
  with Release_privileges.Release_privileges_error err ->
    lwt () = Lwt_log.error err in
    exit 1

let me ?(syslog = true) ?user ~main:main () =
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
