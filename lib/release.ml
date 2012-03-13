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

let handle_proc_death reexec proc =
  let log = Lwt_log.notice_f in
  let pid = proc#pid in
  lwt () = log "creating child process %d" pid in
  lwt () = match_lwt proc#status with
  | Lwt_unix.WEXITED 0 -> log "process %d exited normally" pid
  | Lwt_unix.WEXITED s -> log "process %d exited with status %d" pid s
  | Lwt_unix.WSIGNALED s -> log "process %d signaled to death by %d" pid s
  | Lwt_unix.WSTOPPED s -> log "process %d stopped by signal %d" pid s in
  reexec ()

let rec exec_process path ipc_handler check_death_rate =
  lwt () = check_death_rate () in
  let master_fd, slave_fd =
    Lwt_unix.socketpair Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.set_close_on_exec master_fd;
  let _ipc_t = ipc_handler master_fd in
  let run_proc path =
    let fd = Lwt_unix.unix_file_descr slave_fd in
    let reexec () =
      exec_process path ipc_handler check_death_rate in
    Lwt_process.with_process_none
      ~stdin:(`FD_move fd)
      (path, [| path |])
      (handle_proc_death reexec) in
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

let handle_termination signal _ =
  ignore_result (Lwt_log.notice_f "got %s, exiting" signal);
  Unix.kill 0 15;
  exit 143

let handle_sigterm = handle_termination "sigterm"
let handle_sigint = handle_termination "sigint"

let handle_control_connections (sock_path, handler) =
  try_lwt
    let sock = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
    Lwt_unix.setsockopt sock Lwt_unix.SO_REUSEADDR true;
    let sock_addr = Lwt_unix.ADDR_UNIX sock_path in
    Lwt_unix.bind sock sock_addr;
    Lwt_unix.listen sock 10;
    let rec accept () =
      lwt cli_sock, _ = Lwt_unix.accept sock in
      let timeout_t =
        lwt () = Lwt_unix.sleep 10.0 in
        lwt () = Lwt_log.warning_f "timeout on control socket" in
        Lwt_unix.close cli_sock in
      let handler_t =
        lwt () = handler cli_sock in
        Lwt_unix.close cli_sock in
      ignore (Lwt.pick [handler_t; timeout_t]);
      accept () in
    accept ()
  with Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
    lwt () = Lwt_log.error_f "control socket %s already exists" sock_path in
    exit 1

let master_slaves ?(background = true) ?(syslog = true) ?(privileged = true)
                  ?control ~num_slaves ~lock_file ~slave_ipc_handler ~exec () =
  if syslog then Lwt_log.default := Lwt_log.syslog ~facility:`Daemon ();
  let create_procs () =
    for_lwt i = 1 to num_slaves do
      let exec_slave = init_exec_slave num_exec_tries in
      exec_slave exec slave_ipc_handler
    done in
  let work () =
    ignore (Lwt_unix.on_signal Sys.sigterm handle_sigterm);
    ignore (Lwt_unix.on_signal Sys.sigint handle_sigint);
    lwt () = create_lock_file lock_file in
    Lwt_main.at_exit (fun () ->
      lwt () = Option.either return remove_control_socket control in
      lwt () = remove_lock_file lock_file in
      return ());
    let idle_t, idle_w = Lwt.wait () in
    let control_t =
      Option.either return handle_control_connections control in
    lwt () = create_procs () in
    control_t <&> idle_t in
  let main_t =
    lwt () = if privileged then check_root () else check_nonroot () in
    if background then daemon work else work () in
  Lwt_main.run main_t

let master_slave = master_slaves ~num_slaves:1

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
