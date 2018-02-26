open Printf
open Stdint
open Util

type command = string * string array

module Buffer = Release_buffer
module Bytes = Release_bytes
module Config = Config
module Socket = Socket
module Io = Io
module Ipc = Ipc
module Util = Util
module Option = Util.Option

open Lwt.Infix

let fork () =
  Lwt_io.flush_all () >>= fun () ->
  match Lwt_unix.fork () with
  | 0 ->
      Lwt.return 0
  | pid ->
      (* XXX Lwt_sequence.iter_node_l Lwt_sequence.remove Lwt_main.exit_hooks; *)
      Lwt.return pid

let daemon f =
  let grandchild () =
    Lwt_unix.chdir "/" >>= fun () ->
    Lwt_unix.openfile "/dev/null" [Unix.O_RDWR] 0 >>= fun dev_null ->
    let close_and_dup fd =
      Lwt_unix.close fd >>= fun () ->
      Lwt_unix.dup2 dev_null fd;
      Lwt.return_unit in
    let descrs = Lwt_unix.[stdin; stdout; stderr] in
    Lwt_list.iter_p close_and_dup descrs >>= fun () ->
    Lwt_unix.close dev_null >>= fun () ->
    f () in
  let child () =
    ignore (Unix.setsid ());
    Sys.set_signal Sys.sighup Sys.Signal_ignore;
    Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
    fork () >>= function
    | 0 -> grandchild ()
    | _ -> exit 0 in
  ignore (Unix.umask 0);
  fork () >>= function
  | 0 -> child ()
  | _ -> exit 0

let read_pid fd =
  let buf = Buffer.create 32 in
  Io.read_once fd buf 0 32 >>= fun k ->
  let pid = String.trim (Buffer.to_string (Buffer.sub buf 0 k)) in
  Lwt.catch
    (fun () -> Lwt.return (Some (int_of_string pid)))
    (fun _ -> Lwt.return_none)

let read_lock_file path =
  Lwt.catch
    (fun () ->
      Lwt_unix.openfile path [Unix.O_RDONLY] 0 >>= fun fd ->
      Lwt.finalize
        (fun () -> read_pid fd)
        (fun () -> Lwt_unix.close fd))
    (function
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
        Lwt.return_none
    | Unix.Unix_error (e, _, _) ->
        let err = Unix.error_message e in
        Lwt_log.error_f "cannot read lock file: %s: %s" path err >>= fun () ->
        Lwt.return_none
    | e ->
        Lwt.fail e)

let write_pid path =
  Lwt.catch
    (fun () ->
      let buf = Buffer.of_string (string_of_int (Unix.getpid ()) ^ "\n") in
      let flags = [Unix.O_CREAT; Unix.O_WRONLY] in
      Lwt_unix.openfile path flags 0o644 >>= fun fd ->
      Io.write fd buf)
    (function
    | Unix.Unix_error (e, _, _) ->
        let err = Unix.error_message e in
        Lwt_log.error_f "cannot create lock file %s: %s" path err
    | e ->
        Lwt.fail e)

let create_lock_file path =
  read_lock_file path >>= fun pid ->
  match pid with
  | None ->
      write_pid path
  | Some pid ->
      Lwt.catch
        (fun () ->
          Unix.kill pid 0;
          Lwt_log.error_f
            "there is already a running instance of %s: %d" Sys.argv.(0) pid)
        (function
        | Unix.Unix_error (Unix.ESRCH, _, _) ->
            Lwt_log.info_f
              "found a stale pid in lock file %s: %d" path pid >>= fun () ->
            write_pid path
        | e ->
            Lwt.fail e)

let remove_lock_file path =
  read_lock_file path >>= function
  | None ->
      Lwt_log.info_f "couldn't find lock file %s" path
  | Some pid ->
      let mypid = Unix.getpid () in
      if pid = mypid then
        Lwt_unix.unlink path
      else
        Lwt_log.info_f
          "pid mismatch in lock file: found %d, mine is %d; not removing"
          pid mypid

let check_user test msg =
  if not (test (Unix.getuid ())) then
    Lwt_log.error_f "%s %s" Sys.argv.(0) msg >>= fun () ->
    exit 1
  else
    Lwt.return_unit

let check_root () =
  check_user ((=) 0) "must be run by root"

let check_nonroot () =
  check_user ((<>) 0) "cannot be run by root"

let try_exec run ((path, _) as cmd) =
  let can_exec st =
    let kind = st.Lwt_unix.st_kind in
    let perm = st.Lwt_unix.st_perm in
    kind = Unix.S_REG && perm land 0o100 <> 0 in
  Lwt.catch
    (fun () ->
      Lwt_unix.lstat path >>= fun st ->
      Lwt.return (can_exec st))
    (function
    | Unix.Unix_error (e, _, _) ->
        let err = Unix.error_message e in
        Lwt_log.error_f "cannot stat `%s': %s" path err >>= fun () ->
        exit 126
    | e ->
        let err = Printexc.to_string e in
        Lwt_log.error_f "cannot stat `%s': %s" path err >>= fun () ->
        exit 126)
  >>= fun ok ->
  if ok then
    run cmd
  else
    Lwt_log.error_f "cannot execute `%s'" path >>= fun () ->
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

module ConnMap = Map.Make(struct
  type t = int
  let compare = compare
end)

let slave_connection_map = ref ConnMap.empty
let slave_connection_map_mtx = Lwt_mutex.create ()

let add_slave_connection pid conn =
  Lwt_mutex.with_lock slave_connection_map_mtx
    (fun () ->
      slave_connection_map := ConnMap.add pid conn !slave_connection_map;
      Lwt.return_unit)

let remove_slave_connection pid =
  Lwt_mutex.with_lock slave_connection_map_mtx
    (fun () ->
      slave_connection_map := ConnMap.remove pid !slave_connection_map;
      Lwt.return_unit)

let slave_connections () =
  ConnMap.bindings !slave_connection_map

let getenv k =
  try
    let v = Sys.getenv k in
    Some (sprintf "%s=%s" k v)
  with Not_found ->
    None

let restrict_env = function
  | `Inherit ->
      [||]
  | `Keep allowed ->
      let setenv env k =
        Option.may_default env (fun v -> v::env) (getenv k) in
      Array.of_list (List.fold_left setenv [] allowed)

let handle_process master_fd reexec proc =
  let log fmt = ksprintf (fun s -> Lwt_log.notice_f "process %s" s) fmt in
  let pid = proc#pid in
  add_slave_connection pid master_fd >>= fun () ->
  log "%d created" pid >>= fun () ->
	proc#status >>= begin function
  | Lwt_unix.WEXITED 0 -> log "%d exited normally" pid
  | Lwt_unix.WEXITED s -> log "%d exited with status %s" pid (signame s)
  | Lwt_unix.WSIGNALED s -> log "%d signaled to death by %s" pid (signame s)
  | Lwt_unix.WSTOPPED s -> log "%d stopped by signal %s" pid (signame s)
  end >>= fun () ->
  remove_slave_connection pid >>= fun () ->
  reexec ()

let fork_exec cmd env reexec =
  let master_fd, slave_fd = Lwt_unix.(socketpair PF_UNIX SOCK_STREAM 0) in
  Lwt_unix.set_close_on_exec master_fd;
  Lwt_process.with_process_none
    ~stdin:(`FD_move (Lwt_unix.unix_file_descr slave_fd))
    ~env
    cmd
    (fun proc ->
      let master_conn = Ipc.create_connection master_fd in
      handle_process master_conn reexec proc)

let rec exec_process cmd ipc_handler slave_env check_death_rate =
  let run_proc cmd =
    let reexec () =
      match check_death_rate () with
      | `Ok -> exec_process cmd ipc_handler slave_env check_death_rate
      | `Disabled -> Lwt.return_unit
      | `Exceeded ->
          Lwt_log.error "slave process dying too fast" >>= fun () ->
          exit 1 in
    let env = restrict_env slave_env in
    fork_exec cmd env reexec  in
  let _slave_t =
    try_exec run_proc cmd in
  Lwt.return_unit

let num_exec_tries = 10 (* per second *)
let death_rate_checking_enabled = ref true

let disable_slave_restart () =
  death_rate_checking_enabled := false

let check_death_rate time tries () =
  if !death_rate_checking_enabled then begin
    let now = Unix.time () in
    if now -. !time > 1. then begin
      tries := num_exec_tries;
      time := now
    end;
    match !tries with
    | 0 -> `Exceeded
    | _ -> decr tries; `Ok
  end else
    `Disabled

let init_exec_slave max_tries =
  let tries = ref max_tries in
  let time = ref 0. in
  fun cmd ipc_handler slave_env ->
    exec_process cmd ipc_handler slave_env (check_death_rate time tries)

let signal_slaves signum =
  Lwt_list.iter_p
    (fun (pid, _) ->
      Lwt_log.info_f "signaling process %d" pid >>= fun () ->
      Unix.kill pid signum;
      Lwt.return_unit)
    (slave_connections ())

let async_exit signame signum =
  disable_slave_restart ();
  Lwt.async
    (fun () ->
      Lwt_log.info_f "got %s, signaling child processes" signame >>= fun () ->
      signal_slaves signum >>= fun () ->
      Lwt_log.info "exiting" >>= fun () ->
      exit (128 + signum));
  (* XXX Lwt sometimes hangs without the call below. Maybe related to
   * https://github.com/ocsigen/lwt/issues/48 *)
  exit (128 + signum)

let handle_sigint _ =
  async_exit "SIGINT" 2

let handle_sigterm _ =
  async_exit "SIGTERM" 15

let curry f (x, y) = f x y

let rec loop i n f =
  if i > n then
    Lwt.return_unit
  else
    f () >>= fun () ->
    loop (i+1) n f

let default_slave_env = `Keep ["TZ"; "OCAMLRUNPARAM"]

let set_logger logger =
  Lwt_log.default := logger

let master_slaves ?(background = true) ?logger ?(privileged = true)
                  ?(slave_env = default_slave_env) ?control ?main
                  ~lock_file ~slaves () =
  Option.either ignore set_logger logger;
  let create_slaves (cmd, ipc_handler, n) =
    loop 1 n
      (fun () ->
        let exec_slave = init_exec_slave num_exec_tries in
        exec_slave cmd ipc_handler slave_env) in
  let work () =
    ignore (Lwt_unix.on_signal Sys.sigint handle_sigint);
    ignore (Lwt_unix.on_signal Sys.sigterm handle_sigterm);
    create_lock_file lock_file >>= fun () ->
    Lwt_main.at_exit (fun () -> remove_lock_file lock_file);
    let idle_t, _wait_t = Lwt.wait () in
    let control_t =
      Option.either Lwt.return (curry Ipc.control_socket) control in
    Lwt_list.iter_p create_slaves slaves >>= fun () ->
    let main_t =
      Option.either Lwt.return (fun f -> f slave_connections) main in
    main_t <&> control_t <&> idle_t in
  let main_t =
    let check = if privileged then check_root else check_nonroot in
    check () >>= fun () ->
    if background then daemon work else work () in
  Lwt_main.run main_t

let master_slave ~slave =
  let (cmd, ipc_handler) = slave in
  master_slaves ~slaves:[cmd, ipc_handler, 1]

let lose_privileges user =
  check_root () >>= fun () ->
  Lwt.catch
    (fun () ->
      Privileges.drop user)
    (function
    | Privileges.Error err ->
        Lwt_log.error_f "Error dropping privileges: %s" err >>= fun () ->
        exit 1
    | e ->
        Lwt.fail e)

let me ?logger ?user ~main () =
  let main_t =
    Option.either check_nonroot lose_privileges user >>= fun () ->
    Option.either ignore set_logger logger;
    let ipc_fd = Lwt_unix.dup Lwt_unix.stdin in
    Lwt_unix.close Lwt_unix.stdin >>= fun () ->
    let pid = Unix.getpid () in
    Lwt_log.info_f "starting up (PID %d)" pid >>= fun () ->
    let ipc_conn = Ipc.create_connection ipc_fd in
    main ipc_conn >>= fun () ->
    Lwt_log.info_f "stopping (PID %d)" pid >>= fun () ->
    Lwt.return_unit in
  Lwt_main.run main_t
