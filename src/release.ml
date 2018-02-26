open Printf
open Stdint
open Util

module type S = sig
  type +'a future
  type fd
  type command = string * string array
  type logger

  module Buffer : sig
    type t

    val create : int -> t
    val make : int -> char -> t
    val get : t -> int -> char
    val set : t -> int -> char -> unit
    val size : t -> int
    val length : t -> int
    val index : t -> char -> int option
    val index_from : t -> int -> char -> int option
    val add_char : t -> char -> unit
    val add_bytes : t -> bytes -> unit
    val add_buffer : t -> t -> unit
    val contents : t -> string
    val to_string : t -> string
    val of_string : string -> t
    val blit : t -> int -> t -> int -> int -> unit
    val sub : t -> int -> int -> t
    val read : fd -> t -> int -> int -> int future
    val write : fd -> t -> int -> int -> int future
  end

  module Io : sig
    val read_once : fd
                 -> Buffer.t
                 -> int
                 -> int
                 -> int future
    val read : ?timeout:float
            -> fd
            -> int
            -> [`Data of Buffer.t | `EOF | `Timeout] future
    val write : fd -> Buffer.t -> unit future
  end

  module Binary : sig
    val read_byte_at : int -> Buffer.t -> int
    val read_byte : Buffer.t -> int
    val write_byte : int -> Buffer.t -> unit

    module type Integer = sig
      type t
      val bytes : int
      val zero : t
      val byte_max : t
      val shift_left : t -> int -> t
      val shift_right_logical : t -> int -> t
      val logor : t -> t -> t
      val logand : t -> t -> t
      val of_int : int -> t
      val to_int : t -> int
    end

    module type ByteOps = sig
      type t
      val read_at : int -> Buffer.t -> t
      val write_byte : t -> Buffer.t -> unit
      val write : t -> Buffer.t -> unit
    end

    module type IntegerOps = sig
      module Make (I : Integer) : ByteOps with type t = I.t

      val read_int16_at : int -> Buffer.t -> int
      val read_int16 : Buffer.t -> int
      val write_int16_byte : int -> Buffer.t -> unit
      val write_int16 : int -> Buffer.t -> unit

      val read_int_at : int -> Buffer.t -> int
      val read_int : Buffer.t -> int
      val write_int_byte : int -> Buffer.t -> unit
      val write_int : int -> Buffer.t -> unit

      val read_int32_at : int -> Buffer.t -> int32
      val read_int32 : Buffer.t -> int32
      val write_int32_byte : int32 -> Buffer.t -> unit
      val write_int32 : int32 -> Buffer.t -> unit

      val read_uint32_at : int -> Buffer.t -> Uint32.t
      val read_uint32 : Buffer.t -> Uint32.t
      val write_uint32_byte : Uint32.t -> Buffer.t -> unit
      val write_uint32 : Uint32.t -> Buffer.t -> unit

      val read_int64_at : int -> Buffer.t -> int64
      val read_int64 : Buffer.t -> int64
      val write_int64_byte : int64 -> Buffer.t -> unit
      val write_int64 : int64 -> Buffer.t -> unit

      val read_uint64_at : int -> Buffer.t -> Uint64.t
      val read_uint64 : Buffer.t -> Uint64.t
      val write_uint64_byte : Uint64.t -> Buffer.t -> unit
      val write_uint64 : Uint64.t -> Buffer.t -> unit

      val read_uint128_at : int -> Buffer.t -> Uint128.t
      val read_uint128 : Buffer.t -> Uint128.t
      val write_uint128_byte : Uint128.t -> Buffer.t -> unit
      val write_uint128 : Uint128.t -> Buffer.t -> unit
    end

    module Big_endian : IntegerOps
    module Little_endian : IntegerOps
  end

  module Config : sig
    module Value : sig
      type t =
        [ `Keyword of string
        | `Bool of bool
        | `Int of int
        | `Float of float
        | `Str of string
        | `Regexp of Re.re
        | `List of t list
        ]

      val to_keyword : [>`Keyword of string] -> string
      val to_bool : [>`Bool of bool] -> bool
      val to_int : [>`Int of int] -> int
      val to_float : [>`Float of float] -> float
      val to_string : [>`Str of string] -> string
      val to_regexp : [>`Regexp of Re.re] -> Re_pcre.regexp
      val to_list : string -> (t -> 'a) -> [>`List of t list] -> 'a list
      val to_keyword_list : [>`List of [>`Keyword of string] list]
                         -> string list
      val to_bool_list : [>`List of [>`Bool of bool] list] -> bool list
      val to_int_list : [>`List of [>`Int of int] list] -> int list
      val to_float_list : [>`List of [>`Float of float] list] -> float list
      val to_string_list : [>`List of [>`Str of string] list] -> string list

      module Default : sig
        val keyword : string -> [>`Keyword of string] option
        val bool : bool -> [>`Bool of bool] option
        val int : int -> [>`Int of int] option
        val float : float -> [>`Float of float] option
        val string : string -> [>`Str of string] option
        val regexp : Re_pcre.regexp -> [>`Regexp of Re.re] option
        val keyword_list : string list
                        -> [>`List of [>`Keyword of string] list] option
        val bool_list : bool list -> [>`List of [>`Bool of bool] list] option
        val int_list : int list -> [>`List of [>`Int of int] list] option
        val float_list : float list
                      -> [>`List of [>`Float of float] list] option
        val string_list : string list
                       -> [>`List of [>`Str of string] list] option
      end
    end

    module Validation : sig
      type result = [`Valid | `Invalid of string]
      type t = Value.t -> result

      val keyword : string -> t
      val keywords : string list -> t
      val bool : t
      val int : t
      val float : t
      val string : t
      val regexp : t
      val bool_list : t
      val int_list : t
      val float_list : t
      val string_list : t
      val int_in_range : int * int -> t
      val int_greater_than : int -> t
      val int_less_than : int -> t
      val float_in_range : float * float -> t
      val float_greater_than : float -> t
      val float_less_than : float -> t
      val string_matching : string -> t
      val int_in : int list -> t
      val string_in : string list -> t
      val existing_file : t
      val nonempty_file : t
      val file_with_mode : Unix.file_perm -> t
      val file_with_owner : string -> t
      val file_with_group : string -> t
      val existing_directory : t
      val existing_dirname : t
      val block_device : t
      val character_device : t
      val symbolic_link : t
      val named_pipe : t
      val unix_socket : t
      val existing_user : t
      val unprivileged_user : t
      val existing_group : t
      val list_of : t -> t
    end

    type t
    type key = string * Value.t option * Validation.t list
    type section =
      [ `Global of key list
      | `Section of (string * key list)
      ]
    type spec = section list
    val parse : string
             -> spec
             -> [`Configuration of t | `Error of string] future
    val defaults : spec -> t
    val get : t -> string -> string -> Value.t
    val get_global : t -> string -> Value.t
  end

  module Socket : sig
    type unix = [ `Unix of string ]
    type inet = [ `Inet of Unix.inet_addr * int ]
    type addr = [ unix | inet ]
    type ('state, 'addr) t
      constraint 'state = [< `Unconnected | `Bound | `Passive | `Active ]
      constraint 'addr  = [< addr ]

    val accept_loop : ?backlog:int
                   -> ?timeout:float
                   -> ([`Unconnected], 'addr) t
                   -> 'addr
                   -> (([`Active], 'addr) t -> unit future)
                   -> 'a future
  end

  module Ipc : sig
    type connection
    type handler = connection -> unit future

    val create_connection : ([`Active], Socket.unix) Socket.t -> connection
    val control_socket : string -> handler -> unit future

    module type Types = sig
      type request
      type response
    end

    module type Ops = sig
      include Types

      val string_of_request : request -> string
      val request_of_string : string -> request
      val string_of_response : response -> string
      val response_of_string : string -> response
    end

    module Marshal : sig
      module Make (T : Types) : Ops
        with type request := T.request and type response := T.response
    end

    module type S = sig
      type request
      type response

      module Server : sig
        val read_request : ?timeout:float
                        -> connection
                        -> [`Request of request | `EOF | `Timeout] future
        val write_response : connection -> response -> unit future
        val handle_request : ?timeout:float
                          -> ?eof_warning:bool
                          -> connection
                          -> (request -> response future)
                          -> unit future
      end

      module Client : sig
        val read_response : ?timeout:float
                         -> connection
                         -> [`Response of response | `EOF | `Timeout] future
        val write_request : connection -> request -> unit future
        val make_request : ?timeout:float
                        -> connection
                        -> request
                        -> ([`Response of response | `EOF | `Timeout] ->
                             'a future)
                        -> 'a future
      end
    end

    module Make (O : Ops) : S
      with type request := O.request and type response := O.response
  end

  module Util : sig
    module Option : sig
      val either : (unit -> 'a) -> ('b -> 'a) -> 'b option -> 'a
      val some : 'a option -> 'a
      val default : 'a -> 'a option -> 'a
      val may : ('a -> unit) -> 'a option -> unit
      val maybe : ('a -> 'b option) -> 'a option -> 'b option
      val may_default : 'a -> ('b -> 'a) -> 'b option -> 'a
      val map : ('a -> 'b) -> 'a option -> 'b option
      val choose : 'a option -> 'a option -> 'a option
      val apply : 'a -> 'b -> ('b -> 'a) option -> 'a
    end
  end

  val daemon : (unit -> unit future) -> unit future

  val master_slave :
         slave:(command * Ipc.handler)
      -> ?background:bool
      -> ?logger:logger
      -> ?privileged:bool
      -> ?slave_env:[`Inherit | `Keep of string list]
      -> ?control:(string * Ipc.handler)
      -> ?main:((unit -> (int * Ipc.connection) list) -> unit future)
      -> lock_file:string
      -> unit -> unit

  val master_slaves :
         ?background:bool
      -> ?logger:logger
      -> ?privileged:bool
      -> ?slave_env:[`Inherit | `Keep of string list]
      -> ?control:(string * Ipc.handler)
      -> ?main:((unit -> (int * Ipc.connection) list) -> unit future)
      -> lock_file:string
      -> slaves:(command * Ipc.handler * int) list
      -> unit -> unit

  val me : ?logger:logger
        -> ?user:string
        -> main:(Ipc.connection -> unit future)
        -> unit -> unit
end

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
