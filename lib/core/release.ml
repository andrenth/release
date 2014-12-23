open Printf
open Release_util

module type S = sig
  type +'a future
  type command = string * string array
  type fd

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
    val add_string : t -> string -> unit
    val add_buffer : t -> t -> unit
    val contents : t -> string
    val to_string : t -> string
    val of_string : string -> t
    val blit : t -> int -> t -> int -> int -> unit
    val sub : t -> int -> int -> t
    val read : fd -> t -> int -> int -> int future
    val write : fd -> t -> int -> int -> int future
  end

  module Bytes : sig
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
        | `Regexp of Str.regexp
        | `List of t list
        ]

      val to_keyword : [>`Keyword of string] -> string
      val to_bool : [>`Bool of bool] -> bool
      val to_int : [>`Int of int] -> int
      val to_float : [>`Float of float] -> float
      val to_string : [>`Str of string] -> string
      val to_regexp : [>`Regexp of Str.regexp] -> Str.regexp
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
        val regexp : Str.regexp -> [>`Regexp of Str.regexp] option
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

  module IO : sig
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

  module IPC : sig
    type handler = fd -> unit future

    val control_socket : string -> handler -> unit future

    module type Ops = sig
      type request
      type response

      val string_of_request : request -> string
      val request_of_string : string -> request
      val string_of_response : response -> string
      val response_of_string : string -> response
    end

    module type S = sig
      type request
      type response

      module Server : sig
        val read_request : ?timeout:float
                        -> fd
                        -> [`Request of request | `EOF | `Timeout] future
        val write_response : fd -> response -> unit future
        val handle_request : ?timeout:float
                          -> ?eof_warning:bool
                          -> fd
                          -> (request -> response future)
                          -> unit future
      end

      module Client : sig
        val read_response : ?timeout:float
                         -> fd
                         -> [`Response of response | `EOF | `Timeout] future
        val write_request : fd -> request -> unit future
        val make_request : ?timeout:float
                        -> fd
                        -> request
                        -> ([`Response of response | `EOF | `Timeout] ->
                             'a future)
                        -> 'a future
      end
    end

    module Make (O : Ops) : S
      with type request = O.request and type response = O.response
  end

  module Socket : sig
    val accept_loop : ?backlog:int
                   -> ?timeout:float
                   -> fd
                   -> Unix.sockaddr
                   -> (fd -> unit future)
                   -> unit future
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
         slave:(command * IPC.handler)
      -> ?background:bool
      -> ?syslog:bool
      -> ?privileged:bool
      -> ?slave_env:[`Inherit | `Keep of string list]
      -> ?control:(string * IPC.handler)
      -> ?main:((unit -> (int * fd) list) -> unit future)
      -> lock_file:string
      -> unit -> unit

  val master_slaves :
         ?background:bool
      -> ?syslog:bool
      -> ?privileged:bool
      -> ?slave_env:[`Inherit | `Keep of string list]
      -> ?control:(string * IPC.handler)
      -> ?main:((unit -> (int * fd) list) -> unit future)
      -> lock_file:string
      -> slaves:(command * IPC.handler * int) list
      -> unit -> unit

  val me : ?syslog:bool
        -> ?user:string
        -> main:(fd -> unit future)
        -> unit -> unit
end

module Make (Future : Release_future.S) : S
  with type 'a future = 'a Future.t
   and type fd = Future.Unix.fd =
struct
  (* XXX this isn't working *)
  module Future = struct
    include (Future : Release_future.S
      with type 'a t = 'a Future.t
       and type Unix.fd = Future.Unix.fd
       and module Monad := Future.Monad)
    module Monad = struct
      include Future.Monad
      let return_unit = return ()
      let return_none = return None
      let (<&>) t t' = join [t; t']
    end
  end
  open Future.Monad

  type +'a future = 'a Future.t
  type fd = Future.Unix.fd

  type command = string * string array

  module Buffer = Release_buffer.Make (Future)
  module Bytes = Release_bytes.Make (Buffer)
  module Config = Release_config.Make (Future)
  module Socket = Release_socket.Make (Future)
  module IO = Release_io.Make (Future) (Buffer)
  module IPC = Release_ipc.Make (Future) (Buffer) (IO) (Socket)
  module Util = Release_util

  let daemon f =
    let grandchild () =
      Future.Unix.chdir "/" >>= fun () ->
      Future.Unix.openfile "/dev/null" [Unix.O_RDWR] 0 >>= fun dev_null ->
      let close_and_dup fd =
        Future.Unix.close fd >>= fun () ->
        Future.Unix.dup2 dev_null fd >>= fun () ->
        Future.Unix.close dev_null in
      let descrs =
        [Future.Unix.stdin; Future.Unix.stdout; Future.Unix.stderr] in
      Future.iter_p close_and_dup descrs >>= fun () ->
      Future.Unix.close dev_null >>= fun () ->
      f () in
    let child () =
      ignore (Unix.setsid ());
      Sys.set_signal Sys.sighup Sys.Signal_ignore;
      Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
      Future.Unix.fork () >>= function
      | 0 -> grandchild ()
      | _ -> Future.Unix.exit 0 in
    ignore (Unix.umask 0);
    Future.Unix.fork () >>= function
    | 0 -> child ()
    | _ -> Future.Unix.exit 0

  let read_lock_file path =
    Future.catch
      (fun ()->
        Future.IO.with_input_file path
          (fun ch ->
            Future.IO.read_line ch >>= function
            | None ->
                return_none
            | Some l ->
                Future.catch
                  (fun () -> return (Some (int_of_string l)))
                  (fun _ -> return_none)))
      (function
      | Unix.Unix_error (Unix.ENOENT, _, _) ->
          return_none
      | Unix.Unix_error (e, _, _) ->
          let err = Unix.error_message e in
          Future.Logger.error_f "cannot read lock file: %s: %s" path err
          >>= fun () ->
          return_none
      | e ->
          Future.fail e)

  let write_pid path =
    Future.catch
      (fun () ->
        Future.IO.with_output_file path
          (fun ch -> Future.IO.fprintf ch "%d\n" (Unix.getpid ())))
      (function
      | Unix.Unix_error (e, _, _) ->
          let err = Unix.error_message e in
          Future.Logger.error_f "cannot create lock file %s: %s" path err
      | e ->
          Future.fail e)

  let create_lock_file path =
    read_lock_file path >>= fun pid ->
    match pid with
    | None ->
        write_pid path
    | Some pid ->
        Future.catch
          (fun () ->
            Unix.kill pid 0;
            Future.Logger.error_f
              "there is already a running instance of %s: %d" Sys.argv.(0) pid)
          (function
          | Unix.Unix_error (Unix.ESRCH, _, _) ->
              Future.Logger.info_f
                "found a stale pid in lock file %s: %d" path pid
              >>= fun () ->
              write_pid path
          | e ->
              Future.fail e)

  let remove_lock_file path =
    read_lock_file path >>= fun pid ->
    match pid with
    | None ->
        Future.Logger.info_f "couldn't find lock file %s" path
    | Some pid ->
        let mypid = Unix.getpid () in
        if pid = mypid then
          Future.Unix.unlink path
        else
          Future.Logger.info_f
            "pid mismatch in lock file: found %d, mine is %d; not removing"
            pid mypid

  let check_user test msg =
    if not (test (Unix.getuid ())) then
      Future.Logger.error_f "%s %s" Sys.argv.(0) msg >>= fun () ->
      Future.Unix.exit 1
    else
      return_unit

  let check_root () =
    check_user ((=) 0) "must be run by root"

  let check_nonroot () =
    check_user ((<>) 0) "cannot be run by root"

  let try_exec run ((path, _) as cmd) =
    let can_exec st =
      let kind = st.Unix.st_kind in
      let perm = st.Unix.st_perm in
      kind = Unix.S_REG && perm land 0o100 <> 0 in
    Future.catch
      (fun () ->
        Future.Unix.lstat path >>= fun st ->
        if can_exec st then
          run cmd
        else
          Future.Logger.error_f "cannot execute `%s'" path >>= fun () ->
          Future.Unix.exit 126)
      (function
      | Unix.Unix_error (e, _, _) ->
          let err = Unix.error_message e in
          Future.Logger.error_f "cannot stat `%s': %s" path err >>= fun () ->
          Future.Unix.exit 126
      | e ->
          let err = Printexc.to_string e in
          Future.Logger.error_f "cannot stat `%s': %s" path err >>= fun () ->
          Future.Unix.exit 126)

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
  let slave_connection_map_mtx = Future.Mutex.create ()

  let add_slave_connection pid fd =
    Future.Mutex.lock slave_connection_map_mtx >>= fun () ->
    slave_connection_map := ConnMap.add pid fd !slave_connection_map;
    Future.Mutex.unlock slave_connection_map_mtx;
    return_unit

  let remove_slave_connection pid =
    Future.Mutex.lock slave_connection_map_mtx >>= fun () ->
    slave_connection_map := ConnMap.remove pid !slave_connection_map;
    Future.Mutex.unlock slave_connection_map_mtx;
    return_unit

  let slave_connections () =
    ConnMap.bindings !slave_connection_map

  let setup_ipc ipc_handler =
    let master_fd, slave_fd = Future.Unix.socketpair () in
    Future.Unix.set_close_on_exec master_fd;
    let _ipc_t = ipc_handler master_fd in
    master_fd, slave_fd

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

  let move_fd fd fd' =
    Future.Unix.dup2 fd fd' >>= fun () ->
    Future.Unix.close fd

  let wait_child pid master_fd reexec =
    let log fmt =
      ksprintf (fun s -> Future.Logger.info_f "process %s" s) fmt in
    Future.Unix.waitpid pid >>= begin function
    | Unix.WEXITED 0 -> log "%d exited normally" pid
    | Unix.WEXITED s -> log "%d exited with status %s" pid (signame s)
    | Unix.WSIGNALED s -> log "%d signaled to death by %s" pid (signame s)
    | Unix.WSTOPPED s -> log "%d stopped by signal %s" pid (signame s)
    end >>= fun () ->
    remove_slave_connection pid >>= fun () ->
    reexec ()

  let fork_exec (path, argv) env master_fd slave_fd reexec =
    let child () =
      move_fd slave_fd Future.Unix.stdin >>= fun () ->
      try
        Unix.execve path argv env
      with _ ->
        Future.Logger.error_f "execve: %s" path >>= fun () ->
        Future.Unix.exit 127 in
    let parent pid =
      Future.Logger.error_f "process %d created" pid >>= fun () ->
      add_slave_connection pid master_fd >>= fun () ->
      Future.Unix.close slave_fd >>= fun () ->
      wait_child pid master_fd reexec in
    Future.Unix.fork () >>= function
    | 0 -> child ()
    | pid -> parent pid

  let rec exec_process cmd ipc_handler slave_env check_death_rate =
    let master_fd, slave_fd = setup_ipc ipc_handler in
    let run_proc cmd =
      let reexec () =
        check_death_rate () >>= fun restart ->
        if restart then
          exec_process cmd ipc_handler slave_env check_death_rate
        else
          Future.Logger.error "slave process dying too fast" >>= fun () ->
          Future.Unix.exit 1 in
      fork_exec cmd (restrict_env slave_env) master_fd slave_fd reexec in
    let _slave_t =
      try_exec run_proc cmd in
    return_unit

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
      | 0 ->
          return false
      | _ ->
          decr tries;
          return true
    end else
      return false

  let init_exec_slave max_tries =
    let tries = ref max_tries in
    let time = ref 0. in
    fun cmd ipc_handler slave_env ->
      exec_process cmd ipc_handler slave_env (check_death_rate time tries)

  let signal_slaves signum =
    Future.iter_p
      (fun (pid, _) ->
        Future.Logger.info_f "signaling process %d" pid >>= fun () ->
        Unix.kill pid signum;
        return_unit)
      (slave_connections ())

  let async_exit signame signum =
    Future.async
      (fun () ->
        Future.Logger.info_f "got %s, signaling child processes" signame
        >>= fun () ->
        signal_slaves signum >>= fun () ->
        Future.Logger.info "exiting" >>= fun () ->
        Future.Unix.exit (128 + signum))

  let handle_sigint _ =
    async_exit "SIGINT" 2

  let handle_sigterm _ =
    async_exit "SIGTERM" 15

  let curry f (x, y) = f x y

  let setup_syslog () =
    try
      Future.Logger.log_to_syslog ()
    with e ->
      let err = Printexc.to_string e in
      fprintf stderr "could not setup syslog: %s" err;
      exit 1

  let rec loop i n f =
    if i > n then
      return_unit
    else
      f () >>= fun () ->
      loop (i+1) n f

  let default_slave_env = `Keep ["TZ"; "OCAMLRUNPARAM"]

  let master_slaves ?(background = true) ?(syslog = true) ?(privileged = true)
                    ?(slave_env = default_slave_env) ?control ?main ~lock_file
                    ~slaves () =
    if syslog then setup_syslog ();
    let create_slaves (cmd, ipc_handler, n) =
      loop 1 n
        (fun () ->
          let exec_slave = init_exec_slave num_exec_tries in
          exec_slave cmd ipc_handler slave_env) in
    let work () =
      Future.Unix.on_signal Sys.sigint handle_sigint;
      Future.Unix.on_signal Sys.sigterm handle_sigterm;
      create_lock_file lock_file >>= fun () ->
      Future.Main.at_exit (fun () -> remove_lock_file lock_file);
      let idle_t = Future.idle () in
      let control_t =
        Option.either return (curry IPC.control_socket) control in
      Future.iter_p create_slaves slaves >>= fun () ->
      let main_t =
        Option.either return (fun f -> f slave_connections) main in
      main_t <&> control_t <&> idle_t in
    let main_t =
      if privileged then check_root () else check_nonroot () >>= fun () ->
      if background then daemon work else work () in
    Future.Main.run main_t

  let master_slave ~slave =
    let (cmd, ipc_handler) = slave in
    master_slaves ~slaves:[cmd, ipc_handler, 1]

  let lose_privileges user =
    let module Privileges = Release_privileges.Make (Future) in
    check_root () >>= fun () ->
    Future.catch
      (fun () ->
        Privileges.drop user)
      (function
      | Privileges.Release_privileges_error err ->
          Future.Logger.error err >>= fun () ->
          Future.Unix.exit 1
      | e ->
          Future.fail e)

  let me ?(syslog = true) ?user ~main () =
    let main_t =
      Option.either check_nonroot lose_privileges user >>= fun () ->
      if syslog then setup_syslog ();
      Future.Unix.dup Future.Unix.stdin >>= fun ipc_fd ->
      Future.Unix.close Future.Unix.stdin >>= fun () ->
      let pid = Unix.getpid () in
      Future.Logger.info_f "starting up (PID %d)" pid >>= fun () ->
      main ipc_fd >>= fun () ->
      Future.Logger.info_f "stopping (PID %d)" pid >>= fun () ->
      return_unit in
    Future.Main.run main_t
end
