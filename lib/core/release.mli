(** Release is a multi-process daemon library for OCaml. Its main goal is
    to ease the development of servers following the common pattern of one
    master process and one or many slave processes.

    The usual tasks involved in setting up such process scheme are handled
    by Release. These consist of turning the master process into a daemon,
    executing the slave processes, setting up inter-process communication
    channels between master and slave, dropping privileges and chroot'ing
    the slave process, etc.

    The library also provides helper modules {!Release_io}, for simple and
    safe I/O operations, {!Release_ipc}, for type- and thread-safe
    inter-process communication and {!Release_socket} for miscellaneous
    socket-related utility functions.

    Release also provides some utility sub-modules: [Release_buffer], a
    module for buffer operations implemented on top of [Lwt_bytes],
    [Release_bytes], for handling binary representation of integers stored
    in buffers, [Release_config], for parsing and validating configuration
    files, and [Release_util], with miscellaneous functions. Please refer
    to each sub-module's documentation for details on their interfaces.

    Whenever a thread is mentioned in this documentation, it refers to an
    [Lwt.t]
*)

val daemon : (unit -> unit Lwt.t) -> unit Lwt.t
  (** [daemon f] turns the current process into a daemon and subsequently
      calls [f]. The following steps are taken by the function:
        + The [umask] is set to 0;
        + [fork] is called and the parent process exits;
        + The child process calls [setsid], ignores [SIGHUP] and [SIGPIPE],
          calls [fork] again and exits;
        + The grandchild process changes the working directory to [/],
          redirects [stdin], [stdout] and [stderr] to [/dev/null] and
          calls [f]. *)

val master_slave :
       slave:(Lwt_process.command * Release_ipc.handler)
    -> ?background:bool
    -> ?syslog:bool
    -> ?privileged:bool
    -> ?slave_env:[`Inherit | `Keep of string list]
    -> ?control:(Lwt_io.file_name * Release_ipc.handler)
    -> ?main:((unit -> (int * Lwt_unix.file_descr) list) -> unit Lwt.t)
    -> lock_file:Lwt_io.file_name
    -> unit -> unit
  (** Sets up a master process with one slave.

      [slave] is a tuple whose first element contains the path to the slave
      process executable and second element is a callback function that is
      called in the master process to handle IPC requests from the slave
      (see {!Release_ipc}).

      [background] indicates whether the daemon {!daemon} will be called.
      Defaults to [true].

      [syslog] indicates whether syslog logging is enabled. Logging in
      Release is handled via the [Lwt_log] module. Defaults to [true].

      [privileged] indicates if the master process is to be run as [root].
      Defaults to [true].

      [slave_env] controls the environment variables available to the slave
      process. If [slave_env] is [`Inherit], the slave process will inherit
      the master's full environment. Otherwise, if [slave_env] is
      [`Keep env], the slave process will only have access to the variables
      in the [env] list. Defaults to [`Keep ["OCAMLRUNPARAM"]].

      [control], if present, is a tuple containing a path to a UNIX domain
      socket that will be created for communication with external process and
      a callback function that is called when data is sent on the socket.
      Release will set up a listener thread to deal with IPC on the control
      socket and each connection will be handled by a separate thread.

      [main], if given, is a callback function that works as the main thread
      of the master process. This function receives as an argument a function
      that returns the current list of sockets that connect the master process
      to the slave processes. This is useful for broadcast-style communication
      from the master to the slaves.

      [lock_file] is the path to the lock file created by the master process.
      This file contains the PID of the master process. If the file already
      exists and contains the PID of a running process, the master will refuse
      to start. *)

val master_slaves :
       ?background:bool
    -> ?syslog:bool
    -> ?privileged:bool
    -> ?slave_env:[`Inherit | `Keep of string list]
    -> ?control:(Lwt_io.file_name * Release_ipc.handler)
    -> ?main:((unit -> (int * Lwt_unix.file_descr) list) -> unit Lwt.t)
    -> lock_file:Lwt_io.file_name
    -> slaves:(Lwt_process.command * Release_ipc.handler * int) list
    -> unit -> unit
   (** This function generalizes {!master_slave}, taking the same arguments,
       except for [slave], which is substituted by [slaves]. This argument is
       a list of 3-element tuples. The first element of the tuple is the path
       to a slave executable, the second element is the IPC handler for that
       slave and the third element is the number of instances to be created
       (i.e. the number of times the appropriate executable will be run. *)

val me : ?syslog:bool
      -> ?user:string
      -> main:(Lwt_unix.file_descr -> unit Lwt.t)
      -> unit -> unit
  (** This function is supposed to be called in the slave process.

      [syslog] indicates whether the slave process will log to syslog.
      Defaults to [true].

      [user], if present, indicates the name of the user the slave process
      will drop privileges to. See {!Release_privileges.drop}. Otherwise the
      slave process will run as the same user used to run the master process.

      [main] is a function that works as the entry point for the slave process
      code. The file descriptor given as an argument to [main] can be used by
      the slave for communication with the master. *)
