(** Release is a multi-process daemon library for OCaml. Its main goal is
    to ease the development of servers following the common pattern of one
    master process and one or many slave processes.

    The usual tasks involved in setting up such process scheme are handled
    by Release. These consist of turning the master process into a daemon,
    executing the slave processes, setting up inter-process communication
    channels between master and slave, dropping privileges and chroot'ing
    the slave process, etc.

    The library also provides helper sub-modules [IO], for simple and safe
    I/O operations, [IPC], for type- and thread-safe inter-process
    communication and [Socket] for miscellaneous socket-related utility
    functions.

    Release also provides some utility sub-modules: [Buffer], a module for
    buffer operations, [Bytes], for handling binary representation of integers
    stored in buffers, [Config], for parsing and validating configuration
    files, and [Util], with miscellaneous functions. Please refer to each
    sub-module's documentation for details on their interfaces.

    Release is a multi-process framework where each process handles concurrent
    operations by relying on a library that provides asynchronous computation
    capabilities. The framework provides a functor (see [Relesae.Make]) which
    allows the construction of a module based on any given asynchronous library
    that provides the necessary primitives. In Release, asynchronous
    computations are represented by the ['a future] type.
*)

module type S = sig
  type +'a future
  type fd
  type logger
  type command = string * string array

  (** This module defines a buffer type [Buffer.t] and a set of operations
      on such buffers. *)
  module Buffer : sig
    type t
      (** The type of buffers. *)

    val create : int -> t
      (** [create n] will create a buffer of [n] bytes. *)

    val make : int -> char -> t
      (** [make n c] will create a buffer of [n] bytes, all initialized to
          character [c]. *)

    val get : t -> int -> char
      (** [get buf off] returns the byte at offset [off] in [buf]. *)

    val set : t -> int -> char -> unit
      (** [set buf off c] sets the byte at offset [off] in [buf] to [c]. *)

    val size : t -> int
      (** [size buf] returns the amount of bytes currently allocated for
          [buf]. *)

    val length : t -> int
      (** [length buf] returns the offset of the byte set at the highest
          position in [buf], plus [1]. This means that a mostly empty buffer
          with a single byte set at offset [off] is considered to have length
          [off+1]. *)

    val index : t -> char -> int option
      (** [index buf c] returns [Some off], where [off] is the offset of the
          first occurrence of [c] in [buf], or [None] in case [c] does not
          occur in [buf]. *)

    val index_from : t -> int -> char -> int option
      (** [index_from buf i c] returns [Some off], where [off] is the offset of
          the first occurrence of [c] in [buf], starting from offset [i], or
          [None] in case [c] does not occur in [buf] at or after [i]. If [i] is
          not in the [\[0, length buf)] range, also returns [None]. *)

    val add_char : t -> char -> unit
      (** [add_char buf c] appends character [c] at the end of [buf]. *)

    val add_string : t -> string -> unit
      (** [add_string buf s] appends string [s] at the end of [buf]. *)

    val add_buffer : t -> t -> unit
      (** [add_buffer buf1 buf2] appends buffer [buf2] at the end of [buf1]. *)

    val contents : t -> string
      (** [contents buf] converts the contents of a [buf] to a string. The
          string will be [size buf] bytes long. *)

    val to_string : t -> string
      (** [to_string buf] converts the first [length buf] bytes of [buf] to
          a string. *)

    val of_string : string -> t
      (** Converts a string to a buffer. *)

    val blit : t -> int -> t -> int -> int -> unit
      (** [blit buf1 off1 buf2 off2 len] copies [len] bytes from [buf1],
          starting at offset [off1], to [buf2], starting at offset [off2]. *)

    val sub : t -> int -> int -> t
      (** [sub buf off len] returns a buffer consisting of [len] bytes from
          [buf] starting at offset [off]. No copying is made. *)

    val read : fd -> t -> int -> int -> int future
      (** [read fd buf off n] reads at most [n] bytes from file descriptor [fd]
          into buffer [buf], which is filled starting at offset [off]. Returns
          the number of bytes actually read, and 0 on EOF. *)

    val write : fd -> t -> int -> int -> int future
      (** [write fd buf off n] writes at most [n] bytes from [buf] starting at
          offset [off] into file descriptor [fd]. Returns the number of bytes
          actually written. *)
  end

  (** This module contains the file descriptor type definition and functions
      for easy and safe network I/O functions. The functions are safe in the
      sense that reads and writes are automatically retried when [Unix.EINTR]
      or [Unix.EAGAIN] errors are caught.
  *)
  module IO : sig
    val read_once : fd
                 -> Buffer.t
                 -> int
                 -> int
                 -> int future
      (** [read_once fd buf off n] reads at most [n] bytes from file
          descriptor [fd] into buffer [buf], starting at offset [off] in the
          buffer. The actual number of bytes read is returned. *)

    val read : ?timeout:float
            -> fd
            -> int
            -> [`Data of Buffer.t | `EOF | `Timeout] future
       (** [read fd n] will try to read [n] bytes from file descriptor [fd].
           Data will be read until [n] bytes are read or an end-of-file
           condition occurs. An optional [timeout] argument may be given, in
           which case [read] is interrupted after the specified amount of
           seconds. *)

    val write : fd -> Buffer.t -> unit future
      (** [write fd s] writes the full contents of [s] to file descriptor
          [fd]. *)
  end

  (** This module contains functions for handling binary representation of
      integers.

      Functions in the form [read_<t>_at] will return an integer of type [t]
      at the offset given as the first argument from the buffer given as the
      second argument.

      Functions in the form [read_<t>] are equivalent to [read_<t>_at 0].

      The [write_<t>] functions will append an integer of type [t] given as
      the first argument to the buffer given as the second argument.

      The [write_<t>_byte] functions do the same, but a single byte is
      appended.
  *)
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

  (** This module provides an interface to configuration file parsing.
      Configuration files are assumed to be collections of key-value pairs
      possibly organized in sections.

      Sections are started by a section name enclosed in square brackets.
      Keys and values are separated by an equals sign, and values can be
      integers, floats, booleans, strings, regular expressions, user-defined
      keywords or lists of one of those types, as defined by the {!Value.t}
      type.

      Keys must be named starting with a letter and optionally followed by
      any character except whitespace characters, square brackets, equals
      signs or hash signs.

      Integers values are represented by sequences of digits and floats are
      represented by sequences of digits separated by a dot character.

      Strings are sequences of characters enclosed by double quotes, and
      newline characters inside strings are supported.

      Regular expressions are enclosed by forward slash characters and support
      the same constructs as the ones documented in OCaml's [Str] module, but
      the grouping constructs [(] and [)] and the alternative between
      expressions construct [|] don't need to be escaped by a backslash.
      Newlines can be inserted in regular expressions for organization purposes
      and are ignored along with any following whitespace characters.

      Keywords are any user-defined bare words. For example, one can define
      the keywords [debug], [info], [notice], [warning], [error] and [fatal]
      in order to configure log levels of an application, thus making the
      configuration cleaner by avoiding strings for these settings.

      Finally, lists are sequences of the above types enclosed by square
      brackets and separated by commas.

      In terms of code, configuration files are specified by the {!Config.spec}
      type. This is simply a list of {!Config.section}s, each containing lists
      of {!Config.key}s. Keys are defined by their name, an optional default
      value and a list of validations. If a key has no default value and is
      absent from the configuration file, an error will be generated.

      Validation are functions as defined by the {!Validation.t} type. Many
      pre-defined validations are available in the {!Validation} module.
  *)
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
        (** The type of configuration values. Literal newline (['\n'])
            characters are allowed inside strings. In regular expressions,
            newlines are ignored along with any following whitespace characters
            ([' '], ['\t']). *)

      (** {6 Conversion from [Config] values to OCaml values} *)

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

      (** {6 Helpers for specifying default values of directives} *)

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
        (** The value is the given keyword. *)

      val keywords : string list -> t
        (** The value is one of the given keywords. *)

      val bool : t
        (** The value is a boolean. *)

      val int : t
        (** The value is an integer. *)

      val float : t
        (** The value is a float. *)

      val string : t
        (** The value is a string. *)

      val regexp : t
        (** The value is a regular expression. *)

      val bool_list : t
        (** The value is a list of boolean values. *)

      val int_list : t
        (** The value is a list of integer values. *)

      val float_list : t
        (** The value is a list of float values. *)

      val string_list : t
        (** The value is a list of string values. *)

      val int_in_range : int * int -> t
        (** The value is an integer in the inclusive range defined by the given
            tuple. *)

      val int_greater_than : int -> t
        (** The value is an integer greater than the given argument. *)

      val int_less_than : int -> t
        (** The value is an integer less than the given argument. *)

      val float_in_range : float * float -> t
        (** The value is a float in the inclusive range defined by the given
            tuple. *)

      val float_greater_than : float -> t
        (** The value is a float greater than the given argument.. *)

      val float_less_than : float -> t
        (** The value is a float less than the given argument. *)

      val string_matching : string -> t
        (** The value is a string matching the regular expression given as a
            string. The regular expression is created by calling [Str.regexp]
            on the first argument. *)

      val int_in : int list -> t
        (** The value is equal to one of the integers in the given list. *)

      val string_in : string list -> t
        (** The value is equal to one of the strings in the given list. *)

      val existing_file : t
        (** The value is an existing file. *)

      val nonempty_file : t
        (** The value is a file whose size is nonzero. *)

      val file_with_mode : Unix.file_perm -> t
        (** The value is a file with the given mode. *)

      val file_with_owner : string -> t
        (** The value is a file with the given owner. *)

      val file_with_group : string -> t
        (** The value is a file with the given group. *)

      val existing_directory : t
        (** The value is an existing directory. *)

      val existing_dirname : t
        (** The value is a path whose directory name (as in [dirname(3)])
            exists. *)

      val block_device : t
        (** The value is a block device. *)

      val character_device : t
        (** The value is a character device. *)

      val symbolic_link : t
        (** The value is a symbolic link. *)

      val named_pipe : t
        (** The value is a named pipe. *)

      val unix_socket : t
        (** The value is a unix socket. *)

      val existing_user : t
        (** The value is an exiting user. *)

      val unprivileged_user : t
        (** The value is a non-root user. *)

      val existing_group : t
        (** The value is an existing group. *)

      val list_of : t -> t
        (** The value is a list whose elements must pass the given
            validation. *)
    end

    type t
      (** The type of a configuration. *)

    type key = string * Value.t option * Validation.t list
      (** The type of a configuration key specification. A key is specified by
          its name, a possible default value and a list of validations. *)

    type section =
      [ `Global of key list
      | `Section of (string * key list)
      ]
      (** The type of a configuration section specification. A section
          specification contains a list of keys belonging to that section, and
          can either have a name or be global (in which case its keys will not
          appear under any section definitions in the configuration file). *)

    type spec = section list
      (** The type of configuration file specifications. *)

    val parse : string
             -> spec
             -> [`Configuration of t | `Error of string] future
      (** Parse a configuration file. [parse file spec] will try to parse
          [file] and, if it is syntatically correct, validate it against the
          specification given in [spec]. *)

    val defaults : spec -> t
      (** [defaults spec] returns a configuration built from the default values
          of [spec]. This only makes sense if every key in the specification
          has a default value. *)

    val get : t -> string -> string -> Value.t
      (** [get conf section key] returns the value of the parameter [key] of
          section [section] in configuration [conf]. *)

    val get_global : t -> string -> Value.t
      (** [get_global conf key] returns the value of the global parameter
          [key]. *)

  end

  (** Socket type definition and miscellaneous socket-related utility
      functions.
  *)
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
    (** Returns a thread that creates a socket of the given type, binds it to
        the given address and blocks listening for connections. When a new
        connection is established, the callback function is called in a handler
        thread. The default [backlog] value is 10 and the default [timeout] is
        10 seconds. *)
  end

  (** This module allows one to implement type-safe inter-process communication
      when using Release.

      The UNIX socket used by Release to implement IPC is a resource that can
      be shared by multiple threads. Therefore, if multiple threads in your
      program have access to the IPC file descriptor, it should be protected
      by a lock in order to ensure atomicity.

      A simple protocol is assumed. Each IPC message contains a 4-byte header
      followed by a variable length payload. The length of the payload is given
      by the 4-byte integer in the header, but must fit an OCaml [int].
      Therefore, in 32-bit architectures, an exception might be raises during
      header parsing.
  *)
  module IPC : sig
    (** The type of IPC connections. *)
    type connection

    (** The type of IPC handler functions. *)
    type handler = connection -> unit future

    val create_connection : ([`Active], Socket.unix) Socket.t -> connection
      (** Creates an IPC connection from an active unix socket. *)

    val control_socket : string -> handler -> unit future
      (** [control_socket path handler] creates UNIX domain socket at the
          specified path and sets up an accept loop thread that waits for
          connections on the socket. When a new connection is established,
          [handler] is called on a separate thread. *)

    module type Ops = sig
      type request
        (** The type of IPC requests. *)
      type response
        (** The type of IPC responses. *)

      val string_of_request : request -> string
        (** A function that converts a request into a string. *)
      val request_of_string : string -> request
        (** A function that converts a string into a request. *)

      val string_of_response : response -> string
        (** A function that converts a response into a string. *)
      val response_of_string : string -> response
        (** A function that converts a string into a response. *)
    end

    module type S = sig
      type request
        (** The type of IPC requests. *)
      type response
        (** The type of IPC responses. *)

      module Server : sig
        (** Module containing functions to be used by an IPC server, that is,
            the master process. *)

        val read_request : ?timeout:float
                        -> connection
                        -> [`Request of request | `EOF | `Timeout] future
          (** Reads an IPC request from a file descriptor. *)

        val write_response : connection -> response -> unit future
          (** Writes an IPC response to a file descriptor. *)

        val handle_request : ?timeout:float
                          -> ?eof_warning:bool
                          -> connection
                          -> (request -> response future)
                          -> unit future
          (** This function reads an IPC {!request} from a file descriptor and
              passes it to a callback function that must return an appropriate
              {!response}. *)
      end

      module Client : sig
        (** Module containing functions to be used by an IPC client, that is,
            a slave, helper or control process. *)

        val read_response : ?timeout:float
                         -> connection
                         -> [`Response of response | `EOF | `Timeout] future
          (** Reads an IPC response from a file descriptor. *)

        val write_request : connection -> request -> unit future
          (** Writes an IPC request to a file descriptor. *)

        val make_request : ?timeout:float
                        -> connection
                        -> request
                        -> ([`Response of response | `EOF | `Timeout] ->
                             'a future)
                        -> 'a future
          (** This function sends an IPC {!request} on a file descriptor and
              waits for a {!response}, passing it to a callback function
              responsible for handling it. *)
      end
    end

    module Make (O : Ops) : S
      with type request = O.request and type response = O.response
        (** Functor that builds an implementation of the IPC-handling functions
            given the request and response types and the appropriate string
            conversion functions. *)

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
         slave:(command * IPC.handler)
      -> ?background:bool
      -> ?logger:logger
      -> ?privileged:bool
      -> ?slave_env:[`Inherit | `Keep of string list]
      -> ?control:(string * IPC.handler)
      -> ?main:((unit -> (int * IPC.connection) list) -> unit future)
      -> lock_file:string
      -> unit -> unit
    (** Sets up a master process with one slave.

        [slave] is a tuple whose first element contains the {!command}
        associated to the slave process and second element is a callback
        function that is called in the master process to handle IPC requests
        from the slave (see {!IPC}).

        [background] indicates whether {!daemon} will be called. Defaults to
        [true].

        [logger] provides an optional logging facility for the master process.
        Defaults to a syslog logger.

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
        that returns the current list of sockets that connect the master
        process to the slave processes. This is useful for broadcast-style
        communication from the master to the slaves.

        [lock_file] is the path to the lock file created by the master process.
        This file contains the PID of the master process. If the file already
        exists and contains the PID of a running process, the master will
        refuse to start. *)

  val master_slaves :
         ?background:bool
      -> ?logger:logger
      -> ?privileged:bool
      -> ?slave_env:[`Inherit | `Keep of string list]
      -> ?control:(string * IPC.handler)
      -> ?main:((unit -> (int * IPC.connection) list) -> unit future)
      -> lock_file:string
      -> slaves:(command * IPC.handler * int) list
      -> unit -> unit
     (** This function generalizes {!master_slave}, taking the same arguments,
         except for [slave], which is substituted by [slaves]. This argument is
         a list of 3-element tuples. The first element of the tuple is the
         {!command} associated to the slave executable, the second element is
         the IPC handler for that slave and the third element is the number of
         instances to be created (i.e. the number of times the appropriate
         command will be run. *)

  val me : ?logger:logger
        -> ?user:string
        -> main:(IPC.connection -> unit future)
        -> unit -> unit
    (** This function is supposed to be called in the slave process.

        [logger] provides an optional logging facility for the slave process.
        Defaults to a syslog logger.

        [user], if present, indicates the name of the user the slave process
        will drop privileges to. Dropping privileges involves the following
        steps:

        + [chroot] to the users's home directory;
        + Change the current working directory to [/];
        + Call [setgroups] on the user's GID;
        + Call [setgid] and [setuid] on the user's GID and UID, respectively;
        + Check if privileges were successfully dropped.

        If [user] is not given, the slave process will run as the same user
        used to run the master process.

        [main] is a function that works as the entry point for the slave process
        code. The file descriptor given as an argument to [main] can be used by
        the slave for communication with the master. *)
end

module Make (Future : Release_future.S) : S
  with type 'a future := 'a Future.t
   and type fd := Future.Unix.fd
   and type logger := Future.Logger.t
   and type ('state, 'addr) Socket.t = ('state, 'addr) Future.Unix.socket
     (** Functor that builds a {!Release} implementation based on the given
         set of asynchronous operations represented by the [Future] argument
         module (see {!Release_future.S}). *)
