module type S = sig
  type +'a t
  type +'a future = 'a t
  type 'a u

  (* XXX *)
  val catch : (unit -> 'a t) -> (exn -> 'a t) -> 'a t
  val fail : exn -> 'a t
  val wait : unit -> 'a t * 'a u
  val iter_p : ('a -> unit t) -> 'a list -> unit t
  val async : (unit -> 'a t) ->  unit
  val join : unit t list -> unit t
  val pick : 'a t list -> 'a t
  val finalize : (unit -> 'a t) -> (unit -> unit t) -> 'a t

  module Monad : sig
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val return : 'a -> 'a t
  end

  module Process : sig
    type command = string * string array
    type redirection =
      [ `Close
      | `Dev_null
      | `FD_copy of Unix.file_descr
      | `FD_move of Unix.file_descr
      | `Keep
      ]
    type resource_usage
    type state

    class process_none :
      ?timeout : float ->
      ?env : string array ->
      ?stdin : redirection ->
      ?stdout : redirection ->
      ?stderr : redirection ->
      command ->
    object
      method pid : int
      method state : state
      method kill : int -> unit
      method terminate : unit
      method status : Unix.process_status future
      method rusage : resource_usage future
      method close : Unix.process_status future
    end

    val with_process_none : ?timeout:float -> ?env:string array
                         -> ?stdin:redirection -> ?stdout:redirection
                         -> ?stderr:redirection -> command
                         -> (process_none -> 'a future) -> 'a future
  end

  module Mutex : sig
    type t

    val create : unit -> t
    val lock : t -> unit future
    val unlock : t -> unit
    val with_lock : t -> (unit -> 'a future) -> 'a future
  end

  module Main : sig
    type 'a sequence

    val at_exit : (unit -> unit future) -> unit
    val exit_hooks : (unit -> unit future) sequence
    val run : 'a future -> 'a
  end

  module Logger : sig
    type t

    type syslog_facility =
      [ `Auth
      | `Authpriv
      | `Console
      | `Cron
      | `Daemon
      | `FTP
      | `Kernel
      | `LPR
      | `Local0
      | `Local1
      | `Local2
      | `Local3
      | `Local4
      | `Local5
      | `Local6
      | `Local7
      | `Mail
      | `NTP
      | `News
      | `Security
      | `Syslog
      | `UUCP
      | `User ]

    val default : t ref
    val error : string -> unit future
    val error_f : ('a, unit, string, unit future) format4 -> 'a
    val notice : string -> unit future
    val notice_f : ('a, unit, string, unit future) format4 -> 'a
    val warning : string -> unit future
    val warning_f : ('a, unit, string, unit future) format4 -> 'a
    val syslog : syslog_facility -> t
  end

  module IO : sig
    type input
    type output
    type 'a mode
    type 'a channel

    val flush_all : unit -> unit future
    val fprintf : output channel -> ('a, unit, string, unit future) format4
               -> 'a
    val input : input mode
    val output : output mode
    val read_line_opt : input channel -> string option future
    val with_file : 'a mode -> string -> ('a channel -> 'b future)
                 -> 'b future
  end

  module Unix : sig
    type file_descr
    type signal_handler_id

    val accept : file_descr -> (file_descr * Unix.sockaddr) future
    val bind : file_descr -> Unix.sockaddr -> unit
    val chdir : string -> unit future
    val chroot : string -> unit future
    val close : file_descr -> unit future
    val dup : file_descr -> file_descr
    val dup2 : file_descr -> file_descr -> unit
    val fork : unit -> int future
    val getpwnam : string -> Unix.passwd_entry future
    val listen : file_descr -> int -> unit
    val lstat : string -> Unix.stats future
    val on_signal : int -> (int -> unit) -> signal_handler_id
    val openfile : string -> Unix.open_flag list -> Unix.file_perm
                -> file_descr future
    val set_close_on_exec : file_descr -> unit
    val setsockopt : file_descr -> Unix.socket_bool_option -> bool -> unit
    val sleep : float -> unit future
    val socket : Unix.socket_domain -> Unix.socket_type -> int -> file_descr
    val socketpair : Unix.socket_domain -> Unix.socket_type -> int
                  -> file_descr * file_descr
    val stderr : file_descr
    val stdin : file_descr
    val stdout : file_descr
    val unix_file_descr : file_descr -> Unix.file_descr
    val unlink : string -> unit future
  end

  module Bytes : sig
    type t
    type file_descr = Unix.file_descr

    val blit : t -> int -> t -> int -> int -> unit
    val blit_string_bytes : string -> int -> t -> int -> int -> unit
    val create : int -> t
    val fill : t -> int -> int -> char -> unit
    val get : t -> int -> char
    val length : t -> int
    val of_string : string -> t
    val proxy : t -> int -> int -> t
    val read : file_descr -> t -> int -> int -> int future
    val set : t -> int -> char -> unit
    val to_string : t -> string
    val write : file_descr -> t -> int -> int -> int future
  end
end
