(** This module contains functions for easy and safe network I/O functions.
    The functions are safe in the sense that reads and writes are automatically
    retried when [Unix.EINTR] or [Unix.EAGAIN] errors are caught.
*)

val read : ?timeout:float
        -> Lwt_unix.file_descr
        -> int
        -> [`Data of string | `EOF | `Timeout] Lwt.t
   (** [read fd n] will read at most [n] bytes from file descriptor [fd].
       Data will be read until [n] bytes are read or an end-of-file condition
       occurs. An optional [timeout] argument may be given, in which case
       [read] is interrupted after the specified amount of seconds. *)

val write : Lwt_unix.file_descr -> string -> unit Lwt.t
  (** [write fd s] writes the full contents of [s] to file descriptor [fd]. *)
