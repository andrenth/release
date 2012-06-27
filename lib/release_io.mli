(** This module contains functions for easy and safe network I/O functions.
    The functions are safe in the sense that reads and writes are automatically
    retried when [Unix.EINTR] or [Unix.EAGAIN] errors are caught.
*)

val read_once : Lwt_unix.file_descr
             -> Release_buffer.t
             -> int
             -> int
             -> int Lwt.t
  (** [read_once fd buf off n] reads at most [n] bytes from file
      descriptor [fd] into buffer [buf], starting at offset [off] in the
      buffer. The actual number of bytes read is returned. *)

val read : ?timeout:float
        -> Lwt_unix.file_descr
        -> int
        -> [`Data of Release_buffer.t | `EOF | `Timeout] Lwt.t
   (** [read fd n] will read at most [n] bytes from file descriptor [fd].
       Data will be read until [n] bytes are read or an end-of-file condition
       occurs. An optional [timeout] argument may be given, in which case
       [read] is interrupted after the specified amount of seconds. *)

val write : Lwt_unix.file_descr -> Release_buffer.t -> unit Lwt.t
  (** [write fd s] writes the full contents of [s] to file descriptor [fd]. *)
