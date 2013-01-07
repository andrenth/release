(** This module contains functions for easy and safe network I/O functions.
    The functions are safe in the sense that reads and writes are automatically
    retried when [Unix.EINTR] or [Unix.EAGAIN] errors are caught.

    The I/O functionality is provided in two submodules, [Release_io.String]
    and [Release_io.Bytes], each based on the respective [Release_buffer]
    implementation. A functor is also provided to allow the usage of a custom
    buffer type.
*)

module type S = sig
  type buffer

  val read_once : Lwt_unix.file_descr
               -> buffer
               -> int
               -> int
               -> int Lwt.t
    (** [read_once fd buf off n] reads at most [n] bytes from file
        descriptor [fd] into buffer [buf], starting at offset [off] in the
        buffer. The actual number of bytes read is returned. *)

  val read : ?timeout:float
          -> Lwt_unix.file_descr
          -> int
          -> [`Data of buffer | `EOF | `Timeout] Lwt.t
     (** [read fd n] will try to read [n] bytes from file descriptor [fd].
         Data will be read until [n] bytes are read or an end-of-file condition
         occurs. An optional [timeout] argument may be given, in which case
         [read] is interrupted after the specified amount of seconds. *)

  val write : Lwt_unix.file_descr -> buffer -> unit Lwt.t
    (** [write fd s] writes the full contents of [s] to file descriptor [fd]. *)
end

module Make (B : Release_buffer.S) : S with type buffer = B.t
  (** Functor to generate I/O functions from a custom buffer type. *)

module String : S
  (** Module implementing I/O functions based on [Release_buffer.String.t]. *)

module Bytes : S
  (** Module implementing I/O functions based on [Release_buffer.Bytes.t]. *)
