(** This module defines a buffer type [Release_buffer.t] and a set of
    operations on buffers. [Release_buffer.t] is based on [Lwt_buffer.t]
    which on its turn is implemented using OCaml's [Bigarray] module.
*)

type t
  (** The type of buffers. *)

val create : int -> t
  (** [create n] will create a buffer of [n] bytes. *)

val make : int -> char -> t
  (** [make n c] will create a buffer of [n] bytes, all initialized to [c]. *)

val get : t -> int -> char
  (** [get buf off] returns the byte at offset [off] in [buf]. *)

val set : t -> int -> char -> unit
  (** [set buf off c] sets the byte at offset [off] in [buf] to [c]. *)

val size : t -> int
  (** [size buf] returns the amount of bytes currently allocated for [buf]. *)

val length : t -> int
  (** [length buf] returns the offset of the byte set at the highest position
      in [buf], plus [1]. This means that a mostly empty buffer with a single
      byte set at offset [off] is considered to have length [off+1]. *)

val index : t -> char -> int option
  (** [index buf c] returns [Some off], where [off] is the offset of the first
      occurrence of [c] in [buf], or [None] in case [c] does not occur in
      [buf]. *)

val index_from : t -> int -> char -> int option
  (** [index_from buf i c] returns [Some off], where [off] is the offset of
      the first occurrence of [c] in [buf], starting from offset [i], or [None]
      in case [c] does not occur in [buf] at or after [i]. If [i] is not in
      the [\[0, length buf)] range, also returns [None]. *)

val add_char : t -> char -> unit
  (** [add_char buf c] appends character [c] at the end of [buf]. *)

val add_string : t -> string -> unit
  (** [add_string buf s] appends string [s] at the end of [buf]. *)

val add_buffer : t -> t -> unit
  (** [add_buffer buf1 buf2] appends buffer [buf2] at the end of [buf1]. *)

val of_string : string -> t
  (** Converts a string to a buffer. *)

val to_string : t -> string
  (** Converts a buffer to a string. *)

val blit : t -> int -> t -> int -> int -> unit
  (** [blit buf1 off1 buf2 off2 len] copies [len] bytes from [buf1], starting
      at offset [off1], to [buf2], starting at offset [off2]. *)

val sub : t -> int -> int -> t
  (** [sub buf off len] returns a buffer consisting of [len] bytes from [buf]
      starting at offset [off]. No copying is made. *)

val read : Lwt_unix.file_descr -> t -> int -> int -> int Lwt.t
  (** [read fd buf off n] reads at most [n] bytes from file descriptor [fd]
      into buffer [buf], which is filled starting at offset [off]. Returns
      the number of bytes actually read, and 0 on EOF. *)

val write : Lwt_unix.file_descr -> t -> int -> int -> int Lwt.t
  (** [write fd buf off n] writes at most [n] bytes from [buf] starting at
      offset [off] into file descriptor [fd]. Returns the number of bytes
      actually written. *)
