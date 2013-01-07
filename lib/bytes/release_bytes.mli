(** This module contains functions for handling the binary representation of
    integers.

    Functions in the form [read_<t>_at] will return an integer of type [t] at
    the offset given as the first argument from the buffer given as the second
    argument.

    Functions in the form [read_<t>] are equivalent to [read_<t>_at 0].

    The [write_<t>] functions will append an integer of type [t] given as
    the first argument to the buffer given as the second argument.

    The [write_<t>_byte] functions do the same, but a single byte is
    appended.

    The functions are implemented in different modules depending on the
    desired endianess and underlying buffer implementation. Custom modules
    for different integer lengths can be built using the [Make] functor
    from each of the sub-modules in this module.
*)

module type Buffer = Release_buffer.S

module type Ops = sig
  type buffer
  val read_byte_at : int -> buffer -> int
  val read_byte : buffer -> int
  val write_byte : int -> buffer -> unit
end

module type Integer = sig
  (** The interface for creation of integer types. *)

  type t
    (** The type of the integer. *)

  val bytes : int
    (** The number of bytes required to represent the integer. *)

  val zero : t
    (** The number 0 as an instance of this integer type. *)

  val byte_max : t
    (** The number 255 as an instance of this integer type. *)

  val shift_left : t -> int -> t
    (** The shift-left operation. *)

  val shift_right_logical : t -> int -> t
    (** The shift-right logical operation. *)

  val logor : t -> t -> t
    (** The logical OR operation. *)

  val logand : t -> t -> t
    (** The logical AND operation. *)

  val of_int : int -> t
    (** Conversion from [int] to [t]. *)

  val to_int : t -> int
    (** Conversion from [t] to [int]. *)
end

module type ByteOps = sig
  type t
  type buffer
  val read_at : int -> buffer -> t
  val write_byte : t -> buffer -> unit
  val write : t -> buffer -> unit
end

module type IntegerOps = sig
  (** Operations on integers. *)

  module Make (I : Integer) : ByteOps with type t = I.t
    (** Functor for creation of integer operations based on the integer
        module given as a parameter. *)

  type buffer

  val read_int16_at : int -> buffer -> int
  val read_int16 : buffer -> int
  val write_int16_byte : int -> buffer -> unit
  val write_int16 : int -> buffer -> unit

  val read_int_at : int -> buffer -> int
  val read_int : buffer -> int
  val write_int_byte : int -> buffer -> unit
  val write_int : int -> buffer -> unit

  val read_int32_at : int -> buffer -> int32
  val read_int32 : buffer -> int32
  val write_int32_byte : int32 -> buffer -> unit
  val write_int32 : int32 -> buffer -> unit

  val read_uint32_at : int -> buffer -> Uint32.t
  val read_uint32 : buffer -> Uint32.t
  val write_uint32_byte : Uint32.t -> buffer -> unit
  val write_uint32 : Uint32.t -> buffer -> unit

  val read_int64_at : int -> buffer -> int64
  val read_int64 : buffer -> int64
  val write_int64_byte : int64 -> buffer -> unit
  val write_int64 : int64 -> buffer -> unit

  val read_uint64_at : int -> buffer -> Uint64.t
  val read_uint64 : buffer -> Uint64.t
  val write_uint64_byte : Uint64.t -> buffer -> unit
  val write_uint64 : Uint64.t -> buffer -> unit

  val read_uint128_at : int -> buffer -> Uint128.t
  val read_uint128 : buffer -> Uint128.t
  val write_uint128_byte : Uint128.t -> buffer -> unit
  val write_uint128 : Uint128.t -> buffer -> unit
end

module Big_endian (B : Buffer) : IntegerOps with type buffer = B.t
  (** Functor for creation of operations on big-endian integers on top
      of the given buffer module. *)

module Little_endian (B : Buffer) : IntegerOps with type buffer = B.t
  (** Functor for creation of operations on little-endian integers on top
      of the given buffer module. *)

module Big_endian_string : IntegerOps
  (** Module for big-endian operations implemented on top of
      [Release_buffer.String]. *)

module Big_endian_bytes : IntegerOps
  (** Module for big-endian operations implemented on top of
      [Release_buffer.Bytes]. *)

module Little_endian_string : IntegerOps
  (** Module for little-endian operations implemented on top of
      [Release_buffer.String]. *)

module Little_endian_bytes : IntegerOps
  (** Module for little-endian operations implemented on top of
      [Release_buffer.Bytes]. *)
