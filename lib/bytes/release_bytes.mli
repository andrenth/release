(** This module contains functions for handling the binary representation of
    integers.

    Functions in the form [read_<t>_at] will return an integer of type [t] at
    the offset given as the first argument from the Release_buffer.t given as
    the second argument.

    Functions in the form [read_<t>] are equivalent to [read_<t>_at 0].

    The [write_<t>] functions will append an integer of type [t] given as
    the first argument to the buffer given as the second argument.

    The [write_<t>_byte] functions do the same, but a single byte is
    appended.
*)

val read_byte_at : int -> Release_buffer.t -> int
val read_byte : Release_buffer.t -> int
val write_byte : int -> Release_buffer.t -> unit

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
  val read_at : int -> Release_buffer.t -> t
  val write_byte : t -> Release_buffer.t -> unit
  val write : t -> Release_buffer.t -> unit
end

module type IntegerOps = sig
  module Make (I : Integer) : ByteOps with type t = I.t

  val read_int16_at : int -> Release_buffer.t -> int
  val read_int16 : Release_buffer.t -> int
  val write_int16_byte : int -> Release_buffer.t -> unit
  val write_int16 : int -> Release_buffer.t -> unit

  val read_int_at : int -> Release_buffer.t -> int
  val read_int : Release_buffer.t -> int
  val write_int_byte : int -> Release_buffer.t -> unit
  val write_int : int -> Release_buffer.t -> unit

  val read_int32_at : int -> Release_buffer.t -> int32
  val read_int32 : Release_buffer.t -> int32
  val write_int32_byte : int32 -> Release_buffer.t -> unit
  val write_int32 : int32 -> Release_buffer.t -> unit

  val read_uint32_at : int -> Release_buffer.t -> Uint32.t
  val read_uint32 : Release_buffer.t -> Uint32.t
  val write_uint32_byte : Uint32.t -> Release_buffer.t -> unit
  val write_uint32 : Uint32.t -> Release_buffer.t -> unit

  val read_int64_at : int -> Release_buffer.t -> int64
  val read_int64 : Release_buffer.t -> int64
  val write_int64_byte : int64 -> Release_buffer.t -> unit
  val write_int64 : int64 -> Release_buffer.t -> unit

  val read_uint64_at : int -> Release_buffer.t -> Uint64.t
  val read_uint64 : Release_buffer.t -> Uint64.t
  val write_uint64_byte : Uint64.t -> Release_buffer.t -> unit
  val write_uint64 : Uint64.t -> Release_buffer.t -> unit

  val read_uint128_at : int -> Release_buffer.t -> Uint128.t
  val read_uint128 : Release_buffer.t -> Uint128.t
  val write_uint128_byte : Uint128.t -> Release_buffer.t -> unit
  val write_uint128 : Uint128.t -> Release_buffer.t -> unit
end

module Big_endian : IntegerOps
module Little_endian : IntegerOps
