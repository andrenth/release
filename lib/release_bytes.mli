val read_byte_at : int -> string -> int
val read_byte : string -> int
val write_byte : int -> Buffer.t -> unit

val read_int16_at : int -> string -> int
val read_int16 : string -> int
val write_int16_byte : int -> Buffer.t -> unit
val write_int16 : int -> Buffer.t -> unit

val read_int_at : int -> string -> int
val read_int : string -> int
val write_int_byte : int -> Buffer.t -> unit
val write_int : int -> Buffer.t -> unit

val read_int32_at : int -> string -> int32
val read_int32 : string -> int32
val write_int32_byte : int32 -> Buffer.t -> unit
val write_int32 : int32 -> Buffer.t -> unit

val read_uint32_at : int -> string -> Uint32.t
val read_uint32 : string -> Uint32.t
val write_uint32_byte : Uint32.t -> Buffer.t -> unit
val write_uint32 : Uint32.t -> Buffer.t -> unit

val read_int64_at : int -> string -> int64
val read_int64 : string -> int64
val write_int64_byte : int64 -> Buffer.t -> unit
val write_int64 : int64 -> Buffer.t -> unit

val read_uint64_at : int -> string -> Uint64.t
val read_uint64 : string -> Uint64.t
val write_uint64_byte : Uint64.t -> Buffer.t -> unit
val write_uint64 : Uint64.t -> Buffer.t -> unit

val read_uint128_at : int -> string -> Uint128.t
val read_uint128 : string -> Uint128.t
val write_uint128_byte : Uint128.t -> Buffer.t -> unit
val write_uint128 : Uint128.t -> Buffer.t -> unit
