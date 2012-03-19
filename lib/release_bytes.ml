let read_byte_at i buf =
  int_of_char buf.[i]

let read_byte = read_byte_at 0

let write_byte b buf =
  Buffer.add_char buf (char_of_int (b land 255))

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
  val read_at : int -> string -> t
  val write_byte : t -> Buffer.t -> unit
  val write : t -> Buffer.t -> unit
end

module Make (I : Integer) : ByteOps with type t = I.t = struct
  type t = I.t

  let bits = 8 * I.bytes

  let read_at i buf =
    let res = ref I.zero in
    for b = 1 to I.bytes do
      let pos = i + b - 1 in
      let byte = I.of_int (read_byte_at pos buf) in
      res := I.logor !res (I.shift_left byte (bits - 8 * b))
    done;
    !res

  let write_byte b buf =
    Buffer.add_char buf (char_of_int (I.to_int (I.logand b I.byte_max)))

  let write i buf =
    for b = I.bytes downto 1 do
      let shift = 8 * (b - 1) in
      write_byte (I.shift_right_logical i shift) buf
    done
end

let id x = x

module Int16ByteOps = Make(struct
  type t = int
  let bytes = 2
  let zero = 0
  let byte_max = 255
  let shift_left = (lsl)
  let shift_right_logical = (lsr)
  let logor = (lor)
  let logand = (land)
  let of_int = id
  let to_int = id
end)

let read_int16_at = Int16ByteOps.read_at
let read_int16 = read_int16_at 0
let write_int16_byte = Int16ByteOps.write_byte
let write_int16 = Int16ByteOps.write

module IntByteOps = Make(struct
  type t = int
  let bytes = 4
  let zero = 0
  let byte_max = 255
  let shift_left = (lsl)
  let shift_right_logical = (lsr)
  let logor = (lor)
  let logand = (land)
  let of_int = id
  let to_int = id
end)

let read_int_at = IntByteOps.read_at
let read_int = read_int_at 0
let write_int_byte = IntByteOps.write_byte
let write_int = IntByteOps.write

module Int32ByteOps = Make(struct
  type t = Int32.t
  let bytes = 4
  let zero = Int32.zero
  let byte_max = Int32.of_int 255
  let shift_left = Int32.shift_left
  let shift_right_logical = Int32.shift_right_logical
  let logor = Int32.logor
  let logand = Int32.logand
  let of_int = Int32.of_int
  let to_int = Int32.to_int
end)

let read_int32_at = Int32ByteOps.read_at
let read_int32 = read_int32_at 0
let write_int32_byte = Int32ByteOps.write_byte
let write_int32 = Int32ByteOps.write

module Uint32ByteOps = Make(struct
  type t = Uint32.t
  let bytes = 4
  let zero = Uint32.zero
  let byte_max = Uint32.of_int 255
  let shift_left = Uint32.shift_left
  let shift_right_logical = Uint32.shift_right
  let logor = Uint32.logor
  let logand = Uint32.logand
  let of_int = Uint32.of_int
  let to_int = Uint32.to_int
end)

let read_uint32_at = Uint32ByteOps.read_at
let read_uint32 = read_uint32_at 0
let write_uint32_byte = Uint32ByteOps.write_byte
let write_uint32 = Uint32ByteOps.write

module Int64ByteOps = Make(struct
  type t = Int64.t
  let bytes = 8
  let zero = Int64.zero
  let byte_max = Int64.of_int 255
  let shift_left = Int64.shift_left
  let shift_right_logical = Int64.shift_right_logical
  let logor = Int64.logor
  let logand = Int64.logand
  let of_int = Int64.of_int
  let to_int = Int64.to_int
end)

let read_int64_at = Int64ByteOps.read_at
let read_int64 = read_int64_at 0
let write_int64_byte = Int64ByteOps.write_byte
let write_int64 = Int64ByteOps.write

module Uint64ByteOps = Make(struct
  type t = Uint64.t
  let bytes = 8
  let zero = Uint64.zero
  let byte_max = Uint64.of_int 255
  let shift_left = Uint64.shift_left
  let shift_right_logical = Uint64.shift_right
  let logor = Uint64.logor
  let logand = Uint64.logand
  let of_int = Uint64.of_int
  let to_int = Uint64.to_int
end)

let read_uint64_at = Uint64ByteOps.read_at
let read_uint64 = read_uint64_at 0
let write_uint64_byte = Uint64ByteOps.write_byte
let write_uint64 = Uint64ByteOps.write

module Uint128ByteOps = Make(struct
  type t = Uint128.t
  let bytes = 32
  let zero = Uint128.zero
  let byte_max = Uint128.of_int 255
  let shift_left = Uint128.shift_left
  let shift_right_logical = Uint128.shift_right
  let logor = Uint128.logor
  let logand = Uint128.logand
  let of_int = Uint128.of_int
  let to_int = Uint128.to_int
end)

let read_uint128_at = Uint128ByteOps.read_at
let read_uint128 = read_uint128_at 0
let write_uint128_byte = Uint128ByteOps.write_byte
let write_uint128 = Uint128ByteOps.write
