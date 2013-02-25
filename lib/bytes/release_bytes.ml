let read_byte_at i buf =
  int_of_char (Release_buffer.get buf i)

let read_byte = read_byte_at 0

let write_byte b buf =
  Release_buffer.add_char buf (char_of_int (b land 255))

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

module MakeBig (I : Integer) : ByteOps with type t = I.t = struct
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
    let c = char_of_int (I.to_int (I.logand b I.byte_max)) in
    Release_buffer.add_char buf c

  let write i buf =
    for b = I.bytes downto 1 do
      let shift = 8 * (b - 1) in
      write_byte (I.shift_right_logical i shift) buf
    done
end

module MakeLittle (I : Integer) : ByteOps with type t = I.t = struct
  type t = I.t

  let bits = 8 * I.bytes

  let read_at i buf =
    let res = ref I.zero in
    for b = I.bytes downto 1 do
      let pos = i + b - 1 in
      let byte = I.of_int (read_byte_at pos buf) in
      res := I.logor !res (I.shift_left byte (8 * (b - 1)))
    done;
    !res

  let write_byte b buf =
    let c = char_of_int (I.to_int (I.logand b I.byte_max)) in
    Release_buffer.add_char buf c

  let write i buf =
    for b = 1 to I.bytes do
      let shift = 8 * (b - 1) in
      write_byte (I.shift_right_logical i shift) buf
    done
end

let id x = x

module Int16 = struct
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
end

module Int = struct
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
end

module Int32 = struct
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
end

module Uint32 = struct
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
end

module Int64 = struct
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
end

module Uint64 = struct
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
end

module Uint128 = struct
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
end

module Int16BigOps = MakeBig(Int16)
module IntBigOps = MakeBig(Int)
module Int32BigOps = MakeBig(Int32)
module Uint32BigOps = MakeBig(Uint32)
module Int64BigOps = MakeBig(Int64)
module Uint64BigOps = MakeBig(Uint64)
module Uint128BigOps = MakeBig(Uint128)

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

module Big_endian = struct
  module Make = MakeBig

  let read_int16_at = Int16BigOps.read_at
  let read_int16 = read_int16_at 0
  let write_int16_byte = Int16BigOps.write_byte
  let write_int16 = Int16BigOps.write

  let read_int_at = IntBigOps.read_at
  let read_int = read_int_at 0
  let write_int_byte = IntBigOps.write_byte
  let write_int = IntBigOps.write

  let read_int32_at = Int32BigOps.read_at
  let read_int32 = read_int32_at 0
  let write_int32_byte = Int32BigOps.write_byte
  let write_int32 = Int32BigOps.write

  let read_uint32_at = Uint32BigOps.read_at
  let read_uint32 = read_uint32_at 0
  let write_uint32_byte = Uint32BigOps.write_byte
  let write_uint32 = Uint32BigOps.write

  let read_int64_at = Int64BigOps.read_at
  let read_int64 = read_int64_at 0
  let write_int64_byte = Int64BigOps.write_byte
  let write_int64 = Int64BigOps.write

  let read_uint64_at = Uint64BigOps.read_at
  let read_uint64 = read_uint64_at 0
  let write_uint64_byte = Uint64BigOps.write_byte
  let write_uint64 = Uint64BigOps.write

  let read_uint128_at = Uint128BigOps.read_at
  let read_uint128 = read_uint128_at 0
  let write_uint128_byte = Uint128BigOps.write_byte
  let write_uint128 = Uint128BigOps.write
end

module Int16LittleOps = MakeLittle(Int16)
module IntLittleOps = MakeLittle(Int)
module Int32LittleOps = MakeLittle(Int32)
module Uint32LittleOps = MakeLittle(Uint32)
module Int64LittleOps = MakeLittle(Int64)
module Uint64LittleOps = MakeLittle(Uint64)
module Uint128LittleOps = MakeLittle(Uint128)

module Little_endian = struct
  module Make = MakeLittle

  let read_int16_at = Int16LittleOps.read_at
  let read_int16 = read_int16_at 0
  let write_int16_byte = Int16LittleOps.write_byte
  let write_int16 = Int16LittleOps.write

  let read_int_at = IntLittleOps.read_at
  let read_int = read_int_at 0
  let write_int_byte = IntLittleOps.write_byte
  let write_int = IntLittleOps.write

  let read_int32_at = Int32LittleOps.read_at
  let read_int32 = read_int32_at 0
  let write_int32_byte = Int32LittleOps.write_byte
  let write_int32 = Int32LittleOps.write

  let read_uint32_at = Uint32LittleOps.read_at
  let read_uint32 = read_uint32_at 0
  let write_uint32_byte = Uint32LittleOps.write_byte
  let write_uint32 = Uint32LittleOps.write

  let read_int64_at = Int64LittleOps.read_at
  let read_int64 = read_int64_at 0
  let write_int64_byte = Int64LittleOps.write_byte
  let write_int64 = Int64LittleOps.write

  let read_uint64_at = Uint64LittleOps.read_at
  let read_uint64 = read_uint64_at 0
  let write_uint64_byte = Uint64LittleOps.write_byte
  let write_uint64 = Uint64LittleOps.write

  let read_uint128_at = Uint128LittleOps.read_at
  let read_uint128 = read_uint128_at 0
  let write_uint128_byte = Uint128LittleOps.write_byte
  let write_uint128 = Uint128LittleOps.write
end
