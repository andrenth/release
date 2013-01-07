module type Buffer = Release_buffer.S

module type Ops = sig
  type buffer
  val read_byte_at : int -> buffer -> int
  val read_byte : buffer -> int
  val write_byte : int -> buffer -> unit
end

module Make (B : Buffer) : Ops with type buffer = B.t = struct
  type buffer = B.t

  let read_byte_at i buf =
    int_of_char (B.get buf i)

  let read_byte = read_byte_at 0

  let write_byte b buf =
    B.add_char buf (char_of_int (b land 255))
end

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
  type buffer
  val read_at : int -> buffer -> t
  val write_byte : t -> buffer -> unit
  val write : t -> buffer -> unit
end

module MakeBig (B : Buffer) (I : Integer) : ByteOps
  with type t = I.t and type buffer = B.t =
struct
  module Bytes = Make (B)

  type t = I.t
  type buffer = B.t

  let bits = 8 * I.bytes

  let read_at i buf =
    let res = ref I.zero in
    for b = 1 to I.bytes do
      let pos = i + b - 1 in
      let byte = I.of_int (Bytes.read_byte_at pos buf) in
      res := I.logor !res (I.shift_left byte (bits - 8 * b))
    done;
    !res

  let write_byte b buf =
    let c = char_of_int (I.to_int (I.logand b I.byte_max)) in
    B.add_char buf c

  let write i buf =
    for b = I.bytes downto 1 do
      let shift = 8 * (b - 1) in
      write_byte (I.shift_right_logical i shift) buf
    done
end

module MakeLittle (B : Buffer) (I : Integer) : ByteOps
  with type t = I.t and type buffer = B.t =
struct
  module Bytes = Make (B)

  type t = I.t
  type buffer = B.t

  let bits = 8 * I.bytes

  let read_at i buf =
    let res = ref I.zero in
    for b = I.bytes downto 1 do
      let pos = i + b - 1 in
      let byte = I.of_int (Bytes.read_byte_at pos buf) in
      res := I.logor !res (I.shift_left byte (8 * (b - 1)))
    done;
    !res

  let write_byte b buf =
    let c = char_of_int (I.to_int (I.logand b I.byte_max)) in
    B.add_char buf c

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

module MakeInt16Big (B : Buffer) = MakeBig (B) (Int16)
module MakeIntBig (B : Buffer) = MakeBig (B) (Int)
module MakeInt32Big (B : Buffer) = MakeBig (B) (Int32)
module MakeUint32Big (B : Buffer) = MakeBig (B) (Uint32)
module MakeInt64Big (B : Buffer) = MakeBig (B) (Int64)
module MakeUint64Big (B : Buffer) = MakeBig (B) (Uint64)
module MakeUint128Big (B : Buffer) = MakeBig (B) (Uint128)

module type IntegerOps = sig
  module Make (I : Integer) : ByteOps with type t = I.t
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

module Big_endian (B : Buffer) = struct
  module Make = MakeBig (B)

  module Int16Big = MakeInt16Big (B)
  module IntBig = MakeIntBig (B)
  module Int32Big = MakeInt32Big (B)
  module Uint32Big = MakeUint32Big (B)
  module Int64Big = MakeInt64Big (B)
  module Uint64Big = MakeUint64Big (B)
  module Uint128Big = MakeUint128Big (B)

  type buffer = B.t

  let read_int16_at = Int16Big.read_at
  let read_int16 = read_int16_at 0
  let write_int16_byte = Int16Big.write_byte
  let write_int16 = Int16Big.write

  let read_int_at = IntBig.read_at
  let read_int = read_int_at 0
  let write_int_byte = IntBig.write_byte
  let write_int = IntBig.write

  let read_int32_at = Int32Big.read_at
  let read_int32 = read_int32_at 0
  let write_int32_byte = Int32Big.write_byte
  let write_int32 = Int32Big.write

  let read_uint32_at = Uint32Big.read_at
  let read_uint32 = read_uint32_at 0
  let write_uint32_byte = Uint32Big.write_byte
  let write_uint32 = Uint32Big.write

  let read_int64_at = Int64Big.read_at
  let read_int64 = read_int64_at 0
  let write_int64_byte = Int64Big.write_byte
  let write_int64 = Int64Big.write

  let read_uint64_at = Uint64Big.read_at
  let read_uint64 = read_uint64_at 0
  let write_uint64_byte = Uint64Big.write_byte
  let write_uint64 = Uint64Big.write

  let read_uint128_at = Uint128Big.read_at
  let read_uint128 = read_uint128_at 0
  let write_uint128_byte = Uint128Big.write_byte
  let write_uint128 = Uint128Big.write
end

module MakeInt16Little (B : Buffer) = MakeLittle (B) (Int16)
module MakeIntLittle (B : Buffer) = MakeLittle (B) (Int)
module MakeInt32Little (B : Buffer) = MakeLittle (B) (Int32)
module MakeUint32Little (B : Buffer) = MakeLittle (B) (Uint32)
module MakeInt64Little (B : Buffer) = MakeLittle (B) (Int64)
module MakeUint64Little (B : Buffer) = MakeLittle (B) (Uint64)
module MakeUint128Little (B : Buffer) = MakeLittle (B) (Uint128)

module Little_endian (B : Buffer) = struct
  module Make = MakeLittle (B)

  module Int16Little = MakeInt16Little (B)
  module IntLittle = MakeIntLittle (B)
  module Int32Little = MakeInt32Little (B)
  module Uint32Little = MakeUint32Little (B)
  module Int64Little = MakeInt64Little (B)
  module Uint64Little = MakeUint64Little (B)
  module Uint128Little = MakeUint128Little (B)

  type buffer = B.t

  let read_int16_at = Int16Little.read_at
  let read_int16 = read_int16_at 0
  let write_int16_byte = Int16Little.write_byte
  let write_int16 = Int16Little.write

  let read_int_at = IntLittle.read_at
  let read_int = read_int_at 0
  let write_int_byte = IntLittle.write_byte
  let write_int = IntLittle.write

  let read_int32_at = Int32Little.read_at
  let read_int32 = read_int32_at 0
  let write_int32_byte = Int32Little.write_byte
  let write_int32 = Int32Little.write

  let read_uint32_at = Uint32Little.read_at
  let read_uint32 = read_uint32_at 0
  let write_uint32_byte = Uint32Little.write_byte
  let write_uint32 = Uint32Little.write

  let read_int64_at = Int64Little.read_at
  let read_int64 = read_int64_at 0
  let write_int64_byte = Int64Little.write_byte
  let write_int64 = Int64Little.write

  let read_uint64_at = Uint64Little.read_at
  let read_uint64 = read_uint64_at 0
  let write_uint64_byte = Uint64Little.write_byte
  let write_uint64 = Uint64Little.write

  let read_uint128_at = Uint128Little.read_at
  let read_uint128 = read_uint128_at 0
  let write_uint128_byte = Uint128Little.write_byte
  let write_uint128 = Uint128Little.write
end

module Big_endian_bytes = Big_endian (Release_buffer.Bytes)
module Big_endian_string = Big_endian (Release_buffer.String)
module Little_endian_bytes = Little_endian (Release_buffer.Bytes)
module Little_endian_string = Little_endian (Release_buffer.String)
