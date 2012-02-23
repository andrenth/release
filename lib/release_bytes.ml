let read_byte_at i buf =
  int_of_char buf.[i]

let read_byte = read_byte_at 0

let write_byte b buf =
  Buffer.add_char buf (char_of_int (b land 255))

let write_byte32 b buf =
  let (land) = Int32.logand in
  Buffer.add_char buf (char_of_int (Int32.to_int (b land 255l)))

let read_int16_at i buf =
  let b2 = read_byte_at i buf in
  let b1 = read_byte_at (i + 1) buf in
  (b2 lsl 8) lor b1

let read_int16 = read_int16_at 0

let write_int16 i buf =
  write_byte (i lsr 8) buf;
  write_byte i buf

let read_int_at i buf =
  let b4 = read_byte_at i buf in
  let b3 = read_byte_at (i + 1) buf in
  let b2 = read_byte_at (i + 2) buf in
  let b1 = read_byte_at (i + 3) buf in
  (b4 lsl 24) lor (b3 lsl 16) lor (b2 lsl 8) lor b1

let read_int = read_int_at 0

let write_int i buf =
  write_byte (i lsr 24) buf;
  write_byte (i lsr 16) buf;
  write_byte (i lsr 8) buf;
  write_byte i buf

let read_int32_at i buf =
  let (lsl) = Int32.shift_left in
  let (lor) = Int32.logor in
  let b4 = Int32.of_int (read_byte_at i buf) in
  let b3 = Int32.of_int (read_byte_at (i + 1) buf) in
  let b2 = Int32.of_int (read_byte_at (i + 2) buf) in
  let b1 = Int32.of_int (read_byte_at (i + 3) buf) in
  (b4 lsl 24) lor (b3 lsl 16) lor (b2 lsl 8) lor b1

let read_int32 = read_int32_at 0

let write_int32 i buf =
  let (lsr) = Int32.shift_right_logical in
  write_byte32 (i lsr 24) buf;
  write_byte32 (i lsr 16) buf;
  write_byte32 (i lsr 8) buf;
  write_byte32 i buf
