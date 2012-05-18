open Lwt

type t =
  { bytes       : Lwt_bytes.t
  ; mutable len : int
  }

let create n =
  { bytes = Lwt_bytes.create n; len = 0 }

let make n c =
  let buf = create n in
  Lwt_bytes.fill buf.bytes 0 n c;
  buf.len <- n;
  buf

let get buf i =
  Lwt_bytes.get buf.bytes i

let set buf i c =
  Lwt_bytes.set buf.bytes i c;
  let len = i + 1 in
  if len > buf.len then
    buf.len <- len

let add_char buf c =
  set buf buf.len c

let add_string buf s =
  let len = String.length s in
  Lwt_bytes.blit_string_bytes s 0 buf.bytes buf.len len;
  buf.len <- buf.len + len

let of_string s =
  { bytes = Lwt_bytes.of_string s; len = String.length s }

let to_string buf =
  Lwt_bytes.to_string buf.bytes

let size buf =
  Lwt_bytes.length buf.bytes

let length buf =
  buf.len

let blit buf1 off1 buf2 off2 len =
  Lwt_bytes.blit buf1.bytes off1 buf2.bytes off2 len;
  buf2.len <- max buf2.len (off2 + len)

let add_buffer buf1 buf2 =
  blit buf2 0 buf1 0 buf2.len

let sub buf off len =
  let b = Lwt_bytes.proxy buf.bytes off len in
  { bytes = b; len = len }

let read fd buf off len =
  Lwt_bytes.read fd buf.bytes off len

let write fd buf off len =
  lwt n = Lwt_bytes.write fd buf.bytes off len in
  buf.len <- buf.len + n;
  return n
