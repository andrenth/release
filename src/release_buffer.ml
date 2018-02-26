open Lwt.Infix

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

let size buf =
  Lwt_bytes.length buf.bytes

let length buf =
  buf.len

let add_char buf c =
  set buf buf.len c

let add_bytes buf b =
  let len = Bytes.length b in
  Lwt_bytes.blit_from_bytes b 0 buf.bytes buf.len len;
  buf.len <- buf.len + len

let sub buf off len =
  let b = Lwt_bytes.proxy buf.bytes off len in
  { bytes = b; len = len }

let blit buf1 off1 buf2 off2 len =
  Lwt_bytes.blit buf1.bytes off1 buf2.bytes off2 len;
  buf2.len <- max buf2.len (off2 + len)

let contents buf =
  Lwt_bytes.to_string buf.bytes

let to_string buf =
  contents (sub buf 0 (length buf))

let of_string s =
  { bytes = Lwt_bytes.of_string s; len = String.length s }

let add_buffer buf1 buf2 =
  blit buf2 0 buf1 buf1.len buf2.len

let index_from buf base c =
  if base >= 0 && base < buf.len then
    let rec index i =
      if i < buf.len then
        if get buf i = c then
          Some i
        else
          index (i + 1)
      else
        None in
    index base
  else
    None

let index buf c =
  index_from buf 0 c

let read fd buf off len =
  Lwt_bytes.read fd buf.bytes off len >>= fun k ->
  buf.len <- max buf.len (off + k);
  Lwt.return k

let write fd buf off len =
  Lwt_bytes.write fd buf.bytes off len
