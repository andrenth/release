module type S = sig
  type t

  type +'a future
  type fd

  val create : int -> t
  val make : int -> char -> t
  val get : t -> int -> char
  val set : t -> int -> char -> unit
  val size : t -> int
  val length : t -> int
  val index : t -> char -> int option
  val index_from : t -> int -> char -> int option
  val add_char : t -> char -> unit
  val add_string : t -> string -> unit
  val add_buffer : t -> t -> unit
  val contents : t -> string
  val to_string : t -> string
  val of_string : string -> t
  val blit : t -> int -> t -> int -> int -> unit
  val sub : t -> int -> int -> t
  val read : fd -> t -> int -> int -> int future
  val write : fd -> t -> int -> int -> int future
end

module Make (Future : Release_future.S) : S
  with type 'a future := 'a Future.t
   and type fd := Future.Unix.fd =
struct
  open Future.Monad

  type t =
    { bytes       : Future.Bytes.t
    ; mutable len : int
    }

  let create n =
    { bytes = Future.Bytes.create n; len = 0 }

  let make n c =
    let buf = create n in
    Future.Bytes.fill buf.bytes 0 n c;
    buf.len <- n;
    buf

  let get buf i =
    Future.Bytes.get buf.bytes i

  let set buf i c =
    Future.Bytes.set buf.bytes i c;
    let len = i + 1 in
    if len > buf.len then
      buf.len <- len

  let size buf =
    Future.Bytes.length buf.bytes

  let length buf =
    buf.len

  let add_char buf c =
    set buf buf.len c

  let add_string buf s =
    let len = String.length s in
    Future.Bytes.blit_string_bytes s 0 buf.bytes buf.len len;
    buf.len <- buf.len + len

  let sub buf off len =
    let b = Future.Bytes.proxy buf.bytes off len in
    { bytes = b; len = len }

  let blit buf1 off1 buf2 off2 len =
    Future.Bytes.blit buf1.bytes off1 buf2.bytes off2 len;
    buf2.len <- max buf2.len (off2 + len)

  let contents buf =
    Future.Bytes.to_string buf.bytes

  let to_string buf =
    contents (sub buf 0 (length buf))

  let of_string s =
    { bytes = Future.Bytes.of_string s; len = String.length s }

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
    Future.Bytes.read fd buf.bytes off len >>= fun k ->
    buf.len <- max buf.len (off + k);
    return k

  let write fd buf off len =
    Future.Bytes.write fd buf.bytes off len
end
