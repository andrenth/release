open Lwt

module type S = sig
  type buffer

  val read_once : Lwt_unix.file_descr
               -> buffer
               -> int
               -> int
               -> int Lwt.t
    (** [read_once fd buf off n] reads at most [n] bytes from file
        descriptor [fd] into buffer [buf], starting at offset [off] in the
        buffer. The actual number of bytes read is returned. *)

  val read : ?timeout:float
          -> Lwt_unix.file_descr
          -> int
          -> [`Data of buffer | `EOF | `Timeout] Lwt.t
     (** [read fd n] will try to read [n] bytes from file descriptor [fd].
         Data will be read until [n] bytes are read or an end-of-file condition
         occurs. An optional [timeout] argument may be given, in which case
         [read] is interrupted after the specified amount of seconds. *)

  val write : Lwt_unix.file_descr -> buffer -> unit Lwt.t
    (** [write fd s] writes the full contents of [s] to file descriptor [fd]. *)
end

module Make (B : Release_buffer.S) : S with type buffer = B.t = struct
  type buffer = B.t

  module O = Release_util.Option

  let rec interrupt_safe f =
    try_lwt
      f ()
    with
    | Unix.Unix_error (Unix.EINTR, _, _) -> interrupt_safe f
    | Unix.Unix_error (Unix.EAGAIN, _, _) -> interrupt_safe f
    | e -> raise_lwt e

  let eintr_safe op fd buf offset remain =
    interrupt_safe (fun () -> op fd buf offset remain)

  let read_once fd buf offset n =
    eintr_safe B.read fd buf offset n

  let read ?(timeout) fd n =
    let rec read_into buf offset remain =
      if remain = 0 then
        return offset
      else
        lwt k = read_once fd buf offset remain in
        read_into buf (offset + k) (if k = 0 then 0 else remain - k) in
    let handle_read () =
      let buf = B.create n in
      match_lwt read_into buf 0 n with
      | 0 -> return `EOF
      | k -> return (`Data (B.sub buf 0 k)) in
    let read_with_timeout t =
      let timeout_t =
        lwt () = Lwt_unix.sleep t in
        return `Timeout in
      Lwt.pick [timeout_t; handle_read ()] in
    O.either handle_read read_with_timeout timeout

  let write fd buf =
    let rec write offset remain =
      if remain = 0 then
        return_unit
      else
        lwt k = eintr_safe B.write fd buf offset remain in
        write (offset + k) (if k = 0 then 0 else remain - k) in
    write 0 (B.length buf)
end

module Bytes = Make (Release_buffer.Bytes)
module String = Make (Release_buffer.String)
