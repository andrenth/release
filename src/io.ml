open Lwt.Infix
open Util

module B = Release_buffer

let rec interrupt_safe f =
  Lwt.catch f
    (function
    | Unix.Unix_error (Unix.EINTR, _, _) -> interrupt_safe f
    | Unix.Unix_error (Unix.EAGAIN, _, _) -> interrupt_safe f
    | e -> Lwt.fail e)

let eintr_safe op fd buf offset remain =
  interrupt_safe (fun () -> op fd buf offset remain)

let read_once fd buf offset n =
  eintr_safe B.read fd buf offset n

let read ?(timeout) fd n =
  let rec read_into buf offset remain =
    if remain = 0 then
      Lwt.return offset
    else
      read_once fd buf offset remain >>= fun k ->
      read_into buf (offset + k) (if k = 0 then 0 else remain - k) in
  let handle_read () =
    let buf = B.create n in
    read_into buf 0 n >>= function
    | 0 -> Lwt.return `EOF
    | k -> Lwt.return (`Data (if k = n then buf else B.sub buf 0 k)) in
  let read_with_timeout t =
    let timeout_t =
      Lwt_unix.sleep t >>= fun () ->
      Lwt.return `Timeout in
    Lwt.pick [timeout_t; handle_read ()] in
  Option.either handle_read read_with_timeout timeout

let write fd buf =
  let rec write offset remain =
    if remain = 0 then
      Lwt.return_unit
    else
      eintr_safe B.write fd buf offset remain >>= fun k ->
      write (offset + k) (if k = 0 then 0 else remain - k) in
  write 0 (B.length buf)
