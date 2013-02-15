open Lwt

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
  eintr_safe Release_buffer.read fd buf offset n

let read ?(timeout) fd n =
  let rec read_into buf offset remain =
    if remain = 0 then
      return offset
    else
      lwt k = read_once fd buf offset remain in
      read_into buf (offset + k) (if k = 0 then 0 else remain - k) in
  let handle_read () =
    let buf = Release_buffer.create n in
    match_lwt read_into buf 0 n with
    | 0 -> return `EOF
    | k -> return (`Data (if k = n then buf else Release_buffer.sub buf 0 k)) in
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
      lwt k = eintr_safe Release_buffer.write fd buf offset remain in
      write (offset + k) (if k = 0 then 0 else remain - k) in
  write 0 (Release_buffer.length buf)
