open Lwt

let rec interrupt_safe f =
  try_lwt
    f ()
  with
  | Unix.Unix_error (Unix.EINTR, _, _) -> interrupt_safe f
  | Unix.Unix_error (Unix.EAGAIN, _, _) -> interrupt_safe f
  | e -> raise_lwt e

let eintr_safe op fd buf offset remain =
  interrupt_safe (fun () -> op fd buf offset remain)

let read ?(timeout) fd n =
  let buf = String.create n in
  let rec read offset remain =
    if remain = 0 then
      return offset
    else
      lwt k = eintr_safe Lwt_unix.read fd buf offset remain in
      read (offset + k) (if k = 0 then 0 else remain - k) in
  let timeout = match timeout with None -> infinity | Some t -> t in
  let timeout_t = (* XXX doesn't this raise??? *)
    lwt () = Lwt_unix.timeout timeout in
    return `Timeout in
  let read_t =
    match_lwt read 0 n with
    | 0 -> return `EOF
    | k -> return (`Data (String.sub buf 0 k)) in
  timeout_t <?> read_t

let write fd buf =
  let len = String.length buf in
  let rec write offset remain =
    if remain = 0 then
      return ()
    else
      lwt k = eintr_safe Lwt_unix.write fd buf offset remain in
      write (offset + k) (if k = 0 then 0 else remain - k) in
  write 0 len
