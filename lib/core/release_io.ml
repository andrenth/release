module type S = sig
  type +'a future
  type buffer
  type fd

  val read_once : fd
               -> buffer
               -> int
               -> int
               -> int future

  val read : ?timeout:float
          -> fd
          -> int
          -> [`Data of buffer | `EOF | `Timeout] future

  val write : fd -> buffer -> unit future
end

module Make
  (Future : Release_future.S)
  (Buffer : Release_buffer.S
    with type 'a future := 'a Future.t
     and type fd := Future.Unix.fd) : S
  with type 'a future := 'a Future.t
   and type buffer := Buffer.t
   and type fd := Future.Unix.fd =
struct
  open Future.Monad

  module Option = Release_util.Option

  (* XXX *)
  let return_unit = return ()

  let rec interrupt_safe f =
    Future.catch f
      (function
      | Unix.Unix_error (Unix.EINTR, _, _) -> interrupt_safe f
      | Unix.Unix_error (Unix.EAGAIN, _, _) -> interrupt_safe f
      | e -> Future.fail e)

  let eintr_safe op fd buf offset remain =
    interrupt_safe (fun () -> op fd buf offset remain)

  let read_once fd buf offset n =
    eintr_safe Buffer.read fd buf offset n

  let read ?(timeout) fd n =
    let rec read_into buf offset remain =
      if remain = 0 then
        return offset
      else
        read_once fd buf offset remain >>= fun k ->
        read_into buf (offset + k) (if k = 0 then 0 else remain - k) in
    let handle_read () =
      let buf = Buffer.create n in
      read_into buf 0 n >>= function
      | 0 -> return `EOF
      | k -> return (`Data (if k = n then buf else Buffer.sub buf 0 k)) in
    let read_with_timeout t =
      Future.with_timeout t (handle_read ()) >>= function
      | `Result res -> return res
      | `Timeout -> return `Timeout in
    Option.either handle_read read_with_timeout timeout

  let write fd buf =
    let rec write offset remain =
      if remain = 0 then
        return_unit
      else
        eintr_safe Buffer.write fd buf offset remain >>= fun k ->
        write (offset + k) (if k = 0 then 0 else remain - k) in
    write 0 (Buffer.length buf)
end
