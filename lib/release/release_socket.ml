module type S = sig
  type +'a future

  type unix = [ `Unix of string ]
  type inet = [ `Inet of Unix.inet_addr * int ]
  type addr = [ unix | inet ]
  type ('state, 'addr) t
    constraint 'state = [< `Unconnected | `Bound | `Passive | `Active ]
    constraint 'addr  = [< addr ]

  val accept_loop : ?backlog:int
                 -> ?timeout:float
                 -> ([`Unconnected], 'addr) t
                 -> 'addr
                 -> (([`Active], 'addr) t -> unit future)
                 -> 'a future
end

module Make
  (Future: Release_future.S)
  (Logger : Release_logger.S
    with type 'a future := 'a Future.t
     and type t := Future.Logger.t) : S
  with type 'a future := 'a Future.t
   and type unix = Future.Unix.unix
   and type inet = Future.Unix.inet
   and type addr = Future.Unix.addr
   and type ('state, 'addr) t = ('state, 'addr) Future.Unix.socket =
struct
  open Future.Monad

  type unix = Future.Unix.unix
  type inet = Future.Unix.inet
  type addr = Future.Unix.addr
  type ('state, 'addr) t = ('state, 'addr) Future.Unix.socket
    constraint 'state = [< `Unconnected | `Bound | `Passive | `Active ]
    constraint 'addr  = [< addr ]

  let accept_loop ?(backlog = 10) ?(timeout = 10.0) fd addr handler =
    (match addr with
    | `Unix s -> Future.at_exit (fun () -> Future.Unix.unlink s)
    | `Inet _ -> ());
    Future.Unix.setsockopt fd Unix.SO_REUSEADDR true;
    Future.Unix.bind fd addr >>= fun fd ->
    let fd = Future.Unix.listen fd backlog in
    let rec loop () =
      Future.Unix.accept fd >>= fun (cli_fd, _) ->
      let handler_t =
        Future.catch
          (fun () ->
            handler cli_fd)
          (fun e ->
            let err = Printexc.to_string e in
            Logger.error "accept handler exception: %s" err) in
      ignore (
        Future.finalize
          (fun () ->
            Future.with_timeout timeout handler_t >>= function
            | `Result res -> return res
            | `Timeout -> Logger.error "timeout on handler")
          (fun () ->
            Future.Unix.close (Future.Unix.socket_fd cli_fd))
      );
      loop () in
    loop ()
end
