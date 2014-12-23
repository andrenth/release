module type S = sig
  type +'a future
  type fd

  val accept_loop : ?backlog:int
                 -> ?timeout:float
                 -> fd
                 -> Unix.sockaddr
                 -> (fd -> unit future)
                 -> unit future
end

module Make (Future: Release_future.S) : S
  with type 'a future = 'a Future.t
   and type fd = Future.Unix.fd =
struct
  open Future.Monad

  type +'a future = 'a Future.t
  type fd = Future.Unix.fd

  let accept_loop ?(backlog = 10) ?(timeout = 10.0) fd sockaddr handler =
    let accept, listen =
      match sockaddr with
      | Unix.ADDR_UNIX s ->
          Future.Main.at_exit (fun () -> Future.Unix.unlink s);
          Future.Unix.accept_unix, Future.Unix.listen_unix
      | Unix.ADDR_INET _ ->
          Future.Unix.accept_inet, Future.Unix.listen_inet in
    Future.Unix.setsockopt_unix_bool fd Unix.SO_REUSEADDR true;
    Future.Unix.bind fd sockaddr >>= fun fd ->
    let fd = listen fd backlog in
    let rec loop () =
      accept fd >>= fun (cli_fd, _) ->
      let timeout_t =
        Future.Unix.sleep timeout >>= fun () ->
        Future.Logger.error "timeout on handler" in
      let handler_t =
        Future.catch
          (fun () ->
            handler cli_fd)
          (fun e ->
            let err = Printexc.to_string e in
            Future.Logger.error_f "accept handler exception: %s" err) in
      ignore (
        Future.finalize
          (fun () -> Future.pick [timeout_t; handler_t])
          (fun () -> Future.Unix.close (cli_fd))
      );
      loop () in
    loop ()
end
