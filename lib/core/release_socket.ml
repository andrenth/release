module type S = sig
  type +'a future
  type file_descr

  val accept_loop : ?backlog:int
                 -> ?timeout:float
                 -> Unix.socket_type
                 -> Unix.sockaddr
                 -> (file_descr -> unit future)
                 -> unit future
end

module Make (Future: Release_future.S) : S
  with type 'a future = 'a Future.t
   and type file_descr = Future.Unix.file_descr =
struct
  open Future.Monad

  type +'a future = 'a Future.t
  type file_descr = Future.Unix.file_descr

  let accept_loop ?(backlog = 10) ?(timeout = 10.0) socktype sockaddr handler =
    let domain = Unix.domain_of_sockaddr sockaddr in
    let fd = Future.Unix.socket domain socktype 0 in
    (match sockaddr with
    | Unix.ADDR_UNIX s -> Future.Main.at_exit (fun () -> Future.Unix.unlink s)
    | Unix.ADDR_INET _ -> ());
    Future.Unix.setsockopt fd Unix.SO_REUSEADDR true;
    Future.Unix.bind fd sockaddr;
    Future.Unix.listen fd backlog;
    let rec accept () =
      Future.Unix.accept fd >>= fun (cli_fd, _) ->
      let timeout_t =
        Future.Unix.sleep timeout >>= fun () ->
        Future.Logger.warning_f "timeout on handler" in
      let handler_t =
        Future.catch
          (fun () ->
            handler cli_fd)
          (fun e ->
            let err = Printexc.to_string e in
            Future.Logger.error_f "accept handler exception: %s" err) in
      let close_fd () =
        Future.Unix.close cli_fd in
      ignore (
        Future.finalize (fun () -> Future.pick [timeout_t; handler_t]) close_fd
      );
      accept () in
    accept ()
end
