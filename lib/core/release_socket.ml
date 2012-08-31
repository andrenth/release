open Lwt

let accept_loop ?(backlog = 10) ?(timeout = 10.0) socktype sockaddr handler =
  let domain = Unix.domain_of_sockaddr sockaddr in
  let fd = Lwt_unix.socket domain socktype 0 in
  (match sockaddr with
  | Lwt_unix.ADDR_UNIX s -> Lwt_main.at_exit (fun () -> Lwt_unix.unlink s)
  | Lwt_unix.ADDR_INET _ -> ());
  Lwt_unix.setsockopt fd Lwt_unix.SO_REUSEADDR true;
  Lwt_unix.bind fd sockaddr;
  Lwt_unix.listen fd backlog;
  let rec accept () =
    lwt cli_fd, _ = Lwt_unix.accept fd in
    let timeout_t =
      lwt () = Lwt_unix.sleep timeout in
      Lwt_log.warning_f "timeout on handler" in
    let handler_t =
      try_lwt
        handler cli_fd
      with e ->
        let err = Printexc.to_string e in
        Lwt_log.error_f "accept handler exception: %s" err in
    let close_fd () =
      Lwt_unix.close cli_fd in
    ignore (finalize (fun () -> Lwt.pick [timeout_t; handler_t]) close_fd);
    accept () in
  accept ()
