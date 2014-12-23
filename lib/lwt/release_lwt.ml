module Future : Release_future.S
  with type 'a t = 'a Lwt.t
   and type Unix.fd = Lwt_unix.file_descr =
struct
  type +'a t = 'a Lwt.t
  type +'a future = 'a t

  (* XXX *)
  let async = Lwt.async
  let catch = Lwt.catch
  let fail = Lwt.fail
  let idle () = fst (Lwt.wait ())
  let iter_p = Lwt_list.iter_p
  let join = Lwt.join
  let pick = Lwt.pick
  let finalize = Lwt.finalize

  module Monad = struct
    let (>>=) = Lwt.bind
    let return = Lwt.return
  end

  module IO = struct
    type input_channel = Lwt_io.input_channel
    type output_channel = Lwt_io.output_channel

    let fprintf = Lwt_io.fprintf
    let input = Lwt_io.input
    let output = Lwt_io.output
    let read_line = Lwt_io.read_line_opt
    let with_file mode path f = Lwt_io.with_file ~mode path f
    let with_input_file path f = Lwt_io.with_file ~mode:Lwt_io.input path f
    let with_output_file path f = Lwt_io.with_file ~mode:Lwt_io.output path f
  end

  module Mutex = struct
    type t = Lwt_mutex.t

    let create = Lwt_mutex.create
    let lock = Lwt_mutex.lock
    let unlock = Lwt_mutex.unlock
    let with_lock = Lwt_mutex.with_lock
  end

  module Main = struct
    let at_exit = Lwt_main.at_exit
    let run = Lwt_main.run
  end

  module Logger = struct
    let log_to_syslog () =
      Lwt_log.default := Lwt_log.syslog ~facility:`Daemon ()
    let debug s = Lwt_log.debug s
    let debug_f s = Lwt_log.debug_f s
    let info s = Lwt_log.info s
    let info_f s = Lwt_log.info_f s
    let error s = Lwt_log.error s
    let error_f s = Lwt_log.error_f s
  end

  module Unix = struct
    type fd = Lwt_unix.file_descr

    let (>>=) = Lwt.(>>=)
    let return = Lwt.return

    let accept_unix = Lwt_unix.accept
    let accept_inet = Lwt_unix.accept

    let bind sock addr =
      Lwt_unix.bind sock addr;
      return sock
    let chdir = Lwt_unix.chdir
    let chroot = Lwt_unix.chroot
    let close = Lwt_unix.close
    let dup fd = return (Lwt_unix.dup fd)
    let dup2 src dst = return (Lwt_unix.dup2 src dst)
    let exit = Pervasives.exit
    let fd_of_socket s = s
    let fork () =
      Lwt_io.flush_all () >>= fun () ->
      match Lwt_unix.fork () with
      | 0 ->
          return 0
      | pid ->
          Lwt_sequence.iter_node_l Lwt_sequence.remove Lwt_main.exit_hooks;
          return pid
    let getpwnam = Lwt_unix.getpwnam
    let listen sock backlog =
      Lwt_unix.listen sock backlog;
      sock
    let listen_unix = listen
    let listen_inet = listen
    let lstat = Lwt_unix.stat
    let on_signal signum handler = ignore (Lwt_unix.on_signal signum handler)
    let openfile = Lwt_unix.openfile
    let set_close_on_exec = Lwt_unix.set_close_on_exec
    let setsockopt_unix_bool = Lwt_unix.setsockopt
    let sleep = Lwt_unix.sleep
    let unix_socket () = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0
    let socketpair () = Lwt_unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0
    let stderr = Lwt_unix.stderr
    let stdin = Lwt_unix.stdin
    let stdout = Lwt_unix.stdout
    let unix_file_descr = Lwt_unix.unix_file_descr
    let unlink = Lwt_unix.unlink
    let waitpid pid =
      Lwt_unix.waitpid [] pid >>= fun (_, status) -> return status
  end

  module Bytes = struct
    type t = Lwt_bytes.t
    type fd = Lwt_unix.file_descr

    let blit = Lwt_bytes.blit
    let blit_string_bytes src src_pos dst dst_pos len =
      let b = Bytes.of_string src in
      Lwt_bytes.blit_from_bytes b src_pos dst dst_pos len
    let create = Lwt_bytes.create
    let fill = Lwt_bytes.fill
    let get = Lwt_bytes.get
    let length = Lwt_bytes.length
    let of_string = Lwt_bytes.of_string
    let proxy = Lwt_bytes.proxy
    let read = Lwt_bytes.read
    let set = Lwt_bytes.set
    let to_string = Lwt_bytes.to_string
    let write = Lwt_bytes.write
  end
end

module Release = Release.Make (Future)
