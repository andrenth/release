open Lwt

module Future : Release_future.S
  with type 'a t = 'a Lwt.t
   and type Unix.fd = Lwt_unix.file_descr
   and type Logger.t = Lwt_log.logger
   and type ('state, 'addr) Unix.socket = Lwt_unix.file_descr =
struct
  type +'a t = 'a Lwt.t
  type +'a future = 'a t

  let name = "lwt"

  let async = Lwt.async
  let at_exit = Lwt_main.at_exit
  let catch = Lwt.catch
  let fail = Lwt.fail
  let finalize = Lwt.finalize
  let idle () = fst (Lwt.wait ())
  let iter_p = Lwt_list.iter_p
  let join = Lwt.join
  let run = Lwt_main.run

  let with_timeout timeout thread =
    let timeout_t =
      Lwt_unix.sleep timeout >>= fun () ->
      return `Timeout in
    let work_t =
      thread >>= fun r ->
      return (`Result r) in
    Lwt.choose [timeout_t; work_t]

  module Monad = struct
    let (>>=) = Lwt.bind
    let return = Lwt.return
  end

  module Mutex = struct
    type t = Lwt_mutex.t

    let create = Lwt_mutex.create
    let with_lock = Lwt_mutex.with_lock
  end

  module Logger = struct
    type t = Lwt_log.logger

    let syslog =
      Lwt_log.append_rule "*" Lwt_log.Info;
      Lwt_log.syslog ~facility:`User ()

    let debug logger fmt = Lwt_log.debug_f ~logger fmt
    let info logger fmt = Lwt_log.info_f ~logger fmt
    let error logger fmt = Lwt_log.error_f ~logger fmt
  end

  module Unix = struct
    type fd = Lwt_unix.file_descr

    type unix = [ `Unix of string ]
    type inet = [ `Inet of Unix.inet_addr * int ]
    type addr = [ unix | inet ]
    type ('state, 'addr) socket = fd
      constraint 'state = [< `Unconnected | `Bound | `Passive | `Active ]
      constraint 'addr  = [< addr]

    type 'addr unconnected_socket = ([`Unconnected], 'addr) socket
    type 'addr bound_socket = ([`Bound], 'addr) socket
    type 'addr passive_socket = ([`Passive], 'addr) socket
    type 'addr active_socket = ([`Active], 'addr) socket

    let sockaddr_of_addr = function
      | `Unix s -> Unix.ADDR_UNIX s
      | `Inet (a, p) -> Unix.ADDR_INET (a, p)

    let (>>=) = Lwt.(>>=)
    let return = Lwt.return

    let socket_fd (sock: ('state, 'addr) socket) : fd = sock
    let socket_of_fd (fd: fd) : ('state, 'addr) socket = fd

    let accept (sock: 'addr passive_socket) =
      Lwt_unix.accept (socket_fd sock) >>= fun (fd, sockaddr) ->
      let sock' : 'addr active_socket = socket_of_fd fd in
      return (sock', sockaddr)

    let bind (sock: 'addr unconnected_socket) addr =
      let sockaddr = sockaddr_of_addr addr in
      Lwt_unix.bind (socket_fd sock) sockaddr;
      return (sock: 'addr bound_socket)

    let chdir = Lwt_unix.chdir
    let chroot = Lwt_unix.chroot
    let close = Lwt_unix.close
    let dup fd = return (Lwt_unix.dup fd)
    let exit = Pervasives.exit
    let fd_of_socket s = s
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
    let setsockopt = Lwt_unix.setsockopt
    let unix_socket () = Lwt_unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0
    let stdin = Lwt_unix.stdin
    let unix_socket_of_fd fd = fd
    let unlink = Lwt_unix.unlink
    let waitpid pid =
      Lwt_unix.waitpid [] pid >>= fun (_, status) -> return status
    let wrap_file_descr fd =
      Lwt_unix.of_unix_file_descr fd
  end

  module Bytes = struct
    type t = Lwt_bytes.t

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
