module Future : Release_future.S
  with type 'a t = 'a Lwt.t
   and type Unix.fd = Lwt_unix.file_descr
   and type Logger.t = Lwt_log.logger
   and type ('state, 'addr) Unix.socket = Lwt_unix.file_descr

module Release : Release.S
  with type 'a future := 'a Future.t
   and type fd := Future.Unix.fd
   and type logger := Future.Logger.t
   and type ('state, 'addr) Socket.t = ('state, 'addr) Future.Unix.socket
