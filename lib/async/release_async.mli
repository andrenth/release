open Async

module Future : Release_future.S
  with type 'a t = 'a Deferred.t
   and type Unix.fd = Fd.t
   and type Logger.t = Log.t
   and type ('state, 'addr) Unix.socket = ('state, 'addr) Socket.t

module Release : Release.S
  with type 'a future := 'a Future.t
   and type fd := Future.Unix.fd
   and type logger := Future.Logger.t
   and type ('state, 'addr) Socket.t = ('state, 'addr) Future.Unix.socket
