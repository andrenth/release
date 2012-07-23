(** Miscellaneous socket-related utility functions. *)

val accept_loop : ?backlog:int
               -> ?timeout:float
               -> Lwt_unix.socket_type
               -> Lwt_unix.sockaddr
               -> (Lwt_unix.file_descr -> unit Lwt.t)
               -> unit Lwt.t
  (** Returns a thread that creates a socket of the given type, binds it to
      the given address and blocks listening for connections. When a new
      connection is established, the callback function is called in a handler
      thread. The default [backlog] value is 10 and the default [timeout] is
      10 seconds. *)
