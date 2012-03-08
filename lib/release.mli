val daemon : (unit -> unit Lwt.t) -> unit Lwt.t

type ipc_handler = (Lwt_unix.file_descr -> unit Lwt.t)

val master_slave : ?background:bool
                -> ?syslog:bool
                -> ?privileged:bool
                -> ?control_socket:(string * ipc_handler)
                -> lock_file:string
                -> slave_ipc_handler:ipc_handler
                -> exec:string
                -> unit -> unit

val master_slaves : ?background:bool
                 -> ?syslog:bool
                 -> ?privileged:bool
                 -> ?control_socket:(string * ipc_handler)
                 -> num_slaves:int
                 -> lock_file:string
                 -> slave_ipc_handler:ipc_handler
                 -> exec:string
                 -> unit -> unit

val me : ?syslog:bool
      -> ?user:string
      -> main:(Lwt_unix.file_descr -> unit Lwt.t)
      -> unit -> unit
