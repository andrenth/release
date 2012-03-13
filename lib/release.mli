type ipc_handler = (Lwt_unix.file_descr -> unit Lwt.t)

val daemon : (unit -> unit Lwt.t) -> unit Lwt.t

val master_slave : slave:(string * ipc_handler)
                -> ?background:bool
                -> ?syslog:bool
                -> ?privileged:bool
                -> ?control:(string * ipc_handler)
                -> lock_file:string
                -> unit -> unit

val master_slaves : ?background:bool
                 -> ?syslog:bool
                 -> ?privileged:bool
                 -> ?control:(string * ipc_handler)
                 -> lock_file:string
                 -> slaves:(string * ipc_handler * int) list
                 -> unit -> unit

val me : ?syslog:bool
      -> ?user:string
      -> main:(Lwt_unix.file_descr -> unit Lwt.t)
      -> unit -> unit
