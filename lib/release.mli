val daemon : (unit -> unit Lwt.t) -> unit Lwt.t

val master_slave : ?background:bool
                -> ?syslog:bool
                -> ?privileged:bool
                -> lock_file:string
                -> ipc_handler:(Lwt_unix.file_descr -> unit Lwt.t)
                -> exec:string
                -> unit -> unit

val master_slaves : ?background:bool
                 -> ?syslog:bool
                 -> ?privileged:bool
                 -> num_slaves:int
                 -> lock_file:string
                 -> ipc_handler:(Lwt_unix.file_descr -> unit Lwt.t)
                 -> exec:string
                 -> unit -> unit

val slave : ?syslog:bool
         -> ?user:string
         -> main:(Lwt_unix.file_descr -> unit Lwt.t)
         -> unit -> unit
