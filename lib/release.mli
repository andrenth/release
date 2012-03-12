val daemon : (unit -> unit Lwt.t) -> unit Lwt.t

val master_slave : ?background:bool
                -> ?syslog:bool
                -> ?privileged:bool
                -> ?control:(string * (Lwt_unix.file_descr -> unit Lwt.t))
                -> lock_file:string
                -> slave_ipc_handler:(Lwt_unix.file_descr -> unit Lwt.t)
                -> exec:string
                -> unit -> unit

val master_slaves : ?background:bool
                 -> ?syslog:bool
                 -> ?privileged:bool
                 -> ?control:(string * (Lwt_unix.file_descr -> unit Lwt.t))
                 -> num_slaves:int
                 -> lock_file:string
                 -> slave_ipc_handler:(Lwt_unix.file_descr -> unit Lwt.t)
                 -> exec:string
                 -> unit -> unit

val me : ?syslog:bool
      -> ?user:string
      -> main:(Lwt_unix.file_descr -> unit Lwt.t)
      -> unit -> unit
