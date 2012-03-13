val daemon : (unit -> unit Lwt.t) -> unit Lwt.t

val master_slave : slave:(Lwt_io.file_name * Release_ipc.handler)
                -> ?background:bool
                -> ?syslog:bool
                -> ?privileged:bool
                -> ?control:(Lwt_io.file_name * Release_ipc.handler)
                -> lock_file:Lwt_io.file_name
                -> unit -> unit

val master_slaves : ?background:bool
                 -> ?syslog:bool
                 -> ?privileged:bool
                 -> ?control:(Lwt_io.file_name * Release_ipc.handler)
                 -> lock_file:Lwt_io.file_name
                 -> slaves:(Lwt_io.file_name * Release_ipc.handler * int) list
                 -> unit -> unit

val me : ?syslog:bool
      -> ?user:string
      -> main:(Lwt_unix.file_descr -> unit Lwt.t)
      -> unit -> unit
