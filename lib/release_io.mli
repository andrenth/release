val read : ?timeout:float
        -> Lwt_unix.file_descr
        -> int
        -> [`Data of string | `EOF | `Timeout] Lwt.t

val write : Lwt_unix.file_descr -> string -> unit Lwt.t
