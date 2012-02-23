exception Release_privileges_error of string

val drop : string -> unit Lwt.t
