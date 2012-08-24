open Release_config_types

type t

type key = string * value option * validation list

type section =
  [ `Global of key list
  | `Section of (string * key list)
  ]

type spec = section list

val parse : Lwt_io.file_name -> spec -> [`Configuration of t | `Error of string]
val defaults : spec -> t
val has_section : t -> string -> bool
val get : t -> ?section:string -> string -> unit -> value option
val get_exn : t -> ?section:string -> string -> unit -> value
