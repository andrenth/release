open Release_config_values

type t

type key = string * value option * validation list

type section =
  [ `Global of key list
  | `Section of (string * key list)
  ]

type spec = section list

val parse : Lwt_io.file_name
         -> spec
         -> [`Configuration of t | `Error of string] Lwt.t
val defaults : spec -> t
val get : t -> string -> string -> value
val get_global : t -> string -> value
