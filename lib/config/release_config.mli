open Release_config_types

type t

type key =
  [ `Required of (string * validation list)
  | `Optional of (string * value option * validation list)
  ]

type section =
  [ `Required of (string * key list)
  | `Optional of (string * key list)
  | `Global of key list
  ]

type spec = section list

val parse : Lwt_io.file_name -> spec -> [`Configuration of t | `Error of string]
val has_section : t -> string -> bool
val get : t -> ?section:string -> string -> unit -> value option
val get_exn : t -> ?section:string -> string -> unit -> value
