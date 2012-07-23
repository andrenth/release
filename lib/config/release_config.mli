open Release_config_types

type validation = value -> [`Valid | `Invalid of string]

type key =
  [ `Required of (string * validation option)
  | `Optional of (string * validation option)
  ]

type section =
  [ `Required of (string * key list)
  | `Optional of (string * key list)
  | `Global of key list
  ]

type spec = section list

type t

val parse : Lwt_io.file_name -> spec -> [`Configuration of t | `Error of string]
val has_section : t -> string -> bool
val get : t -> ?section:string -> string -> unit -> value option
val get_exn : t -> ?section:string -> string -> unit -> value
val reset : unit -> unit
