(** This module defines the type of {!Release_config} configuration values
    and helper functions to extract OCaml values from {!value}s and to
    specify default values for configuration directives.
*)

type value =
  [ `Int of int
  | `Float of float
  | `Bool of bool
  | `Str of string
  | `Regexp of Str.regexp
  | `Log_level of Lwt_log.level
  | `List of value list
  ]
  (** The type of configuration values. *)

type validation = value -> [`Valid | `Invalid of string]
  (** The type of validation functions. *)

(** {6 Conversion to OCaml values} *)

val bool_value : [>`Bool of 'a] -> 'a
val int_value : [>`Int of 'a] -> 'a
val float_value : [>`Float of 'a] -> 'a
val string_value : [>`Str of 'a] -> 'a
val regexp_value : [>`Regexp of 'a] -> 'a
val log_level_value : [>`Log_level of 'a] -> 'a
val list_value : string -> ('a -> 'b) -> [>`List of 'a list] -> 'b list
val bool_list_value : [>`List of [>`Bool of 'a] list] -> 'a list
val int_list_value : [>`List of [>`Int of 'a] list] -> 'a list
val float_list_value : [>`List of [>`Float of 'a] list] -> 'a list
val string_list_value : [>`List of [>`Str of 'a] list] -> 'a list
val log_level_list_value : [>`List of [>`Log_level of 'a] list] -> 'a list

(** {6 Helpers for specifying default values of configuration directives} *)

val default_bool : bool -> [>`Bool of bool] option
val default_int : int -> [>`Int of int] option
val default_float : float -> [>`Float of float] option
val default_string : string -> [>`Str of string] option
val default_regexp : Str.regexp -> [>`Regexp of Str.regexp] option
val default_log_level : Lwt_log.level -> [>`Log_level of Lwt_log.level] option
val default_bool_list : bool list -> [>`List of [>`Bool of bool] list] option
val default_int_list : int list -> [>`List of [>`Int of int] list] option
val default_float_list : float list
                      -> [>`List of [>`Float of float] list] option
val default_string_list : string list
                       -> [>`List of [>`Str of string] list] option
