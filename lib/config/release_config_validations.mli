open Release_config_types

val bool : value -> [`Invalid of string | `Valid]
val int : value -> [`Invalid of string | `Valid]
val float : value -> [`Invalid of string | `Valid]
val string : value -> [`Invalid of string | `Valid]
val int_in_range : int * int -> value -> [`Invalid of string | `Valid]
val string_matching : string -> value -> [`Invalid of string | `Valid]
val existing_file : value -> [`Invalid of string | `Valid]
val existing_directory : value -> [`Invalid of string | `Valid]
val existing_dirname : value -> [`Invalid of string | `Valid]
val existing_user : value -> [`Invalid of string | `Valid]
val one_of_ints : int list -> value -> [`Invalid of string | `Valid]
val one_of_strings : string list -> value -> [`Invalid of string | `Valid]
