open Release_config_types

val bool : value -> [`Invalid of string | `Valid]
val int : value -> [`Invalid of string | `Valid]
val float : value -> [`Invalid of string | `Valid]
val string : value -> [`Invalid of string | `Valid]
val int_in_range : int * int -> value -> [`Invalid of string | `Valid]
val string_matching : string -> value -> [`Invalid of string | `Valid]
val existing_file : value -> [`Invalid of string | `Valid]
val nonempty_file : value -> [`Invalid of string | `Valid]
val file_with_mode : Unix.file_perm -> value -> [`Invalid of string | `Valid]
val file_with_owner : string -> value -> [`Invalid of string | `Valid]
val file_with_group : string -> value -> [`Invalid of string | `Valid]
val existing_directory : value -> [`Invalid of string | `Valid]
val existing_dirname : value -> [`Invalid of string | `Valid]
val existing_user : value -> [`Invalid of string | `Valid]
val unprivileged_user : value -> [`Invalid of string | `Valid]
val existing_group : value -> [`Invalid of string | `Valid]
val int_in : int list -> value -> [`Invalid of string | `Valid]
val string_in : string list -> value -> [`Invalid of string | `Valid]