(** This module provides validation helpers to be used in {!Release_config}
    specifications.
*)

open Release_config_values

val bool : value -> [`Invalid of string | `Valid]
  (** The value is a boolean. *)

val int : value -> [`Invalid of string | `Valid]
  (** The value is an integer. *)

val float : value -> [`Invalid of string | `Valid]
  (** The value is a float. *)

val string : value -> [`Invalid of string | `Valid]
  (** The value is a string. *)

val regexp : value -> [`Invalid of string | `Valid]
  (** The value is a regular expression. *)

val log_level : value -> [`Invalid of string | `Valid]
  (** The value is a log level. *)

val bool_list : value -> [`Invalid of string | `Valid]
  (** The value is a list of boolean values. *)

val int_list : value -> [`Invalid of string | `Valid]
  (** The value is a list of integer values. *)

val float_list : value -> [`Invalid of string | `Valid]
  (** The value is a list of float values. *)

val string_list : value -> [`Invalid of string | `Valid]
  (** The value is a list of string values. *)

val int_in_range : int * int -> value -> [`Invalid of string | `Valid]
  (** The value is an integer in the inclusive range defined by the given
      tuple. *)

val int_greater_than : int -> value -> [`Invalid of string | `Valid]
  (** The value is an integer greater than the given argument. *)

val int_less_than : int -> value -> [`Invalid of string | `Valid]
  (** The value is an integer less than the given argument. *)

val float_in_range : float * float -> value -> [`Invalid of string | `Valid]
  (** The value is a float in the inclusive range defined by the given
      tuple. *)

val float_greater_than : float -> value -> [`Invalid of string | `Valid]
  (** The value is a float greater than the given argument.. *)

val float_less_than : float -> value -> [`Invalid of string | `Valid]
  (** The value is a float less than the given argument. *)

val string_matching : string -> value -> [`Invalid of string | `Valid]
  (** The value is a string matching the regular expression given as a string.
      The regular expression is created by calling [Str.regexp] on the first
      argument. *)

val int_in : int list -> value -> [`Invalid of string | `Valid]
  (** The value is equal to one of the integers in the given list. *)

val string_in : string list -> value -> [`Invalid of string | `Valid]
  (** The value is equal to one of the strings in the given list. *)

val existing_file : value -> [`Invalid of string | `Valid]
  (** The value is an existing file. *)

val nonempty_file : value -> [`Invalid of string | `Valid]
  (** The value is a file whose size is nonzero. *)

val file_with_mode : Unix.file_perm -> value -> [`Invalid of string | `Valid]
  (** The value is a file with the given mode. *)

val file_with_owner : string -> value -> [`Invalid of string | `Valid]
  (** The value is a file with the given owner. *)

val file_with_group : string -> value -> [`Invalid of string | `Valid]
  (** The value is a file with the given group. *)

val existing_directory : value -> [`Invalid of string | `Valid]
  (** The value is an existing directory. *)

val existing_dirname : value -> [`Invalid of string | `Valid]
  (** The value is a path whose directory name (as in [dirname(3)]) exists. *)

val block_device : value -> [`Invalid of string | `Valid]
  (** The value is a block device. *)

val character_device : value -> [`Invalid of string | `Valid]
  (** The value is a character device. *)

val symbolic_link : value -> [`Invalid of string | `Valid]
  (** The value is a symbolic link. *)

val named_pipe : value -> [`Invalid of string | `Valid]
  (** The value is a named pipe. *)

val unix_socket : value -> [`Invalid of string | `Valid]
  (** The value is a unix socket. *)

val existing_user : value -> [`Invalid of string | `Valid]
  (** The value is an exiting user. *)

val unprivileged_user : value -> [`Invalid of string | `Valid]
  (** The value is a non-root user. *)

val existing_group : value -> [`Invalid of string | `Valid]
  (** The value is an existing group. *)

val list_of : validation -> value -> [`Invalid of string | `Valid]
  (** The value is a list whose elements must pass the given validation. *)
