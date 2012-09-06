(** This module provides an interface to configuration file parsing.
    Configuration files are assumed to be collections of key-value pairs
    possibly organized in sections.

    Sections are started by a section name enclosed in square brackets.
    Keys and values are separated by and equals sign, and values can be
    integers, floats, booleans, strings, regular expressions, log levels
    or lists of one of those types, as defined by the {!Release_values.value}
    type.

    In terms of code, configuration files are specified by the
    {!Release_config.spec} type. This is simply a list of
    {!Release_config.section}s, each containing lists of
    {!Release_config.key}s. Keys are defined by their name, an optional
    default value and a list of validations. If a key has no default value
    and is absent from the configuration file, an error will be generated.

    Validations are functions as defined by the
    {!Release_config_values.validation} type. Many pre-defined validations
    are available in the {!Release_config_validations} module.
*)

open Release_config_values

type t
  (** The type of a configuration. *)

type key = string * value option * validation list
  (** The type of a configuration key specification. A key is specified by
      its name, a possible default value and a list of validations. *)

type section =
  [ `Global of key list
  | `Section of (string * key list)
  ]
  (** The type of a configuration section specification. A section
      specification contains a list of keys belonging to that section, and
      can either have a name or be global (in which case its keys will not
      appear under any section definitions in the configuration file). *)

type spec = section list
  (** The type of configuration file specifications. *)

val parse : Lwt_io.file_name
         -> spec
         -> [`Configuration of t | `Error of string] Lwt.t
  (** Parse a configuration file. [parse file spec] will try to parse [file]
      and, if it is syntatically correct, validate it against the specification
      given in [spec]. *)

val defaults : spec -> t
  (** [defaults spec] returns a configuration built from the default values
      of [spec]. This only makes sense if every key in the specification has
      a default value. *)

val get : t -> string -> string -> value
  (** [get conf section key] returns the value of the parameter [key] of
      section [section] in configuration [conf]. *)

val get_global : t -> string -> value
  (** [get_global conf key] returns the value of the global parameter [key]. *)
