type value =
  [ `Int of int
  | `Float of float
  | `Bool of bool
  | `Str of string
  | `Regexp of Str.regexp
  | `Log_level of Lwt_log.level
  | `List of (value list)
  ]

type validation = value -> [`Valid | `Invalid of string]

let bool_value = function
  | `Bool b -> b
  | _ -> invalid_arg "bool_value"

let int_value = function
  | `Int i -> i
  | _ -> invalid_arg "int_value"

let float_value = function
  | `Float i -> i
  | _ -> invalid_arg "float_value"

let string_value = function
  | `Str s -> s
  | _ -> invalid_arg "string_value"

let regexp_value = function
  | `Regexp r -> r
  | _ -> invalid_arg "regexp_value"

let log_level_value = function
  | `Log_level l -> l
  | _ -> invalid_arg "log_level_value"

let list_value name f = function
  | `List l -> List.map f l
  | _ -> invalid_arg name

let bool_list_value l = list_value "bool_list_value" bool_value l
let int_list_value l = list_value "int_list_value" int_value l
let float_list_value l = list_value "float_list_value" float_value l
let string_list_value l = list_value "string_list_value" string_value l
let log_level_list_value l = list_value "log_level_list_value" log_level_value l

let default_bool b = Some (`Bool b)
let default_int i = Some (`Int i)
let default_float f = Some (`Float f)
let default_string s = Some (`Str s)
let default_regexp r = Some (`Regexp r)
let default_log_level l = Some (`Log_level l)
let default_bool_list l = Some (`List (List.map (fun b -> `Bool b) l))
let default_int_list l = Some (`List (List.map (fun i -> `Int i) l))
let default_float_list l = Some (`List (List.map (fun f -> `Float f) l))
let default_string_list l = Some (`List (List.map (fun s -> `Str s) l))
