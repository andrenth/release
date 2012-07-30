type value =
  [ `Int of int
  | `Float of float
  | `Bool of bool
  | `Str of string
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

let bool_list_value = function
  | `List l -> List.map bool_value l
  | _ -> invalid_arg "bool_list_value"

let int_list_value = function
  | `List l -> List.map int_value l
  | _ -> invalid_arg "int_list_value"

let float_list_value = function
  | `List l -> List.map float_value l
  | _ -> invalid_arg "float_list_value"

let string_list_value = function
  | `List l -> List.map string_value l
  | _ -> invalid_arg "string_list_value"

let default_bool b = Some (`Bool b)
let default_int i = Some (`Int i)
let default_float f = Some (`Float f)
let default_string s = Some (`Str s)
let default_bool_list l = Some (`List (List.map (fun b -> `Bool b) l))
let default_int_list l = Some (`List (List.map (fun i -> `Int i) l))
let default_float_list l = Some (`List (List.map (fun f -> `Float f) l))
let default_string_list l = Some (`List (List.map (fun s -> `Str s) l))
