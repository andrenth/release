type value =
  [ `Int of int
  | `Float of float
  | `Bool of bool
  | `Str of string
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
