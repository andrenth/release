type t =
  [ `Keyword of string
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `Str of string
  | `Regexp of Re_pcre.regexp
  | `List of t list
  ]

let to_keyword = function
  | `Keyword l -> l
  | _ -> invalid_arg "keyword"

let to_bool = function
  | `Bool b -> b
  | _ -> invalid_arg "bool"

let to_int = function
  | `Int i -> i
  | _ -> invalid_arg "int"

let to_float = function
  | `Float i -> i
  | _ -> invalid_arg "float"

let to_string = function
  | `Str s -> s
  | _ -> invalid_arg "string"

let to_regexp = function
  | `Regexp r -> r
  | _ -> invalid_arg "regexp"

let to_list name f = function
  | `List l -> List.map f l
  | _ -> invalid_arg name

let to_keyword_list l = to_list "keyword_list" to_keyword l
let to_bool_list l = to_list "bool_list" to_bool l
let to_int_list l = to_list "int_list" to_int l
let to_float_list l = to_list "float_list" to_float l
let to_string_list l = to_list "string_list" to_string l

module Default = struct
  let keyword l = Some (`Keyword l)
  let bool b = Some (`Bool b)
  let int i = Some (`Int i)
  let float f = Some (`Float f)
  let string s = Some (`Str s)
  let regexp r = Some (`Regexp r)
  let keyword_list l = Some (`List (List.map (fun s -> `Keyword s) l))
  let bool_list l = Some (`List (List.map (fun b -> `Bool b) l))
  let int_list l = Some (`List (List.map (fun i -> `Int i) l))
  let float_list l = Some (`List (List.map (fun f -> `Float f) l))
  let string_list l = Some (`List (List.map (fun s -> `Str s) l))
end
