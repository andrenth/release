open Printf

module type S = sig
  module Value : sig
    type t =
      [ `Keyword of string
      | `Bool of bool
      | `Int of int
      | `Float of float
      | `Str of string
      | `Regexp of Str.regexp
      | `List of t list
      ]

    val to_keyword : [>`Keyword of string] -> string
    val to_bool : [>`Bool of bool] -> bool
    val to_int : [>`Int of int] -> int
    val to_float : [>`Float of float] -> float
    val to_string : [>`Str of string] -> string
    val to_regexp : [>`Regexp of Str.regexp] -> Str.regexp
    val to_list : string -> (t -> 'a) -> [>`List of t list] -> 'a list
    val to_keyword_list : [>`List of [>`Keyword of string] list] -> string list
    val to_bool_list : [>`List of [>`Bool of bool] list] -> bool list
    val to_int_list : [>`List of [>`Int of int] list] -> int list
    val to_float_list : [>`List of [>`Float of float] list] -> float list
    val to_string_list : [>`List of [>`Str of string] list] -> string list

    module Default : sig
      val keyword : string -> [>`Keyword of string] option
      val bool : bool -> [>`Bool of bool] option
      val int : int -> [>`Int of int] option
      val float : float -> [>`Float of float] option
      val string : string -> [>`Str of string] option
      val regexp : Str.regexp -> [>`Regexp of Str.regexp] option
      val keyword_list : string list
                      -> [>`List of [>`Keyword of string] list] option
      val bool_list : bool list -> [>`List of [>`Bool of bool] list] option
      val int_list : int list -> [>`List of [>`Int of int] list] option
      val float_list : float list -> [>`List of [>`Float of float] list] option
      val string_list : string list -> [>`List of [>`Str of string] list] option
    end
  end

  module Validation : sig
    type result = [`Valid | `Invalid of string]
    type t = Value.t -> result

    val keyword : string -> t
    val keywords : string list -> t
    val bool : t
    val int : t
    val float : t
    val string : t
    val regexp : t
    val bool_list : t
    val int_list : t
    val float_list : t
    val string_list : t
    val int_in_range : int * int -> t
    val int_greater_than : int -> t
    val int_less_than : int -> t
    val float_in_range : float * float -> t
    val float_greater_than : float -> t
    val float_less_than : float -> t
    val string_matching : string -> t
    val int_in : int list -> t
    val string_in : string list -> t
    val existing_file : t
    val nonempty_file : t
    val file_with_mode : Unix.file_perm -> t
    val file_with_owner : string -> t
    val file_with_group : string -> t
    val existing_directory : t
    val existing_dirname : t
    val block_device : t
    val character_device : t
    val symbolic_link : t
    val named_pipe : t
    val unix_socket : t
    val existing_user : t
    val unprivileged_user : t
    val existing_group : t
    val list_of : t -> t
  end

  type t
  type +'a future
  type key = string * Value.t option * Validation.t list
  type section =
    [ `Global of key list
    | `Section of (string * key list)
    ]
  type spec = section list

  val parse : string
           -> spec
           -> [`Configuration of t | `Error of string] future
  val defaults : spec -> t
  val get : t -> string -> string -> Value.t
  val get_global : t -> string -> Value.t
end

module Make (Future : Release_future.S) : S
  with type 'a future = 'a Future.t =
struct
  module Value = Release_config_values
  module Validation = Release_config_validations

  module Util = Release_util.Make (Future)
  module Option = Util.Option

  open Future.Monad
  open Util.Monad

  type +'a future = 'a Future.t

  type key = string * Value.t option * Validation.t list

  type section =
    [ `Global of key list
    | `Section of (string * key list)
    ]

  type spec = section list

  type t = (string, (string, Value.t) Hashtbl.t) Hashtbl.t

  let hash_merge h1 h2 =
    Hashtbl.iter (fun k v -> Hashtbl.replace h1 k v) h2

  let dup conf =
    let s = Marshal.to_string conf [] in
    Marshal.from_string s 0

  let hash_find h k =
    try Some (Hashtbl.find h k)
    with Not_found -> None

  let key_table h =
    let h' = Hashtbl.create (Hashtbl.length h) in
    Hashtbl.iter (fun k _ -> Hashtbl.replace h' k true) h;
    h'

  let merge_defaults config defaults =
    let conf = dup defaults in
    Hashtbl.iter
      (fun section keys ->
        match hash_find conf section with
        | None -> Hashtbl.replace conf section keys
        | Some keys' -> hash_merge keys' keys)
      config;
    conf

  let hash_join h sep =
    let s = Hashtbl.fold (fun k v s -> k ^ sep ^ s) h "" in
    String.sub s 0 (String.length s - 2)

  let unknown_config kind conf =
    let unknown = hash_join conf ", " in
    sprintf "unknown %s '%s'" kind unknown

  let validate_unknown msg conf =
    let len = Hashtbl.length conf in
    if len = 0 then
      `Valid
    else
      let msg = if len = 1 then msg else msg ^ "s" in
      `Invalid (unknown_config msg conf)

  let global_section = Release_config_global.global_section

  let validate_key_and cont validations value =
    let rec validate = function
      | [] ->
          cont ()
      | v::vs ->
          match v value with
          | `Valid -> validate vs
          | `Invalid r -> `Invalid r in
    validate validations

  let validate_keys section keys spec =
    let unknown = key_table keys in
    let missing key =
      sprintf "configuration directive '%s' missing in section '%s'"
        key section in
    let rec validate = function
      | [] ->
          validate_unknown "configuration directive" unknown
      | key::rest ->
          let keep_validating () =
            validate rest in
          let name, _, validations = key in
          Hashtbl.remove unknown name;
          let missing_key () =
            `Invalid (missing name) in
          Option.either
            missing_key
            (validate_key_and keep_validating validations)
            (hash_find keys name) in
    validate spec

  let validate_keys_and cont section keys_spec keys =
    match validate_keys section keys keys_spec with
    | `Valid -> cont ()
    | `Invalid r -> `Invalid r

  let validate_sections conf spec =
    let unknown = key_table conf in
    let rec validate = function
      | [] ->
          validate_unknown "section" unknown
      | section::rest ->
          let keep_validating () =
            validate rest in
          let name, keys_spec =
            match section with
            | `Global ks -> global_section, ks
            | `Section (name, ks) -> name, ks in
          Hashtbl.remove unknown name;
          let keys = Hashtbl.find conf name in
          validate_keys_and keep_validating name keys_spec keys in
    validate spec

  let join_errors errors =
    let concat msg (err, line) = msg ^ (sprintf "%s in line %d\n" err line) in
    List.fold_left concat "" errors

  let default_keys spec =
    let keys = Hashtbl.create 8 in
    let add_key key =
      match key with
      | name, None, _ -> ()
      | name, Some default, _ -> Hashtbl.replace keys name default in
    List.iter add_key spec;
    keys

  let defaults spec =
    let conf = Hashtbl.create 4 in
    let create_section section =
      let name, keys_spec =
        match section with
        | `Global spec -> global_section, spec
        | `Section (name, kspec) -> name, kspec in
      let keys = default_keys keys_spec in
      Hashtbl.replace conf name keys in
    List.iter create_section spec;
    conf

  let validate conf spec =
    let conf = merge_defaults conf (defaults spec) in
    match validate_sections conf spec with
    | `Valid -> `Configuration conf
    | `Invalid reason -> `Error reason

  let remove_empty_global_section conf =
    match hash_find conf global_section with
    | None -> ()
    | Some g -> if Hashtbl.length g = 0 then Hashtbl.remove conf global_section

  let config_mutex = Future.Mutex.create ()

  let parse file spec =
    let parse_config () =
      let ch = open_in file in
      try
        let lexbuf = Lexing.from_channel ch in
        while true do
          Release_config_parser.input Release_config_lexer.token lexbuf
        done;
        assert false (* not reached *)
      with End_of_file ->
        close_in ch;
        match Release_config_global.errors () with
        | [] ->
            let conf = Release_config_global.copy () in
            remove_empty_global_section conf;
            return (validate conf spec)
        | errors ->
            return (`Error (join_errors errors)) in
    Future.Mutex.with_lock config_mutex parse_config

  let get conf section key =
    Hashtbl.find (Hashtbl.find conf section) key

  let get_global conf key =
    get conf global_section key
end
