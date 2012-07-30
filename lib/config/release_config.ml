open Printf
open Release_config_types

module Option = Release_option

type key =
  [ `Required of (string * validation list)
  | `Optional of (string * value option * validation list)
  ]

type section =
  [ `Required of (string * key list)
  | `Optional of (string * key list)
  | `Global of key list
  ]

type spec = section list

type t = (string, (string, value) Hashtbl.t) Hashtbl.t

let hash_find h k =
  try Some (Hashtbl.find h k)
  with Not_found -> None

let hash_fetch h k =
  try
    let v = Hashtbl.find h k in
    Hashtbl.remove h k;
    Some v
  with Not_found ->
    None

let hash_join h sep =
  let s = Hashtbl.fold (fun k v s -> k ^ sep ^ s) h "" in
  String.sub s 0 (String.length s - 2)

let unknown_config kind conf =
  let unknown = hash_join conf ", " in
  sprintf "unknown %s: %s" kind unknown

let validate_missing msg conf =
  let len = Hashtbl.length conf in
  if len = 0 then
    `Valid
  else
    let msg = if len = 1 then msg else msg ^ "s" in
    `Invalid (unknown_config msg conf)

let global_section = Release_config_global.global_section

let validate_and cont validations value =
  let rec validate = function
    | [] ->
        cont ()
    | v::vs ->
        match v value with
        | `Valid -> validate vs
        | `Invalid r -> `Invalid r in
  validate validations

let rec validate_keys keys settings =
  match keys with
  | [] ->
      validate_missing "configuration directive" settings
  | key::rest ->
      let keep_validating () =
        validate_keys rest settings in
      let validate_default value validations () =
        Option.either
          keep_validating
          (validate_and keep_validating validations)
          value in
      let missing_required_key k () =
        `Invalid (sprintf "configuration directive '%s' missing" k) in
      let name, validations, deal_with_missing =
        match key with
        | `Required (k, vs) -> k, vs, missing_required_key k
        | `Optional (k, def, vs) -> k, vs, validate_default def vs in
      Option.either
        deal_with_missing
        (validate_and keep_validating validations)
        (hash_fetch settings name)

let validate_keys_and cont keys settings =
  match validate_keys keys (Hashtbl.copy settings) with
  | `Valid -> cont ()
  | `Invalid r -> `Invalid r

let rec validate_sections conf spec =
  match spec with
  | [] ->
      validate_missing "section" conf
  | section::sections ->
      let keep_validating () =
        validate_sections conf sections in
      let missing_required_section s () =
        `Invalid (sprintf "section '%s' missing" s) in
      let name, keys, deal_with_missing =
        match section with
        | `Global ks -> global_section, ks, keep_validating
        | `Required (s, ks) -> s, ks, missing_required_section s
        | `Optional (s, ks) -> s, ks, keep_validating in
      Option.either
        deal_with_missing
        (validate_keys_and keep_validating keys)
        (hash_fetch conf name)

let validate conf =
  (* Validation is destructive, so make a copy of the configuration *)
  validate_sections (Hashtbl.copy conf)

let join_with sep l =
  List.fold_left (fun joined i -> joined ^ sep ^ (string_of_int i)) "" l

exception Error of string

let join_errors errors =
  let concat msg (err, line) = msg ^ (sprintf "%s in line %d\n" err line) in
  List.fold_left concat "" errors

let reset = Release_config_global.reset

let parse file spec =
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
        (try
          let conf = Hashtbl.copy Release_config_global.configuration in
          reset ();
          match validate conf spec with
          | `Valid -> `Configuration conf
          | `Invalid reason -> `Error reason
        with Error reason ->
          `Error reason)
    | errors ->
        `Error (join_errors errors)

let has_section conf section =
  Hashtbl.mem conf section

let get conf ?(section = global_section) key () =
  match hash_find conf section with
  | Some settings -> hash_find settings key
  | None -> None

let get_exn conf ?(section = global_section) key () =
  Hashtbl.find (Hashtbl.find conf section) key
