open Printf
open Release_config_types

module Option = Release_option

type key =
  [ `Required of (string * validation)
  | `Optional of (string * validation)
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

let global_section = Release_config_global.global_section

let validate_and cont validation value =
  match validation value with
  | `Valid -> cont ()
  | `Invalid r -> `Invalid r

let rec validate_keys keys settings =
  match keys with
  | [] ->
      `Valid
  | key::rest ->
      let keep_validating () =
        validate_keys rest settings in
      let missing_required_key k () =
        `Invalid (sprintf "directive '%s' unspecified" k) in
      let name, validation, deal_with_missing =
        match key with
        | `Required (k, v) -> k, v, missing_required_key k
        | `Optional (k, v) -> k, v, keep_validating in
      Option.either
        deal_with_missing
        (validate_and keep_validating validation)
        (hash_find settings name)

let validate_keys_and cont keys settings =
  match validate_keys keys settings with
  | `Valid -> cont ()
  | `Invalid r -> `Invalid r

let rec validate_sections conf spec =
  match spec with
  | [] ->
    `Valid
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
        (hash_find conf name)

let validate =
  validate_sections

let join_with sep l =
  List.fold_left (fun joined i -> joined ^ sep ^ (string_of_int i)) "" l

exception Error of string

let join_errors errors =
  let concat msg (err, line) = msg ^ (sprintf "%s in line %d\n" err line) in
  List.fold_left concat "" errors

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
          let conf = Release_config_global.configuration in
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

let reset = Release_config_global.reset
