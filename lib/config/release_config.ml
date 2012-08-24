open Lwt
open Printf
open Release_config_values

module Option = Release_option

type key = string * value option * validation list

type section =
  [ `Global of key list
  | `Section of (string * key list)
  ]

type spec = section list

type t = (string, (string, value) Hashtbl.t) Hashtbl.t

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

let parse file spec =
  let input () =
    let ch = open_in file in
    try
      let lexbuf = Lexing.from_channel ch in
      while true do
        Release_config_parser.input Release_config_lexer.token lexbuf
      done;
      assert false (* not reached *)
    with End_of_file ->
      close_in ch in
  lwt () = Lwt_preemptive.detach input () in
  match Release_config_global.errors () with
  | [] ->
      let conf = Release_config_global.copy () in
      remove_empty_global_section conf;
      return (validate conf spec)
  | errors ->
      return (`Error (join_errors errors))

let has_section conf section =
  Hashtbl.mem conf section

let get conf ?(section = global_section) key () =
  Option.maybe (fun settings -> hash_find settings key) (hash_find conf section)

let get_exn conf ?(section = global_section) key () =
  Hashtbl.find (Hashtbl.find conf section) key
