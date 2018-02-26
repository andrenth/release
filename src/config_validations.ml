open Printf

type t = Config_values.t -> [`Valid | `Invalid of string]
type result = [`Valid | `Invalid of string]

let keyword kw = function
  | `Keyword kw' when kw = kw' -> `Valid
  | `Keyword kw' -> `Invalid (sprintf "keyword: expected %s, got %s" kw kw')
  | _ -> `Invalid "keyword: not a keyword"

let keywords kws = function
  | `Keyword kw when List.mem kw kws -> `Valid
  | `Keyword kw -> `Invalid (sprintf "keywords: %s unexpected" kw)
  | _ -> `Invalid "keyword: not a keyword"

let bool = function
  | `Bool _ -> `Valid
  | _ -> `Invalid "bool: not a bool"

let int = function
  | `Int _ -> `Valid
  | _ -> `Invalid "int: not an int"

let float = function
  | `Float _ -> `Valid
  | _ -> `Invalid "float: not a float"

let string = function
  | `Str _ -> `Valid
  | _ -> `Invalid "string: not a string"

let regexp = function
  | `Regexp _ -> `Valid
  | _ -> `Invalid "regexp: not a regular expression"

let any_list name f = function
  | `List l ->
      if List.exists (fun x -> f x <> `Valid) l then
        `Invalid (sprintf "%s_list: not a %s list" name name)
      else
        `Valid
  | _ ->
      `Invalid (sprintf "%s_list: not a list" name)

let bool_list : Config_values.t -> result = any_list "bool" bool
let int_list : Config_values.t -> result = any_list "int" int
let float_list : Config_values.t -> result = any_list "float" float
let string_list : Config_values.t -> result = any_list "string" string

let int_in_range (min, max) = function
  | `Int x ->
      if x >= min && x <= max then
        `Valid
      else
        `Invalid (sprintf "int_in_range: %d not in range [%d, %d]" x min max)
  | _ ->
      `Invalid "int_in_range: not an int"

let int_greater_than min = function
  | `Int x ->
      if x > min then `Valid
      else `Invalid (sprintf "int_greater_than: %d not greater than %d" x min)
  | _ ->
      `Invalid "int_greater_than: not an int"

let int_less_than max = function
  | `Int x ->
      if x < max then `Valid
      else `Invalid (sprintf "int_less_than: %d not less than %d" x max)
  | _ ->
      `Invalid "int_less_than: not an int"

let float_in_range (min, max) = function
  | `Float x ->
      if x >= min && x <= max then
        `Valid
      else
        `Invalid (sprintf "float_in_range: %f not in range [%f, %f]" x min max)
  | _ ->
      `Invalid "float_in_range: not a float"

let float_greater_than min = function
  | `Float x ->
      if x > min then `Valid
      else `Invalid (sprintf "float_greater_than: %f not greater than %f" x min)
  | _ ->
      `Invalid "float_greater_than: not a float"

let float_less_than max = function
  | `Float x ->
      if x < max then `Valid
      else `Invalid (sprintf "float_less_than: %f not less than %f" x max)
  | _ ->
      `Invalid "float_less_than: not a float"

let string_matching re = function
  | `Str s ->
      (try
        if Re_pcre.pmatch ~rex:(Re_pcre.regexp re) s then
          `Valid
        else
          `Invalid (sprintf "string_matching: %s doesn't match %s" s re)
      with _ ->
        `Invalid (sprintf "string_matching: invalid regular expression: %s" re))
  | _ ->
      `Invalid "string_matching: not a string"

let int_in l = function
  | `Int i ->
      if List.mem i l then
        `Valid
      else
        `Invalid (sprintf "int_in: %d not found" i)
  | _ ->
      `Invalid "int_in: not an int"

let string_in l = function
  | `Str s ->
      if List.mem s l then
        `Valid
      else
        `Invalid (sprintf "string_in: %s not found" s)
  | _ ->
      `Invalid "string_in: not a string"

let file_with f name err (v : Config_values.t) : result =
  match v with
  | `Str file ->
      (try
        let st = Unix.lstat file in
        if f st then `Valid
        else `Invalid (sprintf "%s: %s %s" name file err)
      with
      | Unix.Unix_error (e, _, _) ->
        `Invalid (sprintf "%s: %s: %s" name file (Unix.error_message e))
      | e ->
        `Invalid (sprintf "%s: %s: %s" name file (Printexc.to_string e)))
  | _ -> `Invalid (sprintf "%s: not a string" name)


let existing_file =
  file_with
    (fun _ -> true)
    "existing_file"
    "does not exist"

let regular_file =
  file_with
    (fun st -> st.Unix.st_kind = Unix.S_REG)
    "regular_file"
    "is not a regular file"

let block_device =
  file_with
    (fun st -> st.Unix.st_kind = Unix.S_BLK)
    "block_device"
    "is not a block device"

let character_device =
  file_with
    (fun st -> st.Unix.st_kind = Unix.S_CHR)
    "character_device"
    "is not a character device"

let symbolic_link =
  file_with
    (fun st -> st.Unix.st_kind = Unix.S_LNK)
    "symbolic_link"
    "is not a symbolic link"

let named_pipe =
  file_with
    (fun st -> st.Unix.st_kind = Unix.S_FIFO)
    "named_pipe"
    "is not a named pipe"

let unix_socket =
  file_with
    (fun st -> st.Unix.st_kind = Unix.S_SOCK)
    "unix_socket"
    "is not a unix socket"

let file_with_mode m =
  file_with
    (fun st -> st.Unix.st_perm = m)
    "file_with_mode"
    (sprintf "must have mode 0%o" m)

let nonempty_file =
  file_with
    (fun st -> st.Unix.st_size > 0)
    "nonempty_file"
    "is empty"

let file_with_owner u =
  file_with
    (fun st -> st.Unix.st_uid = (Unix.getpwnam u).Unix.pw_uid)
    "file_with_owner"
    (sprintf "must be owned by %s" u)

let file_with_group g =
  file_with
    (fun st -> st.Unix.st_gid = (Unix.getgrnam g).Unix.gr_gid)
    "file_with_group"
    (sprintf "must have group %s" g)

let existing_directory = function
  | `Str f ->
      (try
        let st = Unix.lstat f in
        if st.Unix.st_kind = Unix.S_DIR then `Valid
        else `Invalid (sprintf "existing_directory: %s is not a directory" f)
      with Unix.Unix_error (e, _, _) ->
        `Invalid (sprintf "existing_file: %s: %s" f (Unix.error_message e)))
  | _ -> `Invalid "existing_directory: not a string"

let existing_dirname = function
  | `Str p ->
      (try
        ignore (Unix.lstat (Filename.dirname p));
        `Valid
      with Unix.Unix_error (e, _, _) ->
        `Invalid (sprintf "existing_dirname: %s %s" p (Unix.error_message e)))
  | _ -> `Invalid "existing_dirname: not a string"

let existing_user = function
  | `Str u ->
      (try
        ignore (Unix.getpwnam u);
        `Valid
      with
      | Unix.Unix_error (e, _, _) ->
        `Invalid (sprintf "existing_user: %s: %s" u (Unix.error_message e))
      | Not_found ->
        `Invalid (sprintf "existing_user: %s: user not found" u))
  | _ -> `Invalid "existing_user: not a string"

let unprivileged_user = function
  | `Str u ->
      (try
        let pw = Unix.getpwnam u in
        if pw.Unix.pw_uid <> 0 then `Valid
        else `Invalid (sprintf "user %s is privileged" u)
      with
      | Unix.Unix_error (e, _, _) ->
        `Invalid (sprintf "unprivileged_user: %s: %s" u (Unix.error_message e))
      | Not_found ->
        `Invalid (sprintf "unprivileged_user: %s: user not found" u))
  | _ -> `Invalid "unprivileged_user: not a string"

let existing_group = function
  | `Str g ->
      (try
        ignore (Unix.getgrnam g);
        `Valid
      with
      | Unix.Unix_error (e, _, _) ->
        `Invalid (sprintf "existing_group: %s: %s" g (Unix.error_message e))
      | Not_found ->
        `Invalid (sprintf "existing_group: %s: group not found" g))
  | _ -> `Invalid "existing_group: not a string"

let list_of validation = function
  | `List l ->
      let rec validate = function
        | [] ->
            `Valid
        | v::vs ->
            match validation v with
            | `Valid -> validate vs
            | `Invalid reason -> `Invalid reason in
      validate l
  | _ ->
      `Invalid "list_of: not a list"
