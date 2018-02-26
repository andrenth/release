open Printf

type configuration =
  (string, (string, Release_config_values.t) Hashtbl.t) Hashtbl.t

let configuration : configuration =
  Hashtbl.create 10

let configuration_errors : (string * int) Stack.t = Stack.create ()

let global_section = "#global"

let current_section = ref global_section

let section s =
  current_section := s;
  Hashtbl.replace configuration s (Hashtbl.create 16)

let is_new_section s =
  not (Hashtbl.mem configuration s)

let is_new_key k =
  let section = Hashtbl.find configuration !current_section in
  not (Hashtbl.mem section k)

let add k v =
  Hashtbl.replace (Hashtbl.find configuration !current_section) k v

let add_error err line =
  Stack.push (err, line) configuration_errors

let syntax_error =
  add_error "syntax error"

let duplicate_key key line =
  let section_msg =
    if !current_section = global_section then ""
    else sprintf " in section '%s'" !current_section in
  add_error (sprintf "duplicate key '%s'%s" key section_msg) line

let duplicate_section section line =
  add_error (sprintf "duplicate section '%s'" section) line

let errors () =
  let errs = ref [] in
  Stack.iter (fun e -> errs := e::!errs) configuration_errors;
  !errs

let reset () =
  Hashtbl.clear configuration;
  Stack.clear configuration_errors;
  current_section := global_section;
  section global_section

let copy () =
  let s = Marshal.to_string configuration [] in
  let c = (Marshal.from_string s 0 : configuration) in
  reset ();
  c
