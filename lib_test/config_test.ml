open Printf
open Release_config_validations

let path = "./lib_test"

let validate_global_parameter = function
  | `Int i ->
      if i = 0 || i = 1 then `Valid
      else `Invalid ("global_parameter must be a binary number")
  | _ -> `Invalid ("global_parameter must be a binary number")

let validate_list_element = function
  | `Int i ->
      if i >= 0 && i <= 5 then `Valid
      else `Invalid ("global-list elements be 0 <= x <= 5")
  | _ -> `Invalid ("global-list must be a list of integers")

let validate_global_list = function
  | `List l ->
      let rec validate = function
        | [] ->
            `Valid
        | x::xs ->
            let res = validate_list_element x in
            if res = `Valid then validate xs else res in
      validate l
  | _ -> `Invalid ("global-list must be a list")

let spec =
  [ `Global
      [ `Required ("global_parameter", [validate_global_parameter])
      ; `Optional ("another_global_parameter", Some (`Bool true), [bool])
      ; `Optional ("global-list", None, [validate_global_list])
      ]
  ; `Required ("my-required-section",
      [ `Required ("my-required-param", [string])
      ; `Optional ("my-optional-param", Some (`Str "opt"), [string])
      ])
  ; `Optional ("first-optional-section",
      [ `Optional ("optional-parameter-1", Some (`Str "opt1"), [string])
      ; `Optional ("optional-parameter-2", Some (`Str "opt2"), [string])
      ])
  ; `Optional ("second-optional-section",
      [ `Required ("required-parameter-1", [string])
      ; `Required ("required-parameter-2", [string])
      ])
  ]

let getg c k = Release_config.get c k ()
let get c s k = Release_config.get c ~section:s k ()

let some_int i = (Some (`Int i))
let some_bool b = (Some (`Bool b))
let some_string s = (Some (`Str s))
let some_int_list l = (Some (`List (List.map (fun x -> `Int x) l)))

let () =
  match Release_config.parse (path ^ "/complete.conf") spec with
  | `Configuration conf ->
      assert (getg conf "global_parameter" = some_int 0);
      assert (getg conf "another_global_parameter" = some_bool true);
      assert (getg conf "global-list" = some_int_list [1;2;3;4]);

      assert (get conf "my-required-section" "my-required-param"
              = some_string "required-value");
      assert (get conf "my-required-section" "my-optional-param"
              = some_string "optional-value");

      assert (get conf "first-optional-section" "optional-parameter-1"
              = some_string "value1");
      assert (get conf "first-optional-section" "optional-parameter-2"
              = some_string "value2");

      assert (get conf "second-optional-section" "required-parameter-1"
              = some_string "value1");
      assert (get conf "second-optional-section" "required-parameter-2"
              = some_string "value2")
  | `Error reason ->
      printf "### %s\n%!" reason;
      assert false

let () =
  match Release_config.parse (path ^ "/missing-optional-values.conf") spec with
  | `Configuration conf ->
      assert (getg conf "global_parameter" = some_int 0);

      assert (get conf "my-required-section" "my-required-param"
              = some_string "required-value");

      assert (get conf "first-optional-section" "optional-parameter-1"
              = some_string "value1");
      assert (get conf "first-optional-section" "optional-parameter-2"
              = some_string "value2");
  | `Error reason ->
      assert false

let () =
  match Release_config.parse (path ^ "/validation-error.conf") spec with
  | `Configuration _ -> assert false
  | `Error err -> assert (err = "global_parameter must be a binary number")

let () =
  match Release_config.parse (path ^ "/missing-required-section.conf") spec with
  | `Configuration _ -> assert false
  | `Error err -> assert (err = "section 'my-required-section' missing")

let () =
  match Release_config.parse (path ^ "/missing-required-param.conf") spec with
  | `Configuration _ ->
      assert false
  | `Error err ->
      assert (err = "configuration directive 'required-parameter-1' missing")
