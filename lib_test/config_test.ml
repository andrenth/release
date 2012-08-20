open Printf
open Release_config_types
open Release_config_validations

let path = "./lib_test"

let validate_global_parameter = function
  | `Int i ->
      if i = 0 || i = 1 then `Valid
      else `Invalid ("global_parameter must be a binary number")
  | _ -> `Invalid ("global_parameter must be a number")

let validate_list_element = function
  | `Int i ->
      if i >= 0 && i <= 5 then `Valid
      else `Invalid ("global-list elements be 0 <= x <= 5")
  | `Str x -> `Invalid ("foo " ^ x)
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

let validate_empty_list = function
  | `List [] -> `Valid
  | `List _ -> `Invalid ("empty list not empty")
  | _ -> `Invalid ("global-list must be a list")

let spec =
  [ `Global
      [ `Required ("global_parameter", [validate_global_parameter])
      ; `Optional ("another_global_parameter", Some (`Bool true), [bool])
      ; `Optional ("global-list", default_int_list [1], [validate_global_list])
      ; `Optional ("empty-list", default_string_list [], [validate_empty_list])
      ; `Optional ("a-regexp", default_regexp (Str.regexp "."), [regexp])
      ]
  ; `Required ("my-required-section",
      [ `Required ("my-required-param", [string])
      ; `Optional ("my-optional-param", default_string "opt", [string])
      ])
  ; `Optional ("first-optional-section",
      [ `Optional ("optional-parameter-1", default_string "opt1", [string])
      ; `Optional ("optional-parameter-2", default_string "opt2", [string])
      ])
  ; `Optional ("second-optional-section",
      [ `Required ("required-parameter-1", [string])
      ; `Required ("required-parameter-2", [string])
      ])
  ]

let getg c k = Release_config.get_exn c k ()
let get c s k = Release_config.get_exn c ~section:s k ()

let () =
  match Release_config.parse (path ^ "/complete.conf") spec with
  | `Configuration conf ->
      assert (getg conf "global_parameter" = `Int 0);
      assert (getg conf "another_global_parameter" = `Bool true);
      assert (getg conf "global-list" = `List [`Int 1; `Int 2; `Int 3; `Int 4]);
      assert (getg conf "empty-list" = `List []);
      assert (Str.string_match (regexp_value (getg conf "a-regexp")) "foo" 0);

      assert (get conf "my-required-section" "my-required-param"
              = `Str "required-value");
      assert (get conf "my-required-section" "my-optional-param"
              = `Str "optional-value");

      assert (get conf "first-optional-section" "optional-parameter-1"
              = `Str "value1");
      assert (get conf "first-optional-section" "optional-parameter-2"
              = `Str "value2");

      assert (get conf "second-optional-section" "required-parameter-1"
              = `Str "value1");
      assert (get conf "second-optional-section" "required-parameter-2"
              = `Str "value2")
  | `Error reason ->
      printf ">>> %s\n%!" reason;
      assert false

let () =
  match Release_config.parse (path ^ "/missing-optional-values.conf") spec with
  | `Configuration conf ->
      assert (getg conf "global_parameter" = `Int 0);
      assert (getg conf "global-list" = `List [`Int 1]);

      assert (get conf "my-required-section" "my-required-param"
              = `Str "required-value");

      assert (get conf "first-optional-section" "optional-parameter-1"
              = `Str "value1");
      assert (get conf "first-optional-section" "optional-parameter-2"
              = `Str "value2");
  | `Error reason ->
      printf ">>> %s\n%!" reason;
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
