open Printf
open Release_config_types

let validate_global_parameter = function
  | Int i ->
      if i = 0 || i = 1 then `Valid
      else `Invalid ("global_parameter must be a binary number")
  | _ -> `Invalid ("global_parameter must be a binary number")

module ConfigOps = struct
  let spec =
    [ `Global
        [ `Required ("global_parameter", Some validate_global_parameter)
        ; `Optional ("another_global_parameter", None)
        ]
    ; `Required ("my-required-section",
        [ `Required ("my-required-param", None)
        ; `Optional ("my-optional-param", None)
        ])
    ; `Optional ("first-optional-section",
        [ `Optional ("optional-parameter-1", None)
        ; `Optional ("optional-parameter-2", None)
        ])
    ; `Optional ("second-optional-section",
        [ `Required ("required-parameter-1", None)
        ; `Required ("required-parameter-2", None)
        ])
    ]
end

module Config = Release_config.Make(ConfigOps)

let getg c k = Config.get c k ()
let get c s k = Config.get c ~section:s k ()

let some_int i = (Some (Int i))
let some_bool b = (Some (Bool b))
let some_string s = (Some (String s))

let () =
  Config.reset ();
  match Config.parse "./lib_test/complete.conf" with
  | `Configuration conf ->
      assert (getg conf "global_parameter" = some_int 0);
      assert (getg conf "another_global_parameter" = some_bool true);

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
      assert false

let () =
  Config.reset ();
  match Config.parse "./lib_test/missing-optional-values.conf" with
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
  Config.reset ();
  match Config.parse "./lib_test/validation-error.conf" with
  | `Configuration _ -> assert false
  | `Error err -> assert (err = "global_parameter must be a binary number")

let () =
  Config.reset ();
  match Config.parse "./lib_test/missing-required-section.conf" with
  | `Configuration _ -> assert false
  | `Error err -> assert (err = "section 'my-required-section' missing")

let () =
  Config.reset ();
  match Config.parse "./lib_test/missing-required-param.conf" with
  | `Configuration _ -> assert false
  | `Error err -> assert (err = "directive 'required-parameter-1' unspecified")
