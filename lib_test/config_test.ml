open Printf

let path = "./lib_test"

let validate_global_parameter = function
  | `Int i ->
      if i = 0 || i = 1 then `Valid
      else `Invalid ("global_parameter must be a binary number")
  | _ -> `Invalid ("global_parameter must be a binary number")

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

let getg c k = Release_config.get c k ()
let get c s k = Release_config.get c ~section:s k ()

let some_int i = (Some (`Int i))
let some_bool b = (Some (`Bool b))
let some_string s = (Some (`Str s))

let () =
  Release_config.reset ();
  match Release_config.parse (path ^ "/complete.conf") spec with
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
  Release_config.reset ();
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
  Release_config.reset ();
  match Release_config.parse (path ^ "/validation-error.conf") spec with
  | `Configuration _ -> assert false
  | `Error err -> assert (err = "global_parameter must be a binary number")

let () =
  Release_config.reset ();
  match Release_config.parse (path ^ "/missing-required-section.conf") spec with
  | `Configuration _ -> assert false
  | `Error err -> assert (err = "section 'my-required-section' missing")

let () =
  Release_config.reset ();
  match Release_config.parse (path ^ "/missing-required-param.conf") spec with
  | `Configuration _ -> assert false
  | `Error err -> assert (err = "directive 'required-parameter-1' unspecified")