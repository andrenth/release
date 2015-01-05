open Core.Std
open Async.Std
open Printf

open Release_async
module C = Release.Config

let path = "./lib_test"

let return_unit =
  return ()

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
  let module Default = C.Value.Default in
  let open C.Validation in
  [ `Global
      [ "global_parameter", None, [validate_global_parameter]
      ; "another_global_parameter", Default.bool true, [bool]
      ; "global-list", Default.int_list [1], [validate_global_list]
      ; "empty-list", Default.string_list [], [validate_empty_list]
      ; "a-regexp", Default.regexp (Str.regexp "."), [regexp]
      ]
  ; `Section ("section1",
      [ "my-required-param", None, [string]
      ; "my-optional-param", Default.string "opt", [string]
      ])
  ; `Section ("section2",
      [ "optional-parameter-1", Default.string "opt1", [string]
      ; "optional-parameter-2", Default.string "opt2", [string]
      ])
  ; `Section ("section3",
      [ "optional-parameter-1", Default.string "x", [string]
      ; "optional-parameter-2", Default.string "y", [string]
      ])
  ]

let getg = C.get_global
let get = C.get

let _ =
  let module V = C.Value in
  C.parse (path ^ "/complete.conf") spec >>= function
  | `Configuration conf ->
      assert (getg conf "global_parameter" = `Int 0);
      assert (getg conf "another_global_parameter" = `Bool true);
      assert (getg conf "global-list" = `List [`Int 1;`Int 2;`Int 3;`Int 4]);
      assert (getg conf "empty-list" = `List []);
      assert (Str.string_match (V.to_regexp (getg conf "a-regexp")) "foo" 0);

      assert (get conf "section1" "my-required-param" = `Str"required-value");
      assert (get conf "section1" "my-optional-param" = `Str"optional-value");

      assert (get conf "section2" "optional-parameter-1" = `Str "value1");
      assert (get conf "section2" "optional-parameter-2" = `Str "value2");

      assert (get conf "section3" "optional-parameter-1" = `Str "val\nue1");
      assert (get conf "section3" "optional-parameter-2" = `Str "value2");
      return_unit
  | `Error reason ->
      assert false

let _ =
  C.parse (path ^ "/missing-opt-values.conf") spec >>= function
  | `Configuration conf ->
      assert (getg conf "global_parameter" = `Int 0);
      assert (getg conf "global-list" = `List [`Int 1]);
      assert (get conf "section1" "my-required-param" = `Str"required-value");
      assert (get conf "section2" "optional-parameter-1" = `Str "value1");
      assert (get conf "section2" "optional-parameter-2" = `Str "value2");
      return_unit
  | `Error reason ->
      assert false

let _ =
  C.parse (path ^ "/validation-error.conf") spec >>= function
  | `Configuration _ ->
      assert false
  | `Error err ->
      assert (err = "global_parameter must be a binary number");
      return_unit

let _ =
  C.parse (path ^ "/missing-req-param.conf") spec >>= function
  | `Configuration _ ->
      assert false
  | `Error err ->
      assert (err = "configuration directive 'my-required-param' missing " ^
                    "in section 'section1'");
      return_unit

let _ =
  C.parse (path ^ "/unknown-param.conf") spec >>= function
  | `Configuration _ ->
      assert false
  | `Error err ->
      assert (err = "unknown configuration directive 'foo'");
      return_unit

let optspec =
  let module Default = C.Value.Default in
  let open C.Validation in
  [ `Global
      [ "global_parameter", Default.int 0, [validate_global_parameter]
      ; "another_global_parameter", Default.bool true, [bool]
      ; "global-list", Default.int_list [1], [validate_global_list]
      ; "empty-list", Default.string_list [], [validate_empty_list]
      ; "a-regexp", Default.regexp (Str.regexp "^x$"), [regexp]
      ]
  ; `Section ("section1",
      [ "my-required-param", Default.string "required-value", [string]
      ; "my-optional-param", Default.string "opt", [string]
      ])
  ; `Section ("section2",
      [ "optional-parameter-1", Default.string "opt1", [string]
      ; "optional-parameter-2", Default.string "opt2", [string]
      ])
  ; `Section ("section3",
      [ "optional-parameter-1", Default.string "value1", [string]
      ; "optional-parameter-2", Default.string "value2", [string]
      ])
  ]

let () =
  let module V = C.Value in
  let conf = C.defaults optspec in
  assert (getg conf "global_parameter" = `Int 0);
  assert (getg conf "another_global_parameter" = `Bool true);
  assert (getg conf "global-list" = `List [`Int 1]);
  assert (getg conf "empty-list" = `List []);
  assert (Str.string_match (V.to_regexp (getg conf "a-regexp")) "x" 0);

  assert (get conf "section1" "my-required-param" = `Str "required-value");
  assert (get conf "section1" "my-optional-param" = `Str "opt");

  assert (get conf "section2" "optional-parameter-1" = `Str "opt1");
  assert (get conf "section2" "optional-parameter-2" = `Str "opt2");

  assert (get conf "section3" "optional-parameter-1" = `Str "value1");
  assert (get conf "section3" "optional-parameter-2" = `Str "value2");
  never_returns (Scheduler.go ())
