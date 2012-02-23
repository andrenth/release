(* OASIS_START *)
(* OASIS_STOP *)
(*flag ["ocaml"; "compile"; "ppopt_lwt_debug"] & S[A"-ppopt"; A"-lwt-debug"];*)
flag ["ocaml"; "compile"; "warn_error"] & S[A"-warn-error"; A"A"];
Ocamlbuild_plugin.dispatch dispatch_default;;
