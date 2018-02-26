open Printf
open Lwt.Infix
open Release_util

exception Release_privileges_error of string

let check_priv name f exp =
  if f () <> exp then
    Lwt.fail (Release_privileges_error name)
  else
    Lwt.return_unit

let drop user =
  let drop_privs () =
    Lwt_unix.getpwnam user >>= fun pw ->
    let uid = pw.Unix.pw_uid in
    let gid = pw.Unix.pw_gid in
    let dir = pw.Unix.pw_dir in
    match uid with
    | 0 ->
        Lwt.fail
          (Release_privileges_error ("cannot drop privileges to root"))
    | _ ->
        Lwt_unix.chroot dir >>= fun () ->
        Lwt_unix.chdir "/" >>= fun () ->
        Unix.setgroups [|gid|];
        Unix.setgid gid;
        Unix.setuid uid;
        check_priv "getgid" Unix.getgid gid >>= fun () ->
        check_priv "getegid" Unix.getegid gid >>= fun () ->
        check_priv "getuid" Unix.getuid uid >>= fun () ->
        check_priv "geteuid" Unix.geteuid uid in
  Lwt.catch drop_privs
    (function
    | Not_found ->
        Lwt.fail
          (Release_privileges_error (sprintf "user `%s' not found" user))
    | Unix.Unix_error (e, _, _) ->
        Lwt.fail (Release_privileges_error (Unix.error_message e))
    | e ->
        Lwt.fail e)
