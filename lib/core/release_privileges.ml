open Printf
open Lwt

exception Release_privileges_error of string

let check_priv name f exp =
  if f () <> exp then
    raise_lwt (Release_privileges_error name)
  else
    return_unit

let drop user =
  try_lwt
    lwt pw = Lwt_unix.getpwnam user in
    let uid = pw.Lwt_unix.pw_uid in
    let gid = pw.Lwt_unix.pw_gid in
    let dir = pw.Lwt_unix.pw_dir in
    match uid with
    | 0 ->
        raise_lwt (Release_privileges_error ("cannot drop privileges to root"))
    | _ ->
        lwt () = Lwt_unix.chroot dir in
        lwt () = Lwt_unix.chdir "/" in
        Unix.setgroups [|gid|];
        Unix.setgid gid;
        Unix.setuid uid;
        lwt () = check_priv "getgid" Unix.getgid gid in
        lwt () = check_priv "getegid" Unix.getegid gid in
        lwt () = check_priv "getuid" Unix.getuid uid in
        lwt () = check_priv "geteuid" Unix.geteuid uid in
        return_unit
  with
  | Not_found ->
      raise_lwt (Release_privileges_error (sprintf "user `%s' not found" user))
  | Unix.Unix_error (e, _, _) ->
      raise_lwt (Release_privileges_error (Unix.error_message e))
