open Printf
open Lwt

exception Release_privileges_error of string

module U = ExtUnix.Specific

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
        U.setresgid gid gid gid;
        U.setresuid uid uid uid;
        return ()
  with
  | Not_found ->
      raise_lwt (Release_privileges_error (sprintf "user `%s' not found" user))
  | Unix.Unix_error (e, _, _) ->
      raise_lwt (Release_privileges_error (Unix.error_message e))
