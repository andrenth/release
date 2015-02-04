open Printf
module type S = sig
  type +'a future

  exception Release_privileges_error of string

  val drop : string -> unit future
end

module Make (Future : Release_future.S) : S
  with type 'a future := 'a Future.t =
struct
  module Util = Release_util.Make (Future)

  open Future.Monad
  open Util.Monad

  type +'a future = 'a Future.t

  exception Release_privileges_error of string

  let check_priv name f exp =
    if f () <> exp then
      Future.fail (Release_privileges_error name)
    else
      return_unit

  let drop user =
    let drop_privs () =
      Future.Unix.getpwnam user >>= fun pw ->
      let uid = pw.Unix.pw_uid in
      let gid = pw.Unix.pw_gid in
      let dir = pw.Unix.pw_dir in
      match uid with
      | 0 ->
          Future.fail
            (Release_privileges_error ("cannot drop privileges to root"))
      | _ ->
          Future.Unix.chroot dir >>= fun () ->
          Future.Unix.chdir "/" >>= fun () ->
          Unix.setgroups [|gid|];
          Unix.setgid gid;
          Unix.setuid uid;
          check_priv "getgid" Unix.getgid gid >>= fun () ->
          check_priv "getegid" Unix.getegid gid >>= fun () ->
          check_priv "getuid" Unix.getuid uid >>= fun () ->
          check_priv "geteuid" Unix.geteuid uid >>= fun () ->
          return_unit in
    Future.catch drop_privs
      (function
      | Not_found ->
          Future.fail
            (Release_privileges_error (sprintf "user `%s' not found" user))
      | Unix.Unix_error (e, _, _) ->
          Future.fail (Release_privileges_error (Unix.error_message e))
      | e ->
          Future.fail e)
end
