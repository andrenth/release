exception Release_privileges_error of string

val drop : string -> unit Lwt.t
  (** Drop privileges of the current process. This involves the following
      steps:

      + [chroot] to the users's home directory;
      + Change the current working directory to [/];
      + Call [setgroups] on the user's GID;
      + Call [setgid] and [setuid] on the user's GID and UID, respectively;
      + Check if privileges were successfully dropped. *)
