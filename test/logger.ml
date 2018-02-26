let syslog =
  Lwt_log.append_rule "*" Lwt_log.Debug;
  Lwt_log.channel ~close_mode:`Close ~channel:Lwt_io.stdout ()
