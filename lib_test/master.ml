open Lwt
open Printf
open Ipc

let rec ipc_handler fd =
  let handler req =
    let s = SlaveIpcOps.string_of_request req in
    lwt () = Lwt_log.notice_f "got request: %s" s in
    match req with
    | SlaveIpcOps.Req1 pid -> return (SlaveIpcOps.Resp1 pid)
    | SlaveIpcOps.Req2 pid -> return (SlaveIpcOps.Resp2 pid) in
  lwt () = SlaveIpc.handle_request fd handler in
  ipc_handler fd

let control_connection_handler fd =
  let handler req =
    let s = ControlIpcOps.string_of_request req in
    lwt () = Lwt_log.notice_f "got control request: %s" s in
    return (ControlIpcOps.response_of_string (String.uppercase s)) in
  ControlIpc.handle_request ~timeout:5. fd handler

let () =
  let slave_exec = sprintf "%s/_build/lib_test/test_slave" (Unix.getcwd ()) in
  Release.master_slave
    ~background:false
    ~syslog:false
    ~lock_file:(sprintf "/var/run/%s.pid" (Filename.basename Sys.argv.(0)))
    ~control:("/tmp/master.socket", control_connection_handler)
    ~slave:(slave_exec, ipc_handler)
    ()
