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
  ControlIpc.handle_request fd handler

let () =
  Release.master_slaves
    ~num_slaves:1
    ~background:false
    ~syslog:false
    ~lock_file:(sprintf "/var/run/%s.pid" (Filename.basename Sys.argv.(0)))
    ~slave_ipc_handler:ipc_handler
    ~control_socket:("/tmp/master.socket", control_connection_handler)
    ~exec:"/home/andre/ml/daemon/_build/lib_test/test_slave"
    ()
