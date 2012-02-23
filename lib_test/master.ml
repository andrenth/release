open Lwt
open Printf
open Ipc

let rec ipc_handler fd =
  let handler req =
    let s = IpcOps.string_of_request req in
    lwt () = Lwt_log.notice_f "got request: %s" s in
    match req with
    | IpcOps.Req1 pid -> return (IpcOps.Resp1 pid)
    | IpcOps.Req2 pid -> return (IpcOps.Resp2 pid) in
  lwt () = Ipc.handle_request fd handler in
  ipc_handler fd

let () =
  Release.master_slaves
    ~num_slaves:1
    ~background:false
    ~syslog:false
    ~lock_file:(sprintf "/var/run/%s.pid" (Filename.basename Sys.argv.(0)))
    ~ipc_handler:ipc_handler
    ~exec:"/home/andre/ml/daemon/_build/lib_test/test_slave"
    ()
