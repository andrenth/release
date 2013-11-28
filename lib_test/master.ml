open Lwt
open Printf
open Ipc
open Release_util

let slave_connections = ref None

let ipc_handler fd =
  let handler req =
    let s = SlaveIpcOps.string_of_request req in
    lwt () = Lwt_log.notice_f "got request: %s" s in
    match req with
    | SlaveIpcOps.Req1 pid -> return (SlaveIpcOps.Resp1 pid)
    | SlaveIpcOps.Req2 pid -> return (SlaveIpcOps.Resp2 pid) in
  SlaveIpc.Server.handle_request fd handler

let control_connection_handler fd =
  let handler req =
    match req with
    | ControlIpcOps.Req s ->
        lwt () = Lwt_log.notice_f "got control request: %s" s in
        return (ControlIpcOps.response_of_string (String.uppercase s))
    | ControlIpcOps.Broadcast s ->
        let get_conns = Option.some !slave_connections in
        let slave_conns = get_conns () in
        lwt () = Lwt_list.iter_p
          (fun (_, fd) ->
            SlaveIpc.Server.write_response fd (SlaveIpcOps.Broadcast s))
          slave_conns in
        return (ControlIpcOps.Broadcast_sent) in
  ControlIpc.Server.handle_request ~timeout:5. fd handler

let lwt_ignore _ = return_unit

let store_slave_connections get_conns =
  slave_connections := Some (get_conns);
  return_unit

let () =
  let slave_exec =
    sprintf "%s/_build/lib_test/slave.native" (Unix.getcwd ()) in
  let helper_exec =
    sprintf "%s/_build/lib_test/helper.native" (Unix.getcwd ()) in
  let slave_cmd = (slave_exec, [||]) in
  let helper_cmd = (helper_exec, [||]) in
  Release.master_slaves
    ~privileged:false
    ~background:false
    ~syslog:false
    ~lock_file:(sprintf "./_build/release%s.pid" (Filename.basename Sys.executable_name))
    ~control:("./_build/master.socket", control_connection_handler)
    ~main:store_slave_connections
    ~slaves:[ slave_cmd,  ipc_handler, 1
            ; helper_cmd, lwt_ignore,  1 ]
    ()
