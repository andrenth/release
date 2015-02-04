module Std_unix = Unix
open Async.Std
open Printf
open Ipc_async

open Release_async
open Release.Util

let logger = Logger_async.syslog

let slave_connections = ref None

let return_unit = return ()

let ipc_handler fd =
  let handler req =
    let s = SlaveIpcOps.string_of_request req in
    Log.info logger "got IPC request: %s" s;
    match req with
    | SlaveIpcOps.Req1 pid -> return (SlaveIpcOps.Resp1 pid)
    | SlaveIpcOps.Req2 pid -> return (SlaveIpcOps.Resp2 pid) in
  SlaveIpc.Server.handle_request fd handler

let control_connection_handler fd =
  let handler req =
    match req with
    | ControlIpcOps.Req s ->
        Log.info logger "got control request: %s" s;
        return (ControlIpcOps.response_of_string (String.uppercase s))
    | ControlIpcOps.Broadcast s ->
        Log.info logger "got control broadcast request: %s" s;
        let get_conns = Option.some !slave_connections in
        let slave_conns = get_conns () in
        Deferred.List.iter ~how:`Parallel
          ~f:(fun (_, fd) ->
            SlaveIpc.Server.write_response fd (SlaveIpcOps.Broadcast s))
          slave_conns >>= fun () ->
        return (ControlIpcOps.Broadcast_sent) in
  ControlIpc.Server.handle_request ~timeout:5. fd handler

let store_slave_connections get_conns =
  slave_connections := Some (get_conns);
  return_unit

let () =
  let slave_exec =
    sprintf "%s/_build/lib_test/slave_async.native" (Std_unix.getcwd ()) in
  let helper_exec =
    sprintf "%s/_build/lib_test/helper_async.native" (Std_unix.getcwd ()) in
  let slave_cmd = (slave_exec, [|Filename.basename slave_exec|]) in
  let helper_cmd = (helper_exec, [|Filename.basename helper_exec|]) in
  let exec = Filename.basename Sys.executable_name in
  Release.master_slaves
    ~privileged: false
    ~background: false
    ~logger:     logger
    ~lock_file:  (sprintf "./_build/release_async-%s.pid" exec)
    ~control:    ("./_build/master_async.socket", control_connection_handler)
    ~slave_env:  (`Keep ["OCAMLRUNPARAM"; "RELEASE"])
    ~main:       store_slave_connections
    ~slaves:     [
      slave_cmd,  ipc_handler, 1;
      helper_cmd, (fun _ -> return_unit),  1;
    ]
    ()
