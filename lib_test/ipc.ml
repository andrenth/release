open Printf

module SlaveIpcOps = struct
  type request = Req1 of int
               | Req2 of int

  type response = Resp1 of int
                | Resp2 of int
                | Broadcast of string

  let string_of_request = function
    | Req1 pid -> sprintf "req1+%d" pid
    | Req2 pid -> sprintf "req2+%d" pid

  let request_of_string s =
    let re = Str.regexp "^req\\([12]\\)\\+\\([0-9]+\\)$" in
    if Str.string_match re s 0 then begin
      let i = Str.matched_group 1 s in
      let pid = Str.matched_group 2 s in
      match i with
      | "1" -> Req1 (int_of_string pid)
      | "2" -> Req2 (int_of_string pid)
      | _ -> failwith (sprintf "bad request: %s" s)
    end else
      failwith (sprintf "bad request: %s" s)

  let string_of_response = function
    | Resp1 pid -> sprintf "resp1+%d" pid
    | Resp2 pid -> sprintf "resp2+%d" pid
    | Broadcast s -> sprintf "broadcast:%s" s

  let response_of_string s =
    let re1 = Str.regexp "^resp\\([12]\\)\\+\\([0-9]+\\)$" in
    let re2 = Str.regexp "^broadcast:\\([a-z]+\\)$" in
    if Str.string_match re1 s 0 then
      let i = Str.matched_group 1 s in
      let pid = Str.matched_group 2 s in
      match i with
      | "1" -> Resp1 (int_of_string pid)
      | "2" -> Resp2 (int_of_string pid)
      | _ -> failwith (sprintf "bad response: %s" s)
    else if Str.string_match re2 s 0 then
      Broadcast (Str.matched_group 1 s)
    else
      failwith (sprintf "bad response: %s" s)
end

module SlaveIpc = Release_ipc.Make (SlaveIpcOps)

module ControlIpcOps = struct
  type request = Req of string
               | Broadcast of string
  type response = Resp of string
                | Broadcast_sent

  let string_of_request = function
    | Req s -> sprintf "%s" s
    | Broadcast s -> sprintf "broadcast:%s" s

  let request_of_string s =
    let re = Str.regexp "^broadcast:\\([a-z]+\\)$" in
    if Str.string_match re s 0 then
      Broadcast (Str.matched_group 1 s)
    else
      Req s

  let string_of_response = function
    | Resp s -> s
    | Broadcast_sent -> "bcastok"

  let response_of_string s =
    if s = "bcastok" then Broadcast_sent else Resp s
end

module ControlIpc = Release_ipc.Make (ControlIpcOps)
