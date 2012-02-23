module type Ops = sig
  type request
  type response

  val string_of_request : request -> string
  val request_of_string : string -> request

  val string_of_response : response -> string
  val response_of_string : string -> response
end

module type S = sig
  type request
  type response

  val read : ?timeout:float
          -> Lwt_unix.file_descr
          -> [> `Data of string | `EOF | `Timeout ] Lwt.t

  val write : Lwt_unix.file_descr -> string -> unit Lwt.t

  val make_request : ?timeout:float
                  -> Lwt_unix.file_descr
                  -> request
                  -> ([`Response of response | `EOF | `Timeout] -> 'a Lwt.t)
                  -> 'a Lwt.t

  val handle_request : Lwt_unix.file_descr
                    -> (request -> response Lwt.t)
                    -> unit Lwt.t
end

module Make (O : Ops) : S
  with type request = O.request and type response = O.response
