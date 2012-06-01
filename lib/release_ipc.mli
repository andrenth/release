(** This module allows one to implement type-safe inter-process communication
    when using Release.

    A simple protocol is assumed. Each IPC message contains a 1-byte header
    followed by a variable length payload. The length of the payload is given
    by the byte in the header, and is therefore limited to 255 bytes.
*)

(** The type of IPC handler functions. *)
type handler = (Lwt_unix.file_descr -> unit Lwt.t)

val control_socket : Lwt_io.file_name -> handler -> unit Lwt.t
  (** [control_socket path handler] creates UNIX domain socket at the
      specified path and sets up an accept loop thread that waits for
      connections on the socket. When a new connection is established,
      [handler] is called on a separate thread. *)

module type Ops = sig
  type request
    (** The type of IPC requests. *)
  type response
    (** The type of IPC responses. *)

  val string_of_request : request -> string
    (** A function that converts a request into a string. *)
  val request_of_string : string -> request
    (** A function that converts a string into a request. *)

  val string_of_response : response -> string
    (** A function that converts a response into a string. *)
  val response_of_string : string -> response
    (** A function that converts a string into a response. *)
end

module type S = sig
  type request
    (** The type of IPC requests. *)
  type response
    (** The type of IPC responses. *)

  val read : ?timeout:float
          -> Lwt_unix.file_descr
          -> [`Data of Release_buffer.t | `EOF | `Timeout] Lwt.t
    (** Reads an IPC message from a file descriptor. The returned buffer
        consists of the payload of the IPC message. *)

  val write : Lwt_unix.file_descr -> Release_buffer.t -> unit Lwt.t
    (** Writes an IPC message to a file descriptor. The message is created
        using the buffer given as the second argument as its payload. *)

  val read_request : ?timeout:float
                  -> Lwt_unix.file_descr
                  -> [`Request of request | `EOF | `Timeout] Lwt.t
    (** Reads an IPC request from a file descriptor. This is a higher-level
        version of {!read} which automatically converts the buffer to a
        request type. *)

  val read_response : ?timeout:float
                   -> Lwt_unix.file_descr
                   -> [`Response of response | `EOF | `Timeout] Lwt.t
    (** Reads an IPC response from a file descriptor. This is a higher-level
        version of {!read} which automatically converts the buffer to a
        response type. *)

  val write_request : Lwt_unix.file_descr -> request -> unit Lwt.t
    (** Writes an IPC request to a file descriptor. This is a higher-level
        version of {!write}, which converts the request to a buffer
        automatically. *)

  val write_response : Lwt_unix.file_descr -> response -> unit Lwt.t
    (** Writes an IPC response to a file descriptor. This is a higher-level
        version of {!write}, which converts the response to a buffer
        automatically. *)

  val make_request : ?timeout:float
                  -> Lwt_unix.file_descr
                  -> request
                  -> ([`Response of response | `EOF | `Timeout] -> 'a Lwt.t)
                  -> 'a Lwt.t
    (** This is a higher-level version of {!write}, which takes a {!request}
        as a parameter and automatically converts it to an IPC message. *)

  val handle_request : ?timeout:float
                    -> Lwt_unix.file_descr
                    -> (request -> response Lwt.t)
                    -> unit Lwt.t
    (** This function reads an IPC {!request} from a file descriptor and
        passes it to a callback function that must return an appropriate
        {!response}. *)
end

module Make (O : Ops) : S
  with type request = O.request and type response = O.response
    (** Functor that builds an implementation of the IPC-handling functions
        given the request and response types and the appropriate string
        conversion functions. *)
