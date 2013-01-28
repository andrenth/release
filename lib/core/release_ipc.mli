(** This module allows one to implement type-safe inter-process communication
    when using Release.

    The UNIX socket used by Release to implement IPC is a resource that can
    be shared by multiple threads. Therefore, if multiple threads in your
    program have access to the IPC file descriptor, it should be protected
    by a lock in order to ensure atomicity.

    A simple protocol is assumed. Each IPC message contains a 4-byte header
    followed by a variable length payload. The length of the payload is given
    by the 4-byte integer in the header, but must fit an OCaml [int].
    Therefore, in 32-bit architectures, an exception might be raise during
    header parsing.
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

  module Server : sig
    (** Module containing functions to be used by an IPC server, that is,
        the master process. *)

    val read_request : ?timeout:float
                    -> Lwt_unix.file_descr
                    -> [`Request of request | `EOF | `Timeout] Lwt.t
      (** Reads an IPC request from a file descriptor. *)

    val write_response : Lwt_unix.file_descr -> response -> unit Lwt.t
      (** Writes an IPC response to a file descriptor. *)

    val handle_request : ?timeout:float
                      -> ?eof_warning:bool
                      -> Lwt_unix.file_descr
                      -> (request -> response Lwt.t)
                      -> unit Lwt.t
      (** This function reads an IPC {!request} from a file descriptor and
          passes it to a callback function that must return an appropriate
          {!response}. *)
  end

  module Client : sig
    (** Module containing functions to be used by an IPC client, that is,
        a slave, helper or control process. *)

    val read_response : ?timeout:float
                     -> Lwt_unix.file_descr
                     -> [`Response of response | `EOF | `Timeout] Lwt.t
      (** Reads an IPC response from a file descriptor. *)

    val write_request : Lwt_unix.file_descr -> request -> unit Lwt.t
      (** Writes an IPC request to a file descriptor. *)

    val make_request : ?timeout:float
                    -> Lwt_unix.file_descr
                    -> request
                    -> ([`Response of response | `EOF | `Timeout] -> 'a Lwt.t)
                    -> 'a Lwt.t
      (** This function sends an IPC {!request} on a file descriptor and
          waits for a {!response}, passing it to a callback function
          responsible for handling it. *)
  end
end

module Make (O : Ops) (B : Release_buffer.S) : S
  with type request = O.request and type response = O.response
    (** Functor that builds an implementation of the IPC-handling functions
        given the request and response types and the appropriate string
        conversion functions, plus the underlying buffer used to implmenet
        data transfers. *)
