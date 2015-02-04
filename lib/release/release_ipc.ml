open Printf

module type S = sig
  type +'a future

  type socket
  type connection
  type handler = connection -> unit future

  val create_connection : socket -> connection
  val control_socket : string -> handler -> unit future

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

    module Server : sig
      val read_request : ?timeout:float
                      -> connection
                      -> [`Request of request | `EOF | `Timeout] future
      val write_response : connection -> response -> unit future
      val handle_request : ?timeout:float
                        -> ?eof_warning:bool
                        -> connection
                        -> (request -> response future)
                        -> unit future
    end

    module Client : sig
      val read_response : ?timeout:float
                       -> connection
                       -> [`Response of response | `EOF | `Timeout] future
      val write_request : connection -> request -> unit future
      val make_request : ?timeout:float
                      -> connection
                      -> request
                      -> ([`Response of response | `EOF | `Timeout] ->
                           'a future)
                      -> 'a future
    end
  end

  module Make (O : Ops) : S
    with type request = O.request and type response = O.response
end

module Make
  (Future : Release_future.S)
  (Buffer : Release_buffer.S
    with type 'a future := 'a Future.t
     and type fd := Future.Unix.fd)
  (Bytes : Release_bytes.S
    with type buffer := Buffer.t)
  (IO : Release_io.S
    with type 'a future := 'a Future.t
     and type buffer := Buffer.t
     and type fd := Future.Unix.fd)
  (Logger : Release_logger.S
    with type 'a future := 'a Future.t
     and type t := Future.Logger.t)
  (Socket : Release_socket.S
    with type 'a future := 'a Future.t
     and type unix = Future.Unix.unix
     and type inet = Future.Unix.inet
     and type addr = Future.Unix.addr
     and type ('state, 'addr) t = ('state, 'addr) Future.Unix.socket) : S
  with type 'a future := 'a Future.t
   and type socket = ([`Active], Future.Unix.unix) Future.Unix.socket =
struct
  module Util = Release_util.Make (Future)

  open Future.Monad
  open Util.Monad

  type socket = ([`Active], Future.Unix.unix) Future.Unix.socket
  type connection = socket * Future.Mutex.t
  type handler = connection -> unit Future.t

  let create_connection sock =
    (sock, Future.Mutex.create ())

  let control_socket path handler =
    let pid = Unix.getpid () in
    Future.catch
      (fun () ->
        let sock = Future.Unix.unix_socket () in
        let addr = `Unix path in
        Socket.accept_loop sock addr
          (fun sock -> handler (create_connection sock)))
      (function
      | Unix.Unix_error (Unix.EADDRINUSE, _, _) ->
          Logger.error
            "control socket `%s' already exists (%d)" path pid >>= fun () ->
          Future.Unix.exit 1
      | Unix.Unix_error (e, _, _) ->
          let err = Unix.error_message e in
          Logger.error
            "control socket `%s' error (%d): %s" path pid err >>= fun () ->
          Future.Unix.exit 1
      | e ->
          let err = Printexc.to_string e in
          Logger.error
            "control socket `%s' error (%d): %s" path pid err >>= fun () ->
          Future.fail e)

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

    module Server : sig
      val read_request : ?timeout:float
                      -> connection
                      -> [`Request of request | `EOF | `Timeout] Future.t

      val write_response : connection -> response -> unit Future.t

      val handle_request : ?timeout:float
                        -> ?eof_warning:bool
                        -> connection
                        -> (request -> response Future.t)
                        -> unit Future.t
    end

    module Client : sig
      val read_response : ?timeout:float
                       -> connection
                       -> [`Response of response | `EOF | `Timeout] Future.t

      val write_request : connection -> request -> unit Future.t

      val make_request : ?timeout:float
                      -> connection
                      -> request
                      -> ([`Response of response | `EOF | `Timeout] ->
                            'a Future.t)
                      -> 'a Future.t
    end
  end

  module Make (O : Ops) : S
    with type request = O.request and type response = O.response =
  struct
    type request = O.request
    type response = O.response

    (*
     * Do the header operations by hand to avoid forcing a
     * dependency on Release_bytes.
     *
     * IPC header is a 4-byte field containing the payload
     * size, stored in network byte order.
     *)

    let header_length = 4

    let read_byte_at i buf =
      int_of_char (Buffer.get buf i)

    exception Overflow

    let close_connection conn =
      Future.Unix.close (Future.Unix.socket_fd (fst conn))

    let read_header =
      Bytes.Big_endian.read_int

    let write_header =
      Bytes.Big_endian.write_int

    let read_header_and_payload ?timeout sock =
      let fd = Future.Unix.socket_fd sock in
      IO.read ?timeout fd header_length >>= function
      | `Timeout | `EOF as other ->
          return other
      | `Data b ->
          try IO.read ?timeout fd (read_header b)
          with Overflow -> Future.fail (Failure "IPC header length overflow")

    let read ?timeout conn =
      let (sock, mutex) = conn in
      Future.Mutex.with_lock mutex
        (fun () -> read_header_and_payload ?timeout sock)

    let write conn buf =
      let len = Buffer.length buf in
      let buf' = Buffer.create (len + header_length) in
      write_header len buf';
      Buffer.blit buf 0 buf' header_length len;
      let fd = Future.Unix.socket_fd (fst conn) in
      IO.write fd buf'

    let request_of_buffer buf =
      O.request_of_string (Buffer.to_string buf)

    let buffer_of_request req =
      Buffer.of_string (O.string_of_request req)

    let response_of_buffer buf =
      O.response_of_string (Buffer.to_string buf)

    let buffer_of_response resp =
      Buffer.of_string (O.string_of_response resp)

    module Server = struct
      let read_request ?timeout sock =
        read ?timeout sock >>= function
        | `Data buf -> return (`Request (request_of_buffer buf))
        | `Timeout | `EOF as other -> return other

      let write_response sock resp =
        write sock (buffer_of_response resp)

      let handle_request ?timeout ?(eof_warning = true) conn handler =
        let rec handle_req () =
          read_request ?timeout conn >>= function
          | `Timeout ->
              close_connection conn >>= fun () ->
              Logger.error "read from a slave shouldn't timeout"
              >>= fun () ->
              Future.Unix.exit 1
          | `EOF ->
              if eof_warning then Logger.error "got EOF on IPC socket"
              else return_unit >>= fun () ->
              close_connection conn
          | `Request req ->
              Future.catch
                (fun () ->
                  handler req >>= fun resp ->
                  write_response conn resp >>= fun () ->
                  handle_req ())
                (fun e ->
                  let err = Printexc.to_string e in
                  Logger.error "request handler exception: %s" err
                  >>= fun () ->
                  close_connection conn) in
        handle_req ()
    end

    module Client = struct
      let read_response ?timeout fd =
        read ?timeout fd >>= function
        | `Data buf -> return (`Response (response_of_buffer buf))
        | `Timeout | `EOF as other -> return other

      let write_request fd req =
        write fd (buffer_of_request req)

      let make_request ?timeout fd req handler =
        write_request fd req >>= fun () ->
        read_response ?timeout fd >>= fun res ->
        handler res
    end
  end
end
