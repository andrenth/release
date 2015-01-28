module Std_unix = Unix
open Core.Std
open Async.Std
open Async_extended.Std

module Future : Release_future.S
  with type 'a t = 'a Deferred.t
   and type Unix.fd = Fd.t
   and type ('state, 'addr) Unix.socket = ('state, 'addr) Socket.t =
struct
  type +'a t = 'a Deferred.t
  type +'a future = 'a t

  let async f =
    ignore (Thread_safe.block_on_async_exn f)

  let catch f h =
    try_with ~extract_exn:true f >>= fun r ->
    match r with
    | Ok x -> return x
    | Error e -> h e

  let fail e = raise e

  let finalize f g =
    Monitor.protect f ~finally:g

  let idle = Deferred.never
  let iter_p f t = Deferred.List.iter ~how:`Parallel ~f t
  let join = Deferred.all_unit

  let with_timeout t d =
    Clock.with_timeout (sec t) d

  module Monad = struct
    let (>>=) = Deferred.bind
    let return = Deferred.return
  end

  module IO = struct
    type input_channel = Reader.t
    type output_channel = Writer.t

    let fprintf w fmt = ksprintf (fun s -> return (fprintf w "%s" s)) fmt
    let read_line ch =
      Reader.read_line ch
      >>| function
        | `Ok line -> Some line
        | `Eof -> None
    let with_input_file path f = Reader.with_file path ~f
    let with_output_file path f = Writer.with_file path ~f
  end

  module Mutex = struct
    type t = Mutex.t

    let create = Mutex.create
    let lock m =
      Mutex.lock m;
      return ()
    let unlock = Mutex.unlock
    let with_lock m f =
      lock m >>= fun () ->
      Monitor.protect f ~finally:(fun () -> unlock m; return ())
  end

  module Main = struct
    let at_exit = Shutdown.at_shutdown
    let run (_: 'a future) =
      let _ = Scheduler.go () in
      assert false
  end

  module Logger = struct
    let logger = Log.create `Info [Log.Output.stdout ()]

    let log_to_syslog () = Log.set_output logger [Log.Syslog.output ()]
    let debug s = return (Log.debug logger "%s" s)
    let debug_f fmt = ksprintf (fun s -> debug s) fmt
    let info s = return (Log.info logger "%s" s)
    let info_f fmt = ksprintf (fun s -> info s) fmt
    let error s = return (Log.error logger "%s" s)
    let error_f fmt = ksprintf (fun s -> error s) fmt
  end

  module Unix = struct
    module Core_unix = Core.Std.Unix

    type fd = Fd.t

    type unix = Socket.Address.Unix.t
    type inet = Socket.Address.Inet.t
    type addr = Socket.Address.t
    type ('state, 'addr) socket = ('state, 'addr) Socket.t

    let accept sock =
      Socket.accept sock >>| function
      | `Ok (sock', addr) -> (sock', Socket.Address.to_sockaddr addr)
      | `Socket_closed -> failwith "accept: socket closed"

    let bind sock addr = Socket.bind sock addr

    let chdir = Unix.chdir

    let chroot = Unix.chroot

    let close fd = Unix.close fd

    let dup fd =
      Fd.syscall_in_thread_exn fd ~name:"dup"
        (fun file_descr ->
          Core_unix.dup file_descr)
      >>= fun dup_file_descr ->
      Fd.Kind.infer_using_stat dup_file_descr >>| fun kind ->
      Fd.create kind dup_file_descr (Info.of_string "dup")

    let dup2 src dst =
      In_thread.syscall_exn ~name:"dup2"
        (fun () ->
          Fd.with_file_descr_exn src
            (fun src_file_descr ->
              Fd.with_file_descr_exn dst
                (fun dst_file_descr ->
                  Core_unix.dup2 ~src:src_file_descr ~dst:dst_file_descr)))

    let exit code = Shutdown.exit code

    let fork () =
      In_thread.syscall_exn ~name:"fork"
        (fun () ->
          match Core_unix.fork () with
          | `In_the_child -> 0
          | `In_the_parent pid -> (Pid.to_int pid))

    let getpwnam name =
      Unix.Passwd.getbyname_exn name >>| fun pw ->
      let open Unix.Passwd in
      { Std_unix.pw_name = pw.name
      ; Std_unix.pw_passwd = pw.passwd
      ; Std_unix.pw_uid = pw.uid
      ; Std_unix.pw_gid = pw.gid
      ; Std_unix.pw_gecos = pw.gecos
      ; Std_unix.pw_dir = pw.dir
      ; Std_unix.pw_shell = pw.shell
      }

    let listen sock backlog =
      Socket.listen ~max_pending_connections:backlog sock

    let listen_unix fd backlog =
      listen (Socket.of_fd fd Socket.Type.unix) backlog

    let listen_inet fd backlog =
      listen (Socket.of_fd fd Socket.Type.tcp) backlog

    let lstat path =
      Unix.lstat path >>| fun st ->
      let open Unix.Stats in
      let kind =
        match st.kind with
        | `File -> Std_unix.S_REG
        | `Directory -> Std_unix.S_DIR
        | `Char -> Std_unix.S_CHR
        | `Block -> Std_unix.S_BLK
        | `Link -> Std_unix.S_LNK
        | `Fifo -> Std_unix.S_FIFO
        | `Socket -> Std_unix.S_SOCK in
      { Std_unix.st_dev = st.dev
      ; Std_unix.st_ino = st.ino
      ; Std_unix.st_kind = kind
      ; Std_unix.st_perm = st.perm
      ; Std_unix.st_nlink = st.nlink
      ; Std_unix.st_uid = st.uid
      ; Std_unix.st_gid = st.gid
      ; Std_unix.st_rdev = st.rdev
      ; Std_unix.st_size = Option.value_exn (Int64.to_int st.size)
      ; Std_unix.st_atime = Time.to_epoch st.atime
      ; Std_unix.st_mtime = Time.to_epoch st.mtime
      ; Std_unix.st_ctime = Time.to_epoch st.ctime
      }

    let on_signal signum handler =
      let signal = Signal.of_caml_int signum in
      Signal.handle [signal] (fun s -> handler (Signal.to_caml_int s))

    let openfile file flags perm =
      let convert = function
        | Core_unix.O_RDONLY   -> `Rdonly
        | Core_unix.O_WRONLY   -> `Wronly
        | Core_unix.O_RDWR     -> `Rdwr
        | Core_unix.O_NONBLOCK -> `Nonblock
        | Core_unix.O_APPEND   -> `Append
        | Core_unix.O_CREAT    -> `Creat
        | Core_unix.O_TRUNC    -> `Trunc
        | Core_unix.O_EXCL     -> `Excl
        | Core_unix.O_NOCTTY   -> `Noctty
        | Core_unix.O_DSYNC    -> `Dsync
        | Core_unix.O_SYNC     -> `Sync
        | Core_unix.O_RSYNC    -> `Rsync
        | _                    -> failwith "unsupported open flag" in
      let flags = List.map ~f:convert flags in
      Unix.openfile ~mode:flags ~perm file

    let set_close_on_exec = Unix.set_close_on_exec

    let setsockopt sock opt value =
      let convert = function
        | Std_unix.SO_DEBUG      -> Socket.Opt.debug
        | Std_unix.SO_BROADCAST  -> Socket.Opt.broadcast
        | Std_unix.SO_REUSEADDR  -> Socket.Opt.reuseaddr
        | Std_unix.SO_KEEPALIVE  -> Socket.Opt.keepalive
        | Std_unix.SO_DONTROUTE  -> Socket.Opt.dontroute
        | Std_unix.SO_OOBINLINE  -> Socket.Opt.oobinline
        | Std_unix.SO_ACCEPTCONN -> Socket.Opt.acceptconn
        | Std_unix.TCP_NODELAY   -> Socket.Opt.nodelay
        | Std_unix.IPV6_ONLY     -> failwith "IPV6_ONLY option not supported" in
      Socket.setopt sock (convert opt) value

    let sleep sec =
      Clock.after (Time.Span.of_sec sec) >>| fun _ -> ()

    let socket_fd = Socket.fd

    let socketpair () =
      let fd1, fd2 = Unix.socketpair () in
      (Socket.of_fd fd1 Socket.Type.unix, Socket.of_fd fd2 Socket.Type.unix)

    let unix_socket () =
      Socket.create Socket.Type.unix

    let unix_socket_of_fd fd =
      Socket.of_fd fd Socket.Type.unix

    let stderr = Fd.stderr ()

    let stdin = Fd.stdin ()

    let stdout = Fd.stdout ()

    let unix_file_descr = Fd.file_descr_exn

    let unlink = Unix.unlink

    let waitpid pid =
      Unix.waitpid (Pid.of_int pid) >>| function
      | Ok () -> Std_unix.WEXITED 0
      | Error (`Exit_non_zero s) -> Std_unix.WEXITED s
      | Error (`Signal s) -> Std_unix.WSIGNALED (Signal.to_caml_int s)
  end

  module Bytes = struct
    type t = Bigstring.t
    type fd = Fd.t

    let blit src src_pos dst dst_pos len =
      Bigstring.blit ~src ~src_pos ~len ~dst ~dst_pos
    let blit_string_bytes src src_pos dst dst_pos len =
      Bigstring.From_string.blit ~src ~src_pos ~len ~dst ~dst_pos
    let create n = Bigstring.create n
    let fill buf pos len c =
      let s = String.make len c in
      Bigstring.From_string.blit ~src:s ~src_pos:pos ~dst:buf ~dst_pos:0 ~len
    let get = Bigstring.get
    let length = Bigstring.length
    let of_string s = Bigstring.of_string s
    let proxy buf pos len =
      Bigstring.sub_shared ~pos ~len buf
    let read fd buf pos len =
      let reader = Reader.create fd in
      let substr = Bigsubstring.of_bigstring (proxy buf pos len) in
      Reader.read_bigsubstring reader substr >>| function
      | `Ok n -> n
      | `Eof -> 0
    let set = Bigstring.set
    let to_string b = Bigstring.to_string b
    let write fd buf pos len =
      let writer = Writer.create fd in
      Writer.write_bigstring ~pos ~len writer buf;
      return (Int63.to_int_exn (Writer.bytes_written writer))
  end
end

module Release = Release.Make (Future)
