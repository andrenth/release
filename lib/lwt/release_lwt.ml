module Future : Release_future.S
  with type 'a t = 'a Lwt.t
   and type Unix.file_descr = Lwt_unix.file_descr =
struct
  type +'a t = 'a Lwt.t
  type +'a future = 'a t
  type 'a u = 'a Lwt.u

  (* XXX *)
  let async = Lwt.async
  let catch = Lwt.catch
  let fail = Lwt.fail
  let wait = Lwt.wait
  let iter_p = Lwt_list.iter_p
  let join = Lwt.join
  let pick = Lwt.pick
  let finalize = Lwt.finalize

  module Monad = struct
    let (>>=) = Lwt.bind
    let return = Lwt.return
  end

  module IO = struct
    type input = Lwt_io.input
    type output = Lwt_io.output
    type 'a mode = 'a Lwt_io.mode
    type input_channel = Lwt_io.input_channel
    type output_channel = Lwt_io.output_channel
    type 'a channel = 'a Lwt_io.channel
    type +'a future = 'a Lwt.t

    let flush_all = Lwt_io.flush_all
    let fprintf = Lwt_io.fprintf
    let input = Lwt_io.input
    let output = Lwt_io.output
    let read_line_opt = Lwt_io.read_line_opt
    let with_file mode path f = Lwt_io.with_file ~mode path f
  end

  module Mutex = struct
    type t = Lwt_mutex.t

    let create = Lwt_mutex.create
    let lock = Lwt_mutex.lock
    let unlock = Lwt_mutex.unlock
    let with_lock = Lwt_mutex.with_lock
  end

  module Main = struct
    type 'a sequence = 'a Lwt_sequence.t

    let at_exit = Lwt_main.at_exit
    let exit_hooks = Lwt_main.exit_hooks
    let run = Lwt_main.run
  end

  module Logger = struct
    type t = Lwt_log.logger
    type syslog_facility = Lwt_log.syslog_facility

    let default = Lwt_log.default
    let error s = Lwt_log.error s
    let error_f s = Lwt_log.error_f s
    let notice s = Lwt_log.notice s
    let notice_f s = Lwt_log.notice_f s
    let warning s = Lwt_log.warning s
    let warning_f s = Lwt_log.warning_f s
    let syslog facility = Lwt_log.syslog ~facility ()
  end

  module Process = struct
    type command = Lwt_process.command
    type redirection = Lwt_process.redirection
    type resource_usage = Lwt_unix.resource_usage
    type state = Lwt_process.state

    class process_none = Lwt_process.process_none

    let with_process_none = Lwt_process.with_process_none
  end

  module Unix = struct
    open Lwt

    type file_descr = Lwt_unix.file_descr
    type signal_handler_id = Lwt_unix.signal_handler_id
    type +'a future = 'a Lwt.t

    let accept = Lwt_unix.accept
    let bind = Lwt_unix.bind
    let chdir = Lwt_unix.chdir
    let chroot = Lwt_unix.chroot
    let close = Lwt_unix.close
    let dup = Lwt_unix.dup
    let dup2 = Lwt_unix.dup2
    let fork () =
      Lwt_io.flush_all () >>= fun () ->
      match Lwt_unix.fork () with
      | 0 ->
          return 0
      | pid ->
          Lwt_sequence.iter_node_l Lwt_sequence.remove Lwt_main.exit_hooks;
          return pid
    let getpwnam = Lwt_unix.getpwnam
    let listen = Lwt_unix.listen
    let lstat = Lwt_unix.lstat
    let on_signal = Lwt_unix.on_signal
    let openfile = Lwt_unix.openfile
    let set_close_on_exec = Lwt_unix.set_close_on_exec
    let setsockopt = Lwt_unix.setsockopt
    let sleep = Lwt_unix.sleep
    let socket = Lwt_unix.socket
    let socketpair = Lwt_unix.socketpair
    let stderr = Lwt_unix.stderr
    let stdin = Lwt_unix.stdin
    let stdout = Lwt_unix.stdout
    let unix_file_descr = Lwt_unix.unix_file_descr
    let unlink = Lwt_unix.unlink
  end

  module Bytes = struct
    type t = Lwt_bytes.t
    type file_descr = Lwt_unix.file_descr

    let blit = Lwt_bytes.blit
    let blit_string_bytes src src_pos dst dst_pos len =
      let b = Bytes.of_string src in
      Lwt_bytes.blit_from_bytes b src_pos dst dst_pos len
    let create = Lwt_bytes.create
    let fill = Lwt_bytes.fill
    let get = Lwt_bytes.get
    let length = Lwt_bytes.length
    let of_string = Lwt_bytes.of_string
    let proxy = Lwt_bytes.proxy
    let read = Lwt_bytes.read
    let set = Lwt_bytes.set
    let to_string = Lwt_bytes.to_string
    let write = Lwt_bytes.write
  end
end

module Release = Release.Make (Future)
