# Release

## Introduction

Release is a multi-process Lwt-enabled daemon framework for OCaml, providing
facilities for type-safe inter-process communication and privilege-dropping.

Its goal is to make it easy to write servers that are released from the calling
terminal and to release root privileges when those are not necessary.

In this documentation, any mention of a "thread" refers to an `Lwt.t`.

All quotes are from the Pearl Jam song _Release - Master/Slave_ :)

## Installation

After cloning the repository, run the commands below.

    $ make configure
    $ make
    $ make install

## Forking daemons

> Oh dear dad  
> Can you see me now  
> I am myself  
> Like you somehow  

The simplest way to use the library is to simply daemonize a process. Release
provides an Lwt-enabled function to do this in the `Release` module.

    val daemon : (unit -> unit Lwt.t) -> unit Lwt.t

Upon calling `Release.daemon f`, the usual steps to create a daemon
process will be executed.

1.  The `umask` will be set to 0;
2.  `fork` will be called;
3.  The parent process exits;
4.  The child process calls `setsid`, ignores the `SIGHUP` and `SIGPIPE`
    signals, calls `fork` again and exits.
5.  The grandchild process changes the working directory to `/`, redirects
    `stdin`, `stdout` and `stderr` to `/dev/null` and finally calls `f`.

## Master/slave processes

A common pattern when writing multi-process servers is to have a master process
and _n_ slave processes. Usually the master process runs as a privileged user,
while the slaves run unprivileged. This allows one to design a server such
that the bulk of the code runs as a normal user in a slave process, and a
minimum amount of code that actually requires root privileges is implemented
in the master process. Thus, in case of a bug, the likelihood that it can be
exploited with root escalation is considerably decreased.

Consider the type of an inter-process communication callback function, defined
as `Release_ipc.handler`:

    type handler = (Lwt_unix.file_descr -> unit Lwt.t)

The simple case of a master process and one slave process is implemented in
the function `Release.master_slave`.

    val master_slave : slave:(Lwt_io.file_name * Release_ipc.handler)
                    -> ?background:bool
                    -> ?syslog:bool
                    -> ?privileged:bool
                    -> ?control:(Lwt_io.file_name * Release_ipc.handler)
                    -> lock_file:Lwt_io.file_name
                    -> unit -> unit

The `slave` argument is a tuple whose first argument is the path to the slave
process executable. The second argument is a callback function that is used by
the master to handle inter-process communication with the slave. This function
receives the file descriptor to be used as a communication channel with the
slave process. More details about slave processes and IPC in Release will be
described in more details below.

The `background` argument indicates whether `Release.daemon` will be called.
The `syslog` argument indicates if the syslog facilities from the `Lwt_log`
module will be enabled. If the master process is supposed to run as root, then
the `privileged` argument must be set to `true`.

The `master_slave` function will create a lock file in the path given in the
`lock_file` argument. This file will contain the PID of the master process.
If the lock file already exists and contains the PID of a running process,
the master process will refuse to start.

The `control` argument consists of a tuple specifying the path to a Unix socket
and a callback function. The master process will create and listen on this
socket on startup. This is useful for the implementation of control programs
that communicate with the master process.

The general case of _n_ slave processes is handled by the function
`Release.master_slaves`.

    val master_slaves : ?background:bool
                     -> ?syslog:bool
                     -> ?privileged:bool
                     -> ?control:(Lwt_io.file_name * Release_ipc.handler)
                     -> lock_file:Lwt_io.file_name
                     -> slaves:(Lwt_io.file_name * Release_ipc.handler * int) list
                     -> unit -> unit

This function generalizes `Release.master_slave`, allowing the creation of
heterogeneous groups of slave process via the `slaves` argument. This argument
is a list of 3-element tuples containing the path to the slave executables,
the IPC callback function and the number of processes that will be created for
the given executable.

### Slaves

> I'll ride the wave  
> Where it takes me  
> I'll hold the pain  
> Release me  

When a slave process is run, some code must be run in order to setup
communication with the master, and also to drop privileges to a non-root user.
The `Release.me` function deals with this:

    val me : ?syslog:bool
          -> ?user:string
          -> main:(Lwt_unix.file_descr -> unit Lwt.t)
          -> unit -> unit

The `syslog` argument works like in `master_slave`. The `user` argument, if
given, indicates the user whose UID and GID the slave process will set its own
IDs to. This argument can only be given is `privileged` is `true` in the master
process.

The `main` argument is a function that returns the slave's main thread. It
accepts a file descriptor for communication with the master process.

### Inter-process communication

> I'll wait up in the dark  
> For you to speak to me  

Inter-process communication in `Release` is handled in a type-safe manner in
the module `Release_ipc`.

The `Release_ipc.Make` functor receives as an argument a module with the
following signature.

    module type Ops = sig
      type request
      type response
    
      val string_of_request : request -> string
      val request_of_string : string -> request
    
      val string_of_response : response -> string
      val response_of_string : string -> response
    end

The types `request` and `response` correspond to the respective IPC operations,
and the convertion functions of requests and responses to and from strings
must be provided.

The output of `Release_ipc.Make` is a module with the signature below.

    module type S = sig
      type request
      type response
    
      val read : ?timeout:float
              -> Lwt_unix.file_descr
              -> [`Data of string | `EOF | `Timeout] Lwt.t
    
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

The functions `read` and `write` will perform these operations on the IPC file
descriptor given as an argument.

The functions `make_request` and `handle_request` are IPC helpers wrapping
`read` and `write`. A slave process can call `make_request fd req handler` to
make an IPC request to the master process and wait for a response, which will
be passed as an argument to the `handler` callback. The master process can call
`handle_request fd handler` to wait for an IPC request from a slave. This
request will be passed to the callback function, which must return a response
that will be written back to the slave.

#### The IPC protocol

Release uses a very simple IPC protocol. This information is not necessary for
writing applications using this library, but it is useful for creating control
programs or scripts in languages other than OCaml, which can't use Release
(see the `control` argument described above).

Every Release IPC message consists of a 1-byte field containing the length of
the payload portion of the message and a payload field of variable length.
Thus, a message containing the payload string "hello" will have the length byte
set to `5` (the length of the string) and the payload field will be the string
itself.

Higher-level protocols are left for the library users to implement according
to the needs of their respective applications.

## Extras

### The `Release_bytes` module

When writing network-based daemons, the need to implement some kind of binary
protocol is very common. Very often, these protocols have numeric fields that
must be read or written by the application. Since the network I/O functions
take string buffers as arguments, the need to perform conversions from strings
to integers, and conversely, is quite frequent.

The Release library offers the `Release_bytes` module to help in such
conversions. This module contains a set of functions that take a string as an
argument and read or write integers of various sizes at a given offset on the
string.

A list of the functions available in `Release_bytes`, with their respective
signatures, is given below.

* `val read_byte_at : int -> string -> int`
* `val read_byte : string -> int`
* `val write_byte : int -> Buffer.t -> unit`
* `val write_byte32 : int32 -> Buffer.t -> unit`
* `val read_int16_at : int -> string -> int`
* `val read_int16 : string -> int`
* `val write_int16 : int -> Buffer.t -> unit`
* `val read_int_at : int -> string -> int`
* `val read_int : string -> int`
* `val write_int : int -> Buffer.t -> unit`
* `val read_int32_at : int -> string -> int32`
* `val read_int32 : string -> int32`
* `val write_int32 : int32 -> Buffer.t -> unit`

### Control sockets

It may be useful for slaves to have control sockets like the one available
to the master process. This can be useful for writing control programs or for
communication between slave processes. The `Release_ipc` module provides the
`control_socket` function to help setting up the socket and spawning a handler
thread.

    val control_socket : Lwt_io.file_name -> Release_ipc.handler -> unit Lwt.t
