# Release

## Introduction

Release is a multi-process Lwt-enabled daemon framework for OCaml, providing
facilities for type-safe inter-process communication and privilege-dropping.

Its goal is to make it easy to write servers that are released from the calling
terminal and to release root privileges when those are not necessary.

In this documentation, any mention of a "thread" refers to an `Lwt.t`.

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
-- Pearl Jam, _Release - Master/Slave_  

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

The simple case of a master process and one slave process is implemented in
the function `Release.master_slave`.

    val master_slave : ?background:bool
                    -> ?syslog:bool
                    -> ?privileged:bool
                    -> lock_file:string
                    -> ipc_handler:(Lwt_unix.file_descr -> unit Lwt.t)
                    -> exec:string
                    -> unit -> unit

The `background` argument indicates whether `Release.daemon` will be called.
The `syslog` argument indicates if the syslog facilities from the `Lwt_log`
module will be enabled. If the master process is supposed to run as root, then
the `privileged` argument must be set to `true`.

The `master_slave` function will create a lock file in the path given in the
`lock_file` argument. This file will contain the PID of the master process.
If the lock file already exists and contains the PID of a running process,
the master process will refuse to start.

Inter-process communication is handled in the master process by a callback
function given in the `ipc_handler` argument. This function receives a file
descriptor that is used for communication with the slave process. IPC in
Release will be described in more details below.

The `exec` argument must be a path to an executable file that corresponds to
a program that will be run as the slave process. More about the slave process
will be described below.

The general case of _n_ slave processes is handled by the function
`Release.master_slaves`.

    val master_slaves : ?background:bool
                     -> ?syslog:bool
                     -> ?privileged:bool
                     -> num_slaves:int
                     -> lock_file:string
                     -> ipc_handler:(Lwt_unix.file_descr -> unit Lwt.t)
                     -> exec:string
                     -> unit -> unit

This function works exactly like `Release.master_slave`, but it creates
`num_slaves` slave processes.

### Slaves

> I'll ride the wave  
> Where it takes me  
> I'll hold the pain  
> Release me  
-- Pearl Jam, _Release - Master/Slave_  

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
-- Pearl Jam, _Release - Master/Slave_  

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
