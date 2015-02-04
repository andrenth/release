open Release_util

module type S = sig
  type t
  type +'a future

  val set : t -> unit
  val debug : ('a, unit, string, unit future) format4 -> 'a
  val info  : ('a, unit, string, unit future) format4 -> 'a
  val error : ('a, unit, string, unit future) format4 -> 'a
end


module Make (Future : Release_future.S) : S
  with type 'a future := 'a Future.t
   and type t := Future.Logger.t =
struct
  open Future.Monad

  let logger = ref Future.Logger.syslog

  let set l =
    logger := l

  let debug fmt =
    Future.Logger.debug !logger fmt

  let info fmt =
    Future.Logger.info !logger fmt

  let error fmt =
    Future.Logger.error !logger fmt
end
