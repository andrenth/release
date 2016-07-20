open Async_extended.Std

let syslog =
  Log.create ~level:`Debug ~output:[Log.Output.stdout ()] ~on_error:`Raise
