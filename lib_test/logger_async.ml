open Async_extended.Std

let syslog = Log.create `Debug [Log.Output.stdout ()]
