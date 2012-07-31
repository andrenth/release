let either g f = function
  | None -> g ()
  | Some x -> f x

let some = function
  | None -> failwith "Release_option.some: None value"
  | Some x -> x

let default z = function
  | None -> z
  | Some x -> x

let may f = function
  | None -> ()
  | Some x -> f x

let may_default z f = function
  | None -> z
  | Some x -> f x

let map f = function
  | None -> None
  | Some x -> Some (f x)

let choose o1 o2 =
  if o1 = None then o2 else o1

let apply z x = function
  | None -> z
  | Some f -> f x
