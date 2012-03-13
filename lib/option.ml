let either g f = function
  | None -> g ()
  | Some x -> f x

let some = function
  | None -> failwith "Option.some: None value"
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
