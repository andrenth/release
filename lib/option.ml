let either g f = function
  | None -> g ()
  | Some x -> f x

let some = function
  | None -> failwith "Option.some: None value"
  | Some x -> x

let with_default z = function
  | None -> z
  | Some x -> x

let may f = function
  | None -> ()
  | Some x -> f x
