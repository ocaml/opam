let map f = function
  | Some x -> Some (f x)
  | None -> None

let bind f = function
  | Some x -> f x
  | None -> None
