let map f = function
  | Some x -> Some (f x)
  | None -> None
