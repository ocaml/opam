let bind f = function
  | Some x -> f x
  | None -> None
