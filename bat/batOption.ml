let bind f = function
  | Some x -> f x
  | None -> None

let print f oc o = 
  BatList.print ~first:(if o = None then "None" else "Some ") ~last:"" ~sep:"" f oc
    (match o with 
      | Some v -> [ v ]
      | None -> [])

let iter f = function
  | Some x -> f x
  | None -> ()
