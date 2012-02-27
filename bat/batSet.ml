module Make (O : Set.OrderedType) = 
struct
  include Set.Make (O)

  let print ?(first="{") ?(last="}") ?(sep=",") f oc set =
    BatList.print ~first ~last ~sep f oc (elements set)

  let of_enum = List.fold_left (fun set e -> add e set) empty 
end
