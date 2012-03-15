open ExtList

module Make (O : Set.OrderedType) = 
struct
  include Set.Make (O)

  let print ?(first="{") ?(last="}") ?(sep=",") f oc set =
    BatList.print ~first ~last ~sep f oc (elements set)

  let of_enum = Enum.fold add empty 

  let of_list l = of_enum (List.enum l)
end
