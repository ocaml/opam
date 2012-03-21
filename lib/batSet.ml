open ExtList

module Make (O : Set.OrderedType) = 
struct
  include Set.Make (O)

  let to_string ?(first="{") ?(last="}") ?(sep=", ") s set =
    let f oc x =
      BatIO.write_string oc (s x) in
    BatIO.to_string (fun oc set ->
      BatList.print ~first ~last ~sep f oc (elements set)
    ) set

  let of_enum = Enum.fold add empty 

  let of_list l = of_enum (List.enum l)

end
