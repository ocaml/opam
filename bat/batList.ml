open ExtList

let print ?(first="[") ?(last="]") ?(sep=";") f (BatIO.O oc) l =
  begin
    Buffer.add_string oc first;
    (match l with 
      | x :: xs -> 
        begin
          f (BatIO.O oc) x;
          List.iter (fun x -> 
            begin
              Buffer.add_string oc sep;            
              f (BatIO.O oc) x;
            end) xs;
        end
      | [] -> ());
    Buffer.add_string oc last;
  end

let takewhile_map f = 
  let rec aux acc = function
    | x :: xs -> 
      (match f x with Some x -> aux (x :: acc) xs | None -> List.rev acc)
    | _ -> List.rev acc in
  aux []

module Exceptionless =
struct
  let assoc a l = try Some (List.assoc a l) with Not_found -> None
end
