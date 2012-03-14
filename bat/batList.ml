include List
let of_enum x = x

let filter_map f l =
  List.rev (List.fold_left (fun l x -> match f x with Some x -> x :: l | None -> l) [] l)

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

let take_while_map f = 
  let rec aux acc = function
    | x :: xs -> 
      (match f x with Some x -> aux (x :: acc) xs | None -> List.rev acc)
    | _ -> List.rev acc in
  aux []

let drop_while f = 
  let rec aux = function
    | x :: xs when f x -> aux xs
    | xs -> xs in
  aux

let enum x = x

module Exceptionless =
struct
  let assoc a l = try Some (assoc a l) with Not_found -> None
end

let take_while f l = take_while_map (fun x -> if f x then Some x else None) l
