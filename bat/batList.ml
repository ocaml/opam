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

let take_while f = 
  let rec aux acc = function
    | x :: xs when f x -> aux (x :: acc) xs
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
