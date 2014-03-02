(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)

(** Extension of the stdlib List module *)

(***********************************************************************)
(*                                                                     *)
(*                                                                     *)
(*                             CLEAN SECTION                           *)
(*                                                                     *)
(*                                                                     *)
(***********************************************************************)

(** [last l] returns the last elements of [l]. Raise [Not_found] if
    [l] is empty *)
val last : 'a list -> 'a

(** [take n l] returns the [n] first elements of [l] *)
val take : int -> 'a list -> 'a list

(** [drop n l] drops the [n] first elements of [l] *)
val drop : int -> 'a list -> 'a list

(** [make n x] returns a list of [n] times the element [x] *)
val make : int -> 'a -> 'a list

(** Same as {!List.map} but tail recursive *)
val tail_map : ('a -> 'b) -> 'a list -> 'b list









(***********************************************************************)
(*                                                                     *)
(*                                                                     *)
(*                             DIRTY SECTION                           *)
(*                                                                     *)
(*                                                                     *)
(***********************************************************************)

(** [diff a b] returns elements (in [a] and not in [b]) or (in [b] and
    not in [a]). [a] and [b] MUST be sorted *)
val diff : 'a list -> 'a list -> 'a list

(* Fabrice: [sub] is a bad name for this function. [sub list x y] should
   be equivalent to [take y (drop x list)] *)

(** [sub a b] returns elements in [a] and not in [b]. [a] and [b] MUST
    be sorted *)
val sub : 'a list -> 'a list -> 'a list

(** [take_while fn l] returns the first elements of [l] which verify
    the predicate [fn] *)
val take_while : ('a -> bool) -> 'a list ->  'a list

(** [drop_while fn l] drops the first elements of [l] which verify
    the predicate [fn] *)
val drop_while : ('a -> bool) -> 'a list -> 'a list

(** [setify l] removes duplicates in [l] in quadratic time. *)
val setify : 'a list -> 'a list

(** [setify_sorted l] removes duplicates in [l] in linear time. [l]
    MUST be sorted. *)
val setify_sorted : 'a list -> 'a list

(** [union_set l1 l2] makes the union of the two ordered lists [l1] and
    [l2] and removes duplicates. [l1] and [l2] MUST be sorted. *)
val union_set : 'a list -> 'a list -> 'a list

(** [inter_set l1 l2] takes the intersection of two ordered lists [l1] and
    [l2] and removes duplicates. [l1] and [l2] MUST be ordered. *)
val inter_set : 'a list -> 'a list -> 'a list

(** [remove_all_assoc k l] removes all the bindings of [k] in the
    association list [l] *)
val remove_all_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list

(** [replace_assoc k v l] removes any existing binding of [k] in the
    association list [l] and add a binding of [k] to [v] *)
val replace_assoc : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list

(** [assoc_all k l] returns all the values associated with [k] in the
    association list [l] *)
val assoc_all : 'a -> ('a * 'b) list -> 'b list

(** [inv_assoc v l] looks whether the key has a value in [l]. *)
val inv_mem_assoc : 'b -> ('a * 'b) list -> bool

(** [inv_assoc v l] looks for the key which has value [v] in [l]. *)
val inv_assoc : 'b -> ('a * 'b) list -> 'a

(** [rank x l] returns the position of the first occurence of [x] in
    the list [l]. Raise [Not_found] if [x] is not in [l]. *)
val rank : 'a -> 'a list -> int

val filter_map : ('a -> 'b option) -> 'a list -> 'b list

val find_map : ('a -> 'b option) -> 'a list -> 'b

val filter_opt : 'a option list -> 'a list

(** [flatten_map fn l] is equivalent to applying [fn] to all elements
    of the list [l] and then flatten the result. WARNING: the order of
    elements is not kept *)
val flatten_map : ('a -> 'b list) -> 'a list -> 'b list

(** [remove x l] removes all the elements structuraly equal to [x] in
    list [l] *)
val remove : 'a -> 'a list -> 'a list

(** [removeq x l] removes all the elements physically equal to [x] in
    list [l] *)
val removeq : 'a -> 'a list -> 'a list

(** [prefixes l] returns all the prefixes of the list [l] ([l] is
    included) *)
val prefixes : 'a list -> 'a list list

(** [suffixes l] returns all the suffixes of the list [l] ([l] is
    included) *)
val suffixes : 'a list -> 'a list list

exception Invalid_step

(** [range i j] returns the list where elements range from [i] to [j]
    (included). The optional argument [step] can be used to set the
    range step (default is 1). If [i] is greater than [j], then the
    steps are done in decreasing order. Raise [Invalid_step] if the
    optional argument step is lesser or equal to 0.*)
val range : ?step:int -> int -> int -> int list

(** [filter_out pred l] filters out elements of [l] which verify
    [pred] predicates. It is equivalent to [List.filter (fun x -> not
    (pred x)) l]. *)
val filter_out : ('a -> bool) -> 'a list -> 'a list

(** The [intersperse] function takes an element and a list and
    `intersperses' that element between the elements of the list.*)
val intersperse : 'a -> 'a list -> 'a list

(** [intercalate xs xss] is inserts the list [xs] in between the
    elements of [xss] and concatenates the result. *)
val intercalate : pattern:'a list -> 'a list -> 'a list

(** {2 Compatibility } *)

(** Same as {!List.iter}, but the
   function is applied to the index of the element as first argument (counting from 0),
   and the element itself as second argument.
   @since 3.13.0
*)
val iteri : (int -> 'a -> unit) -> 'a list -> unit

(** Same as {!List.map}, but the
   function is applied to the index of the element as first argument (counting from 0),
   and the element itself as second argument.
   @since 3.13.0 *)
val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
