(* OpamLazy is a mutex protected thread-safe form of Lazy. *)
type 'a t

(* [create f] is equivalent to [lazy (f ())]. *)
val create : (unit -> 'a) -> 'a t

(* Same as create. *)
val from_fun : (unit -> 'a) -> 'a t

(* [force t] is equivalent to [Lazy.force t]. *)
val force : 'a t -> 'a

(* Identical to [Lazy.from_val]. *)
val from_val : 'a -> 'a t

(* Equivalent to [OpamLazy.create (fun () -> f (OpamLazy.force x))] *)
val map : ('a -> 'b) -> 'a t -> 'b t

(* [is_val t] returns true if t has already been forced and did not raise an exception. *)
val is_val : 'a t -> bool

(* Equivalent to [let t = create f in fun () -> force f] *)
val memo_unit : (unit -> 'a) -> (unit -> 'a)
