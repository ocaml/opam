type 'a t = {
  l : 'a Lazy.t;
  m : Mutex.t;
}

let create f =
  let l = lazy (f ()) in
  let m = Mutex.create () in
  { l; m }

let from_fun = create

external reraise : exn -> 'a = "%reraise"

let force t =
  let open Mutex in
  lock t.m;
  match Lazy.force t.l with
  | x ->
    unlock t.m;
    x
  | exception e ->
    unlock t.m;
    reraise e

let from_val x =
  let l = Lazy.from_val x in
  let m = Mutex.create () in
  { l; m }

let map f x =
  create (fun () -> f (force x))

let is_val t =
  Mutex.lock t.m;
  let res = Lazy.is_val t.l in
  Mutex.unlock t.m;
  res

let memo_unit f =
  let t = create f in
  fun () -> force t
