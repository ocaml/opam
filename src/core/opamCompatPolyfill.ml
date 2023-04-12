module String = struct
  include String

  let exists p s =
    let n = length s in
    let rec loop i =
      if i = n then false
      else if p (unsafe_get s i) then true
      else loop (succ i) in
    loop 0
end

module EitherPolyfill = struct
  module Either = struct
    type ('a, 'b) t =
      | Left of 'a
      | Right of 'b
  end
end

module Lazy = struct
  include Lazy

  let map f x =
    lazy (f (force x))
end

module Unix = struct
  include Unix

  let realpath s =
    let getchdir s =
      let p =
        try Sys.getcwd ()
        with Sys_error _ -> Filename.get_temp_dir_name ()
      in
      Unix.chdir s;
      p
    in
    try getchdir (getchdir s) with Unix.Unix_error _ -> s
end
