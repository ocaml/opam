module String : sig
    include module type of struct include String end
    val exists: (char -> bool) -> string -> bool
end

module EitherPolyfill : sig
  module Either : sig
      type ('a, 'b) t =
        | Left of 'a
        | Right of 'b
  end
end

module Lazy : sig
    include module type of struct include Lazy end
    val map : ('a -> 'b) -> 'a t -> 'b t
end
module Unix : sig
    include module type of struct include Unix end
    val realpath: string -> string
end
