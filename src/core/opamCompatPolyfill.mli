module EitherPolyfill : sig
  module Either : sig
      type ('a, 'b) t =
        | Left of 'a
        | Right of 'b
  end
end
