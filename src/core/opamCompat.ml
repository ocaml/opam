(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018-2020 OCamlPro                                        *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module String = struct
  include struct
    [@@@warning "-33"]
    include OpamCompatPolyfill.String
    include Stdlib.String

    let exists = exists
  end
end

module Either = struct
  include struct
    [@@@warning "-33"]
    include OpamCompatPolyfill.EitherPolyfill
    include Stdlib

    type ('a, 'b) t = ('a, 'b) Either.t =
      | Left of 'a
      | Right of 'b
  end
end

module Unix = struct
  include struct
    [@@@warning "-33"]
    include OpamCompatPolyfill.Unix
    include Unix

    let realpath = realpath
  end
end

module Lazy = struct
  include struct
    [@@@warning "-33"]
    include OpamCompatPolyfill.Lazy
    include Stdlib.Lazy

    let map = map
  end
end
