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

(* Pervasives extension *)

include OcpPervasives

module List = struct
  include List
  include OcpList
end

module String = struct
  include String
  include OcpString
end

module Stream = struct
  include Stream
  include OcpStream
end

module Genlex = struct
  include Genlex
  include OcpGenlex
end

module Hashtbl = struct
  include Hashtbl
  include OcpHashtbl
end

module Digest = struct
  include Digest
  include OcpDigest
end

