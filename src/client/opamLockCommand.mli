open OpamTypes
open OpamStateTypes

val select_packages:
  [ `Atom of atom | `Filename of filename | `Dirname of dirname ] list ->
  'a switch_state -> 'a switch_state * package_set
val lock_opam: ?only_direct:bool -> 'a switch_state -> OpamFile.OPAM.t -> OpamFile.OPAM.t
