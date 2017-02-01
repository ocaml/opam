
open OpamTypes
open OpamStateTypes

(** dump the pef universe *)
val dump : unlocked global_state -> out_channel -> unit

(** transform an opam univese into a list of 822 stanzas *)
val pef_package : ?orphans:package_set -> OpamTypes.switch list ->
  unlocked global_state -> Common.Format822.doc

(** transform a list of 822 stanzas into a list of dose3/opam packages *)
val pef_packagelist: ?profiles: string list -> 
  OpamTypes.switch list -> unlocked switch_state -> Opam.Packages.package list

(** transform a list of 822 stanzas into a hashtable of dose3/opam packages 
    indexed by their (name,version) *)
val pef_packageuniv: ?orphans: package_set -> ?profiles: string list -> 
  OpamTypes.switch list  -> unlocked switch_state -> 
    (Pef.Packages_types.name * Pef.Packages_types.version, Opam.Packages.package) Hashtbl.t
