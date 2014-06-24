(** Small lib for writing opam-repo admin scripts *)

(** The current repo (taken from CWD !) *)
val repo : OpamTypes.repository

(** All defined packages in the current repo *)
val packages : OpamPackage.Set.t

(** All defined compilers in the current repo *)
val compilers : OpamCompiler.Set.t

open OpamFile

(** Maps on the files of every package. Only changed files are written back to
    disk. Removes the file if [None] is returned. *)
val map_packages_gen:
  (OpamPackage.t ->
   opam:OPAM.t ->
   descr:Descr.t option ->
   url:URL.t option ->
   dot_install:Dot_install.t option ->
   OPAM.t * Descr.t option * URL.t option * Dot_install.t option)
  -> unit

(** Quicker interface when considering a single type of file *)
val map_packages:
  ?opam:(OPAM.t -> OPAM.t) ->
  ?descr:(Descr.t -> Descr.t) ->
  ?url:(URL.t -> URL.t) ->
  ?dot_install:(Dot_install.t -> Dot_install.t) ->
  unit -> unit

(** Similarly for compiler descriptions *)
val map_compilers_gen:
  (OpamCompiler.t -> comp:Comp.t -> descr:Descr.t option ->
   Comp.t * Descr.t option)
  -> unit

val map_compilers:
  ?comp:(Comp.t -> Comp.t) ->
  ?descr:(Descr.t -> Descr.t) ->
  unit -> unit
