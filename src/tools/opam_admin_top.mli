(** Small lib for writing opam-repo admin scripts *)

(** The current repo (taken from CWD !) *)
val repo : OpamTypes.repository

(** All defined packages in the current repo *)
val packages : OpamPackage.Set.t

(** All defined compilers in the current repo *)
val compilers : OpamCompiler.Set.t

open OpamFile

type 'a action = [`Update of 'a | `Remove | `Keep ]

(** Maps on the files of every package. Only changed files are written back to
    disk. *)
val iter_packages_gen:
  ?quiet:bool ->
  (OpamPackage.t ->
   prefix:string option ->
   opam:OPAM.t ->
   descr:Descr.t option ->
   url:URL.t option ->
   dot_install:Dot_install.t option ->
   OPAM.t * Descr.t action * URL.t action * Dot_install.t action)
  -> unit

(** Turn a list of glob patterns into a proper filtering function on
    package names. *)
val filter_packages: string list -> (OpamPackage.t -> bool)

(** Quicker interface when considering a single type of file *)
val iter_packages:
  ?quiet:bool ->
  ?filter:(OpamPackage.t -> bool) ->
  ?f:(OpamPackage.t -> string option -> OPAM.t -> unit) ->
  ?opam:(OpamPackage.t -> OPAM.t -> OPAM.t) ->
  ?descr:(OpamPackage.t -> Descr.t -> Descr.t) ->
  ?url:(OpamPackage.t -> URL.t -> URL.t) ->
  ?dot_install:(OpamPackage.t -> Dot_install.t -> Dot_install.t) ->
  unit -> unit

(** Similarly for compiler descriptions *)
val iter_compilers_gen:
  ?quiet:bool ->
  (OpamCompiler.t ->
   prefix:string option ->
   comp:Comp.t ->
   descr:Descr.t option ->
   Comp.t * Descr.t action)
  -> unit

(** Turn a list of glob patterns into a proper filtering function on
    compiler names. *)
val filter_compilers: string list -> (OpamCompiler.t -> bool)

val iter_compilers:
  ?quiet:bool ->
  ?filter:(OpamCompiler.t -> bool) ->
  ?f:(OpamCompiler.t -> string option -> Comp.t -> unit) ->
  ?comp:(OpamCompiler.t -> Comp.t -> Comp.t) ->
  ?descr:(OpamCompiler.t -> Descr.t -> Descr.t) ->
  unit -> unit
