(** Tracing *)

val setup : trace_file:string option -> unit -> unit

val with_span : ?data:(string * OpamJson.t) list -> string -> (unit -> 'a) -> 'a

val instant : ?data:(string * OpamJson.t) list -> string -> unit

val counter : string -> (string * float) list -> unit

