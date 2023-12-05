module Json = OpamJson

type state = {
  mutable first_item: bool;
  oc: out_channel;
}

let output : state option ref = ref None

let start_time_s_ : Mtime.t = Mtime_clock.now ()

(** time in microseconds *)
let[@inline] now_s () : float =
  let t = Mtime_clock.now () in
  (Mtime.span t start_time_s_ |> Mtime.Span.to_float_ns) /. 1e9

let[@inline] now_us () : float =
  let t = Mtime_clock.now () in
  (Mtime.span t start_time_s_ |> Mtime.Span.to_float_ns) /. 1e3

let[@inline] pid () : float = float_of_int @@ Unix.getpid ()
let[@inline] tid () : float = float_of_int 0

let last_gc_ = ref (now_s ())

let counter_json_ name cnts : Json.t =
  let cnts = List.map (fun (k,v) -> k, `Float v) cnts in
  `O [
    "ts", `Float (now_us ()); "pid", `Float (pid()); "tid", `Float (tid());
    "ph", `String "C"; "name", `String name; "args", `O cnts
  ] 

let emit_single_entry_ (self:state) (j:Json.t) =
  if self.first_item then (
    self.first_item <- false;
  ) else (
    output_string self.oc ",\n";
  );
  let str = Json.to_string ~minify:true j in
  output_string self.oc str

let[@inline never] emit_entry_ (self:state) (j:Json.t) =
  emit_single_entry_ self j;

  let now = now_s () in
  if now -. !last_gc_ > 0.2 then (
    (* emit GC stats *)
    last_gc_ := now;
    let minor, major, _ = Gc.counters() in
    let j = counter_json_ "gc" [
      "minor", minor;
      "major", major
    ] in
    emit_single_entry_ self j
  )

let instant_json_ ?(data=[]) msg : Json.t =
  `O [
    "ts", `Float (now_us ()); "pid", `Float (pid()); "tid", `Float (tid());
    "ph", `String "I"; "name", `String msg; "args", `O data
  ] 

let[@inline] instant ?data msg = match !output with
  | None -> ()
  | Some out ->
    let j = instant_json_ ?data msg in
    emit_entry_ out j

let[@inline] counter msg cnts = match !output with
  | None -> ()
  | Some out ->
    let j = counter_json_ msg cnts in
    emit_entry_ out j

let with_span_json_ ?(data=[]) name start : Json.t =
  let stop = now_us () in
  `O [
    "ts", `Float start; "pid", `Float (pid()); "tid", `Float (tid());
    "ph", `String "X"; "args", `O data;
    "name", `String name; "dur", `Float (stop -. start);
  ] 

let[@inline] with_span ?data name f =
  match !output with
  | None -> f()
  | Some out ->
    let start = now_us () in
    try
      let x = f() in
      let j = with_span_json_ ?data name start in
      emit_entry_ out j;
      x
    with e ->
      let j = with_span_json_ ?data name start in
      emit_entry_ out j;
      raise e

(* ## setup ## *)

let setup ~trace_file () : unit =
  match trace_file, Sys.getenv_opt "TRACE_FILE" with
  | None, None -> ()
  | Some file, _
  | None, Some file ->
    let oc = open_out_bin file in
    output_char oc '[';
    at_exit (fun () ->
      output_char oc ']';
      flush oc;
      close_out_noerr oc);
    output := Some {oc; first_item=true}

