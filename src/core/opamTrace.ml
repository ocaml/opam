module Json = OpamJson

type state = {
  mutable first_item: bool;
  oc: out_channel;
}

let output : state option ref = ref None

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

(** emit a singe entry *)
let emit_entry_ (self:state) (j:Json.t) =
  if self.first_item then (
    self.first_item <- false;
  ) else (
    output_string self.oc ",\n";
  );
  let str = Json.to_string ~minify:true j in
  output_string self.oc str

let start_time_s_  : float = Unix.gettimeofday () 

(** time in microseconds *)
let[@inline] now_ () : float = (Unix.gettimeofday () -. start_time_s_) *. 1e6

let[@inline] pid () : float = float_of_int @@ Unix.getpid ()
let[@inline] tid () : float = float_of_int 0

let instant_json_ ?(data=[]) msg : Json.t =
  `O [
    "ts", `Float (now_ ()); "pid", `Float (pid()); "tid", `Float (tid());
    "ph", `String "I"; "name", `String msg; "args", `O data
  ] 

let[@inline] instant ?data msg = match !output with
  | None -> ()
  | Some out ->
    let j = instant_json_ ?data msg in
    emit_entry_ out j

let counter_json_ name cnts : Json.t =
  let cnts = List.map (fun (k,v) -> k, `Float (float_of_int v)) cnts in
  `O [
    "ts", `Float (now_ ()); "pid", `Float (pid()); "tid", `Float (tid());
    "ph", `String "c"; "name", `String name; "args", `O cnts
  ] 

let[@inline] counter msg cnts = match !output with
  | None -> ()
  | Some out ->
    let j = counter_json_ msg cnts in
    emit_entry_ out j

let with_span_json_ ?(data=[]) name start : Json.t =
  let stop = now_ () in
  `O [
    "ts", `Float stop; "pid", `Float (pid()); "tid", `Float (tid());
    "ph", `String "X"; "args", `O data;
    "name", `String name; "dur", `Float (stop -. start);
  ] 

let[@inline] with_span ?data name f =
  match !output with
  | None -> f()
  | Some out ->
    let start = now_ () in
    try
      let x = f() in
      let j = with_span_json_ ?data name start in
      emit_entry_ out j;
      x
    with e ->
      let j = with_span_json_ ?data name start in
      emit_entry_ out j;
      raise e
