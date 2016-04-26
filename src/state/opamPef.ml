open OpamTypes
open OpamStd.Op
open OpamPackage.Set.Op
  
let log fmt = OpamConsole.log "PEF" fmt
let slog = OpamConsole.slog
    
open OpamStateTypes

let string_of_atom = function
  |(n,None,[]) -> OpamPackage.Name.to_string n
  |(n,None,fl) -> 
      Printf.sprintf "%s <%s>"
        (OpamPackage.Name.to_string n)
        (String.concat "," fl)
  |(n,Some (op,v),[]) ->
      Printf.sprintf "%s (%s %s)"
        (OpamPackage.Name.to_string n)
        (OpamFormula.string_of_relop op) 
        (OpamPackage.Version.to_string v)
  |(n,Some (op,v),fl) ->
      Printf.sprintf "%s (%s %s) <%s>"
        (OpamPackage.Name.to_string n)
        (OpamFormula.string_of_relop op)
        (OpamPackage.Version.to_string v)
        (String.concat "," fl)

let string_of_cnf string_of_atom cnf =
  let string_of_clause c =
    Printf.sprintf "%s" (OpamStd.List.concat_map " | " string_of_atom (List.rev c)) in
  Printf.sprintf "%s" (OpamStd.List.concat_map " , " string_of_clause (List.rev cnf))

let string_of_conjunction string_of_atom c =
  Printf.sprintf "%s" (OpamStd.List.concat_map " , " string_of_atom (List.rev c))

(* true if the opam package is available on the given switch *)
let available_filter gt opam switch =
  let switch_config = OpamSwitchState.load_switch_config gt switch in
  let switch_global = (gt :> unlocked global_state) in
  OpamFilter.eval_to_bool ~default:false
  (OpamPackageVar.resolve_switch_raw
     switch_global switch switch_config)
  (OpamFile.OPAM.available opam)

let pef_package ?(orphans=OpamPackage.Set.empty) switches gt =
  (* could be done all at once *)
  let all_opams =
    List.fold_left (fun acc switch ->
      OpamSwitchState.with_ `Lock_none gt ~switch @@ fun st ->
	  (* The same package in two switches ? *)
	  OpamPackage.Map.union (fun a _ -> a) st.opams acc
    ) OpamPackage.Map.empty switches
  in
  let all_pinned =
    List.fold_left (fun acc switch ->
      OpamSwitchState.with_ `Lock_none gt ~switch @@ fun st ->
	  OpamSwitch.Map.add switch st.pinned acc
    ) OpamSwitch.Map.empty switches
  in
  let all_installed = 
    List.fold_left (fun acc switch ->
      OpamSwitchState.with_ `Lock_none gt ~switch @@ fun st ->
	  OpamSwitch.Map.add switch st.installed acc
    ) OpamSwitch.Map.empty switches
  in
  let all_reinstall = 
    List.fold_left (fun acc switch ->
      OpamSwitchState.with_ `Lock_none gt ~switch @@ fun st ->
	  OpamSwitch.Map.add switch st.reinstall acc
    ) OpamSwitch.Map.empty switches
  in
  let all_compiler_packages = 
    List.fold_left (fun acc switch ->
      OpamSwitchState.with_ `Lock_none gt ~switch @@ fun st ->
	  OpamSwitch.Map.add switch st.compiler_packages acc
    ) OpamSwitch.Map.empty switches
  in

  let depends   = OpamPackage.Map.map OpamFile.OPAM.depends all_opams in
  let depopts   = OpamPackage.Map.map OpamFile.OPAM.depopts all_opams in
  let conflicts = OpamPackage.Map.map OpamFile.OPAM.conflicts all_opams in
  let maintainers = OpamPackage.Map.map OpamFile.OPAM.maintainer all_opams in
  let to_dnf t =
    let rec and_formula acc = function
	| Atom c -> (c::acc)
	| And(x,y) -> and_formula (and_formula acc y) x
	| Empty | Block _ | Or _ -> assert false
    in
    let rec aux acc = function
	| Empty   -> acc
	| Block _ -> assert false
      | (Atom _ | And _) as t -> (and_formula [] t)::acc
	| Or(x,y) -> aux (aux acc y) x
    in
    aux [] (OpamFormula.dnf_of_formula t)
  in
  let to_cnf t =
    let rec or_formula acc = function
	| Atom c -> (c::acc)
      | Or(x,y) -> or_formula ((or_formula acc y)) x
	| Empty | Block _ | And _ -> assert false
    in
    let rec aux acc = function
	| Empty    -> acc
	| Block _  -> assert false
      | (Atom _ | Or _ ) as t -> (or_formula [] t)::acc
      | And(x,y) -> aux (aux acc y) x
    in
    aux [] (OpamFormula.cnf_of_formula t)
  in
  let formula_of_filtered f =
    let aux n f =
      let dnf_l =
        to_dnf (
          OpamFormula.map (function
            |Constraint c -> Atom (Some c,None)
            |Filter flt -> Atom (None,Some (OpamFilter.to_string flt))
          ) f
        )
      in
      OpamFormula.ands (
        List.map (fun l ->
          let flt =
            List.fold_left (fun acc -> function
              |(None,Some fl) -> fl::acc
              |_ -> acc
            ) [] l
          in
          OpamFormula.ors (
            OpamStd.List.filter_map (function
              |(None,Some f) when List.length l > 1 -> (
                (* Group constraints of the form ocamlfind <build>,ocamlfind (>= 1.3.1) <build> 
                 * into ocamlfind (>= 1.3.1) <build> *)
                (*
                Printf.eprintf "-> %s\n" (string_of_atom (n,None,[f]));
                Printf.eprintf "%s\n" (String.concat "," (List.map (function (c,_) -> string_of_atom (n,c,flt)) l));
                *)
                None 
              )
              |(c,_) -> Some (Atom (c,flt))
            ) l
          )
        ) dnf_l
      )
    in
    List.rev (
      to_cnf (
        OpamFormula.map (fun (n,c) ->
          match aux n c with
          |Empty | Block _ -> (
            (*
              Printf.eprintf "xxx-> %s\n" (string_of_atom (n,None,[]));
            *)
              Atom (n,None,[])
          )
          |ff -> OpamFormula.map (fun (cs,s) -> Atom (n,cs,s)) ff
        ) f
      )
    )
  in
  let aux switches gt package =
    let opam =  OpamPackage.Map.find package all_opams in
    let available =
      match List.filter (available_filter gt opam) switches with
      |[] -> None
      |l -> Some("switch",(String.concat ", " (List.map OpamSwitch.to_string l)))
    in
    match available with
    |None -> None
    |Some _ -> 
      let name = Some("package",(OpamPackage.name_to_string package)) in
      let version = Some("version",(OpamPackage.version_to_string package)) in
      let maintainer =
        try
          let m = OpamPackage.Map.find package maintainers in
          Some("maintainer",(string_of_conjunction (fun a -> a) m))
        with Not_found -> None
      in
      let depends =
        try
          let d = OpamPackage.Map.find package depends in
          let dd = formula_of_filtered d in
          Some ("depends", string_of_cnf string_of_atom dd)
        with Not_found -> None
      in
      let depopts =
        try
          let d = OpamPackage.Map.find package depopts in
          let dd = formula_of_filtered d in
          Some ("depopts", string_of_cnf string_of_atom dd)
        with Not_found -> None
      in
      let conflicts =
        try
          let c = OpamPackage.Map.find package conflicts in
          let n = OpamPackage.name package in
          let cc = (n,None)::(OpamFormula.to_disjunction c) in
          Some("conflicts", string_of_conjunction OpamFormula.string_of_atom cc)
        with Not_found -> None
      in
      let pinned =
        let l =
          List.fold_left (fun acc switch ->
            let pinned = OpamSwitch.Map.find switch all_pinned in
            if OpamPackage.Set.mem package pinned then
              (OpamSwitch.to_string switch) :: acc
            else acc
          ) [] switches
        in
        match l with
        |[] -> None
        |l -> Some("pinned",String.concat ", " l)
      in
      let installed =
        let l =
          List.fold_left (fun acc switch ->
            let installed = OpamSwitch.Map.find switch all_installed in
            if OpamPackage.Set.mem package installed then
              (OpamSwitch.to_string switch) :: acc
            else acc
          ) [] switches
        in
        match l with
        |[] -> None
        |l -> Some("installed",String.concat ", " l)
      in
      let reinstall =
        let l =
          List.fold_left (fun acc switch ->
            let reinstall = OpamSwitch.Map.find switch all_reinstall in
            if OpamPackage.Set.mem package reinstall then
              (OpamSwitch.to_string switch) :: acc
            else acc
          ) [] switches
        in
        match l with
        |[] -> None
        |l -> Some("reinstall",String.concat ", " l)
      in
      let base =
        let l =
          List.fold_left (fun acc switch ->
            let compiler_packages = OpamSwitch.Map.find switch all_compiler_packages in
            if OpamPackage.Set.mem package compiler_packages then
              (OpamSwitch.to_string switch) :: acc
            else acc
          ) [] switches
        in
        match l with
        |[] -> None
        |l -> Some("base",String.concat ", " l)
      in
      Some (
        List.fold_left (fun acc -> function
          |None -> acc
          |Some (k,v) -> (k,(Common.Format822.dummy_loc,v))::acc
        ) [] [name;version;maintainer;available;installed;pinned;base;depends;depopts;conflicts;reinstall]
      )
  in
  let s =
    List.fold_left (fun acc switch ->
      OpamSwitchState.with_ `Lock_none gt ~switch @@ fun st ->
        OpamPackage.Set.union (st.packages ++ st.installed) acc
    ) orphans switches
  in
  OpamPackage.Set.fold (fun p l ->
    match aux switches gt p with
    |None -> l
    |Some par -> par::l
  ) s []
;;

let dump gt oc =
  let switches = OpamFile.Config.installed_switches gt.config in
  List.iter (fun par ->
    List.iter (fun (k,(_,v)) -> Printf.fprintf oc "%s: %s\n" k v) (List.rev par);
    Printf.fprintf oc "\n"
  ) (pef_package switches gt)

let pef_packagelist ?(profiles=[]) switches st =
  let switch = OpamSwitch.to_string st.switch in
  let options = (switch,List.map OpamSwitch.to_string switches,profiles) in
  (* parse_string here is wrong ... It should parse the list XXX *)
  let extras = [("reinstall",Some (Pef.Packages.parse_s Pef.Packages.parse_string))] in 
  List.fold_left (fun acc par ->
    match Opam.Packages.parse_package_stanza options ~extras par with
    |Some pkg -> pkg::acc
    |None -> acc
  ) [] (pef_package switches st.switch_global)

let pef_packageuniv ?(orphans=OpamPackage.Set.empty) ?(profiles=[]) switches st =
  let switch = OpamSwitch.to_string st.switch in
  let options = (switch,List.map OpamSwitch.to_string switches,profiles) in
  let extras = [("reinstall",Some (Pef.Packages.parse_s Pef.Packages.parse_string))] in 
  let h = Hashtbl.create 1024 in
  List.iter (fun par ->
    match Opam.Packages.parse_package_stanza options ~extras par with
    |Some pkg -> Hashtbl.add h (pkg#name,pkg#version) pkg
    |None -> ()
  ) (pef_package ~orphans switches st.switch_global);
  h
