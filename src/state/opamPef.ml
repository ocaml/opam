
open OpamTypes
open OpamStateTypes
open OpamPackage.Set.Op

let to_atom_ext_formula t =
  let atom (r,v) = Atom (r, v) in
  let atoms (x, c, k) =
    match OpamFormula.cnf_of_formula (OpamFormula.map atom c) with
    | Empty -> Atom (x, None, k)
    | cs    -> OpamFormula.map (fun c -> Atom (x, Some c, k)) cs in
  OpamFormula.map atoms t

let to_cnf t =
  let rec or_formula = function
    | Atom (x,None,k)      -> [k, x, None]
    | Atom (x,Some(r,v),k) -> [k, x, Some(r,v)]
    | Or(x,y)            -> or_formula x @ or_formula y
    | Empty
    | Block _
    | And _      -> assert false in
  let rec aux t = match t with
    | Empty    -> []
    | Block _  -> assert false
    | Atom _
    | Or _     -> [or_formula t]
    | And(x,y) -> aux x @ aux y in
  aux (OpamFormula.cnf_of_formula (to_atom_ext_formula t))

let formula_of_extended f =
  to_cnf (
    OpamFormula.(map (fun (n, (kws,formula)) ->
        OpamFormula.Atom (n, formula, kws))
      ) f)

let string_of_flags l = 
  let aux = function
    | Depflag_Build -> "build"
    | Depflag_Test -> "test"
    | Depflag_Doc -> "doc"
    | Depflag_Dev -> "dev"
    | Depflag_Unknown s -> s
  in match l with
  |[] -> ""
  |l -> Printf.sprintf " <%s>" (String.concat " " (List.map aux l))

let string_of_atom = function
  | fl , n, None ->
    Printf.sprintf "%s%s" (OpamPackage.Name.to_string n) (string_of_flags fl)
  | fl, n, Some (r,c) ->
    Printf.sprintf "%s (%s %s)%s"
      (OpamPackage.Name.to_string n)
      (OpamFormula.string_of_relop r)
      (OpamPackage.Version.to_string c)
      (string_of_flags fl)

let string_of_cnf string_of_atom cnf =
  let string_of_clause c =
    Printf.sprintf "%s" (OpamStd.List.concat_map " | " string_of_atom (List.rev c))
  in
  Printf.sprintf "%s" (OpamStd.List.concat_map " , " string_of_clause (List.rev cnf))

let string_of_conjunction string_of_atom c =
  Printf.sprintf "%s" (OpamStd.List.concat_map " , " string_of_atom (List.rev c))

(* transform all opams (from repo_opams and each switch) to pef.
   We consider only opams available for at least one switch *)
let pef_package switches st =
  let aux switches nv package =
    let opam_name = OpamPackage.name nv in
    let opam_version = OpamPackage.version nv in
    let avail = OpamFile.OPAM.available package in
    let available =
      let filter switch = 
        let env = OpamPackageVar.resolve ~opam:package st switch in
        OpamFilter.eval_to_bool ~default:false env avail
      in
      match List.filter filter (OpamSwitch.Map.keys st.switch_global.aliases) with
      |[] -> None
      |l -> Some("switch",(String.concat ", " (List.map OpamSwitch.to_string l)))
    in
    match available with
    |None -> None
    |Some _ -> 
      let name = Some("package",(OpamPackage.Name.to_string opam_name)) in
      let version = Some("version",(OpamPackage.Version.to_string opam_version)) in
      let maintainer =
        try
          let m = OpamFile.OPAM.maintainer package in
          Some("maintainer",(string_of_conjunction (fun a -> a) m))
        with Not_found -> None
      in
      let depends =
        try
          let d = OpamFile.OPAM.depends package in
          let dd = formula_of_extended d in
          Some ("depends", string_of_cnf string_of_atom dd)
        with Not_found -> None
      in
      let depopts =
        try
          let d = OpamFile.OPAM.depopts package in
          let dd = formula_of_extended d in
          Some ("depopts", string_of_cnf string_of_atom dd)
        with Not_found -> None
      in
      let conflicts =
        try
          let c = OpamFile.OPAM.conflicts package in
          let cc = (opam_name,None)::(OpamFormula.to_disjunction c) in
          Some("conflicts", string_of_conjunction OpamFormula.string_of_atom cc)
        with Not_found -> None
      in
      let pinned =
        let l =
          List.fold_left (fun acc switch ->
            let sst = OpamSwitchState.get_switch st switch in
            if OpamPackage.Name.Map.mem opam_name sst.pinned then
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
            let sst = OpamSwitchState.get_switch st switch in
            if OpamPackage.Set.mem nv sst.installed then
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
            let sst = OpamSwitchState.get_switch st switch in
            if OpamPackage.Set.mem nv sst.reinstall then
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
            let sst = OpamSwitchState.get_switch st switch in
            if OpamPackage.Set.mem nv sst.compiler_packages then
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
        ) [] [name;version;maintainer;available;installed;pinned;
              base;depends;conflicts;depopts;reinstall]
      )
  in
  let to_pef s acc =
    OpamPackage.Map.fold (fun nv opam acc ->
      match aux switches nv opam with
      |None -> acc
      |Some par -> par::acc
    ) s acc
  in
  List.fold_left (fun acc switch ->
    let sst = OpamSwitchState.get_switch st switch in
    to_pef sst.opams acc
  ) (to_pef st.switch_repos.repo_opams []) switches
;;

let dump_state st oc =
  let switches = OpamSwitch.Map.keys st.switch_global.aliases in
  List.iter (fun par ->
    List.iter (fun (k,(_,v)) -> Printf.fprintf oc "%s: %s\n" k v) (List.rev par);
    Printf.fprintf oc "\n"
  ) (pef_package switches st)

let pef_packagelist ?(profiles=[]) switches st =
  let switch = OpamSwitch.to_string st.current_switch in
  let options = (switch,List.map OpamSwitch.to_string switches,profiles) in
  (* parse_string here is wrong ... It should parse the list XXX *)
  let extras = [("reinstall",Some (Pef.Packages.parse_s Pef.Packages.parse_string))] in 
  List.fold_left (fun acc par ->
    match Opam.Packages.parse_package_stanza options ~extras par with
    |Some pkg -> pkg::acc
    |None -> acc
  ) [] (pef_package switches st)

let pef_packageuniv ?(profiles=[]) switches st =
  let switch = OpamSwitch.to_string st.current_switch in
  let options = (switch,List.map OpamSwitch.to_string switches,profiles) in
  let extras = [("reinstall",Some (Pef.Packages.parse_s Pef.Packages.parse_string))] in 
  let h = Hashtbl.create 1024 in
  List.iter (fun par ->
    match Opam.Packages.parse_package_stanza options ~extras par with
    |Some pkg -> Hashtbl.add h (pkg#name,pkg#version) pkg
    |None -> ()
  ) (pef_package switches st);
  h

(** cudf transformation *)

let pefcudf_aux (switch,switches,profiles,depopts,installed) tables pefpkglist =
  let options = { Opam.Opamcudf.switch = switch; switches; profiles; depopts} in
  let cudfpkglist =
    List.map (fun pkg ->
      let reinstallist =
        try OpamStd.String.split (pkg#get_extra "reinstall") ','
        with Not_found -> []
      in
      if (installed && (List.mem switch pkg#installedlist)) || not installed then (
        let pl = Opam.Opamcudf.tocudf tables ~options pkg in
        List.fold_left (fun acc1 p ->
          let sw =
            try Cudf.lookup_package_property p "switch"
            with Not_found -> failwith "mandatory field switch not found"
          in
          let p =
            if List.mem sw reinstallist then
              { p with Cudf.pkg_extra = ("reinstall",`Bool true) :: p.Cudf.pkg_extra }
            else
              p
          in
          p :: acc1
        ) [] pl
      )
      else []
    ) pefpkglist
  in
  List.flatten cudfpkglist

(*
let pefcudflist universe ?(depopts=false) ~build opam_packages =
  let options =
    let (switch,switches,profiles) = universe.u_options in
    log "Pef -> Cudflist (depopts:%b, build:%b, switch:%s, switches:%s)"
    depopts build switch (String.concat "," switches);
    let l = ref profiles in
    if build then l := "build"::!l;
    (switch,switches,!l,depopts,false)
  in
  let pefuniv = universe.u_pefuniv in
  let tables = universe.u_tables in
  let reinstall = match universe.u_action with
    | Upgrade (reinstall , _ )
    | Reinstall (reinstall , _ ) -> reinstall
    | _ -> OpamPackage.Set.empty
  in
  let pefpkglist =
    OpamPackage.Set.fold (fun p acc ->
      let name = OpamPackage.name p in
      let version = OpamPackage.version p in
      let n = OpamPackage.Name.to_string name in
      let v = OpamPackage.Version.to_string version in
      try (Hashtbl.find pefuniv (n,v))::acc
      with Not_found -> acc
    ) opam_packages []
  in
  pefcudf_aux options tables pefpkglist
*)
