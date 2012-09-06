(***********************************************************************)
(*                                                                     *)
(*    Copyright 2012 OCamlPro                                          *)
(*    Copyright 2012 INRIA                                             *)
(*                                                                     *)
(*  All rights reserved.  This file is distributed under the terms of  *)
(*  the GNU Public License version 3.0.                                *)
(*                                                                     *)
(*  OPAM is distributed in the hope that it will be useful,            *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU General Public License for more details.                       *)
(*                                                                     *)
(***********************************************************************)

let log fmt = Globals.log "TYPES" fmt

module type SET = sig
  include Set.S
  val map: (elt -> elt) -> t -> t
  val choose_one : t -> elt
  val of_list: elt list -> t
  val to_string: t -> string
  val find: (elt -> bool) -> t -> elt
end               
module type MAP = sig
  include Map.S
  val to_string: ('a -> string) -> 'a t -> string
  val values: 'a t -> 'a list
  val merge_max: (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val of_list: (key * 'a) list -> 'a t
end
module type ABSTRACT = sig
  type t
  val of_string: string -> t
  val to_string: t -> string
  module Set: SET with type elt = t
  module Map: MAP with type key = t
end

module type OrderedType = sig
  include Set.OrderedType
  val to_string: t -> string
end

module Set = struct

  module Make (O : OrderedType) = struct

    module S = Set.Make(O)

    include S

    let choose_one s = 
      match elements s with
        | [x] -> x
        | [] -> raise Not_found
        | _  -> invalid_arg "choose_one"

    let of_list l =
      List.fold_left (fun set e -> add e set) empty l

    let to_string s =
      let l = fold (fun nv l -> O.to_string nv :: l) s [] in
      Printf.sprintf "{ %s }" (String.concat ", " l)

    let map f t =
      of_list (List.map f (elements t))

    let find fn s =
      choose (filter fn s)

  end

end

module Map = struct

  module Make (O : OrderedType) = struct

    module M = Map.Make(O)

    include M

    let values map = List.map snd (bindings map)

    let merge_max f = 
      merge
        (fun k -> function 
          | None -> fun x -> x
          | Some o1 -> function
              | None -> Some o1
              | Some o2 -> f k o1 o2)

  let to_string string_of_value m =
    let s (k,v) = Printf.sprintf "%s:%s" (O.to_string k) (string_of_value v) in
    let l = fold (fun k v l -> s (k,v)::l) m [] in
    Printf.sprintf "{ %s }" (String.concat ", " l)

  let of_list l =
    List.fold_left (fun map (k,v) -> add k v map) empty l

  end

end

(* The basic implementation of abstract types is just abstracted
   [string] *)
module Base = struct
  type t = string
  let of_string x = x
  let to_string x = x
  module O = struct
    type t = string
    let to_string = to_string
    let compare = compare
  end
  module Set = Set.Make(O)
  module Map = Map.Make(O)
end



(* Filenames *)

(* Basenames *)
module Basename: ABSTRACT = Base
type basename = Basename.t

(* Absolute directory names *)
module Dirname: sig
  include ABSTRACT
  val cwd: unit -> t
  val rmdir: t -> unit
  val mkdir: t -> unit
  val list: t -> t list
  val in_dir: t -> (unit -> 'a) -> 'a
  val exec: t ->
    ?add_to_env:(string*string) list ->
    ?add_to_path:t list -> string list list -> int
  val chdir: t -> unit
  val move: t -> t -> unit
  val copy: t -> t -> unit
  val dirname: t -> t
  val basename: t -> basename
  val starts_with: prefix:t -> t -> bool
  val remove_prefix: prefix:t -> t -> string
  val exists: t -> bool
  val raw: string -> t
  val with_tmp_dir: (t -> 'a) -> 'a
end = struct

  include Base

  let of_string dirname =
    if not (Filename.is_relative dirname) then
      dirname
    else
      Run.real_path dirname

  let to_string dirname =
    if dirname.[String.length dirname - 1] = Filename.dir_sep.[0] then
      Filename.concat (Filename.dirname dirname) (Filename.basename dirname)
    else
      dirname

  let raw s = s

  let with_tmp_dir fn =
    Run.with_tmp_dir (fun dir -> fn (of_string dir))

  let rmdir dirname =
    Run.remove (to_string dirname)

  let cwd () =
    of_string (Run.cwd ())

  let mkdir dirname =
    Run.mkdir (to_string dirname)

  let list d =
    let fs = Run.directories_with_links (to_string d) in
    List.map of_string fs

  let in_dir dirname fn =
    if Sys.file_exists dirname then
      Run.in_dir dirname fn
    else
      Globals.error_and_exit "%s does not exists!" dirname

  let exec dirname ?(add_to_env=[]) ?(add_to_path=[]) cmds =
    Run.in_dir (to_string dirname) 
      (fun () -> 
        Run.commands
          ~add_to_env
          ~add_to_path:(List.map of_string add_to_path)
          cmds)

  let chdir dirname =
    Run.chdir (to_string dirname)

  let move src dst =
    let err = Run.command [ "mv"; to_string src; to_string dst ] in
    if err <> 0 then
      Globals.exit err

  let copy src dst =
    with_tmp_dir (fun tmp ->
      let err = Run.command [ "rsync"; "-a"; Filename.concat (to_string src) "/"; to_string tmp ] in
      if err <> 0 then
        Globals.exit err;
      match list tmp with
      | [f] ->
          rmdir dst;
          move f dst
      | _ -> Globals.error_and_exit "Error while copying %s to %s" (to_string src) (to_string dst)
    )

  let basename dirname =
    Basename.of_string (Filename.basename (to_string dirname))

  let dirname dirname =
    to_string (Filename.dirname (of_string dirname))

  let exists dirname =
    Sys.file_exists (to_string dirname)

  let starts_with ~prefix dirname =
    let prefix = to_string prefix in
    Utils.starts_with ~prefix (to_string dirname)

  let remove_prefix ~prefix dirname =
    let prefix = 
      let str = to_string prefix in
      if str = "" then "" else Filename.concat str "" in
    let dirname = to_string dirname in
    Utils.remove_prefix ~prefix dirname
end
    
type dirname = Dirname.t

let (/) d1 s2 =
  let s1 = Dirname.to_string d1 in
  Dirname.raw (Filename.concat s1 s2)

(* Raw file contents *)
module Raw: ABSTRACT = Base
type raw = Raw.t

(* Keep a link to [Filename] for the standard library *)
module F = Filename

module Stdlib_filename = F

module Filename: sig
  include ABSTRACT
  val create: dirname -> basename -> t
  val of_basename: basename -> t
  val dirname: t -> dirname
  val basename: t -> basename
  val read: t -> Raw.t
  val remove: t -> unit
  val write: t -> Raw.t -> unit
  val exists: t -> bool
  val check_suffix: t -> string -> bool
  val add_extension: t -> string -> t
  val chop_extension: t -> t
  val list: dirname -> t list
  val rec_list: dirname -> t list
  val with_raw: (Raw.t -> 'a) -> t -> 'a
  val move: t -> t -> unit
  val copy_in: t -> dirname -> unit
  val link_in: t -> dirname -> unit
  val copy: t -> t -> unit
  val link: t -> t -> unit
  val extract: t -> dirname -> unit
  val extract_in: t -> dirname -> unit
  val starts_with: dirname -> t -> bool
  val remove_prefix: prefix:dirname -> t -> string
  val download: t -> dirname -> t option
  val download_iter: t list -> dirname -> t option
  val patch: t -> dirname -> bool
  val digest: t -> Digest.t
  val touch: t -> unit
  val chmod: t -> int -> unit
  val raw: string -> t
end = struct

  type t = {
    dirname:  Dirname.t;
    basename: Basename.t;
  }

  let create dirname basename =
    let b1 = Filename.dirname (Basename.to_string basename) in
    let b2 = Basename.of_string (Filename.basename (Basename.to_string basename)) in
    if basename = b2 then
      { dirname; basename }
    else
      { dirname = dirname / b1; basename = b2 }

  let of_basename basename =
    let dirname = Dirname.of_string "." in
    { dirname; basename }

  let raw str =
    let dirname = Dirname.raw (Filename.dirname str) in
    let basename = Basename.of_string (Filename.basename str) in
    create dirname basename

  let to_string t =
    F.concat (Dirname.to_string t.dirname) (Basename.to_string t.basename)

  let digest t =
    Digest.to_hex (Digest.file (to_string t))

  let touch t =
    Run.write (to_string t) ""

  let chmod t p =
    Unix.chmod (to_string t) p

  let of_string s =
    let dirname = F.dirname s in
    let basename = F.basename s in
    {
      dirname  = Dirname.of_string dirname;
      basename = Basename.of_string basename;
    }

  let dirname t = t.dirname

  let basename t = t.basename

  let read filename =
    let str = Run.read (to_string filename) in
    Raw.of_string str

  let write filename raw =
    Run.write (to_string filename) (Raw.to_string raw)

  let remove filename =
    Run.remove_file (to_string filename)

  let exists filename =
    Sys.file_exists (to_string filename)

  let with_raw fn filename =
    let raw = read filename in
    fn raw

  let check_suffix filename s =
    F.check_suffix (to_string filename) s

  let add_extension filename suffix =
    of_string ((to_string filename) ^ "." ^ suffix)

  let chop_extension filename =
    of_string (F.chop_extension (to_string filename))

  let list d =
    let fs = Run.files_with_links (Dirname.to_string d) in
    List.map of_string fs

  let rec_list d =
    let fs = Run.rec_files (Dirname.to_string d) in
    List.map of_string fs

  let copy src dst =
    Run.copy (to_string src) (to_string dst)

  let move src dst =
    let err = Run.command [ "mv"; to_string src; to_string dst ] in
    if err <> 0 then
      Globals.exit err

  let link src dst =
    if Globals.os = Globals.Win32 then
      copy src dst
    else
      Run.link (to_string src) (to_string dst)

  let process_in fn src dst =
    let src_s = to_string src in
    let dst = F.concat (Dirname.to_string dst) (F.basename src_s) in
    fn src (of_string dst)

  let copy_in = process_in copy

  let link_in = process_in link

  let extract filename dirname =
    Run.extract (to_string filename) (Dirname.to_string dirname)

  let extract_in filename dirname =
    Run.extract_in (to_string filename) (Dirname.to_string dirname)

  let starts_with dirname filename =
    Utils.starts_with (Dirname.to_string dirname) (to_string filename)

  let remove_prefix ~prefix filename =
    let filename = Dirname.raw (to_string filename) in
    Dirname.remove_prefix ~prefix filename

  let download filename dirname =
    Dirname.mkdir dirname;
    match Run.download ~filename:(to_string filename) ~dirname:(Dirname.to_string dirname) with
    | None   -> None
    | Some f -> Some (of_string f)

  let rec download_iter filenames dirname =
    match filenames with
    | []   -> None
    | h::t ->
        match download h dirname with
        | None -> download_iter t dirname
        | x    -> x

  let patch filename dirname =
    Dirname.in_dir dirname (fun () -> Run.patch (to_string filename))

  module O = struct
    type tmp = t
    type t = tmp
    let compare x y = compare (to_string x) (to_string y)
    let to_string = to_string
  end
  module Map = Map.Make(O)
  module Set = Set.Make(O)
end
type filename = Filename.t

type 'a download =
  | Up_to_date of 'a
  | Not_available
  | Result of 'a

type file =
  | D of dirname
  | F of filename

let (//) d1 s2 =
  let d = Stdlib_filename.dirname s2 in
  let b = Stdlib_filename.basename s2 in
  if d <> "." then
    Filename.create (d1 / d) (Basename.of_string b)
  else
    Filename.create d1 (Basename.of_string s2)

(* Package name and versions *)

(* Versions *)
module V: ABSTRACT = Base
type version = V.t

(* Names *)
module N: ABSTRACT = struct
  type t = string
  let of_string x = x
  let to_string x = x
  module O = struct
    type t = string
    let to_string = to_string
    let compare n1 n2 = 
      match compare (String.lowercase n1) (String.lowercase n2) with
        | 0 -> compare n1 n2
        | i -> i
  end
  module Set = Set.Make(O)
  module Map = Map.Make(O)
end

type name = N.t

module NV: sig
  include ABSTRACT
  val name: t -> name
  val version: t -> version
  val create: name -> version -> t
  val of_filename: filename -> t option
  val of_dirname: dirname -> t option
  val of_dpkg: Debian.Packages.package -> t
  val of_cudf: Debian.Debcudf.tables -> Cudf.package -> t
  val to_map: Set.t -> V.Set.t N.Map.t
end = struct

  type t = {
    name   : name;
    version: version;
  }

  let create name version = { name; version }

  let name t = t.name

  let version t = t.version

  let sep = '.'

  let check s =
    if Utils.contains s ' ' || Utils.contains s '\n' then
      None
    else match Utils.cut_at s sep with
      | None        -> None
      | Some (n, v) -> Some { name = N.of_string n; version = V.of_string v }

  let of_string s = match check s with
    | Some x -> x
    | None   -> Globals.error_and_exit "%s is not a valid versioned package name" s

  (* XXX: this function is quite hackish, as it mainly depends on the shape the paths
     built in path.ml *)
  let rec of_filename f =
    let f = Utils.string_strip (Filename.to_string f) in
    if Utils.cut_at f ' ' <> None then
      None
    else begin
      let base = F.basename f in
      let parent = F.basename (F.dirname f) in
      match base with
      | "opam" | "descr" | "url" ->
          check parent
      | _ ->
          if F.check_suffix base ".opam" then
            check (F.chop_suffix base ".opam")
          else if F.check_suffix base "+opam.tar.gz" then
            check (F.chop_suffix base "+opam.tar.gz")
          else
            match parent with
            | "files" ->
                let parent2 = F.basename (F.dirname (F.dirname f)) in
                check parent2
            | _ ->
                (* XXX: handle the case with a deeper files hierarchy *)
                None
    end

  let of_dirname d =
    check (Basename.to_string (Dirname.basename d))

  let of_dpkg d =
    { name    = N.of_string d.Debian.Packages.name;
      version = V.of_string d.Debian.Packages.version }

  let of_cudf table pkg =
    let real_version =
      Debian.Debcudf.get_real_version
        table
        (pkg.Cudf.package, pkg.Cudf.version) in
    { name    = N.of_string (Common.CudfAdd.decode pkg.Cudf.package);
      version = V.of_string real_version; }
    
  let to_string t =
    Printf.sprintf "%s%c%s" (N.to_string t.name) sep (V.to_string t.version)

  module O = struct
    type tmp = t
    type t = tmp
    let compare = compare
    let to_string = to_string
  end
  module Set = Set.Make (O)
  module Map = Map.Make (O)

  let to_map nv =
    Set.fold (fun nv map ->
      let name = name nv in
      let version = version nv in
      let versions =
        if N.Map.mem name map then
          N.Map.find name map
        else
          V.Set.empty in
      N.Map.add name (V.Set.add version versions) (N.Map.remove name map)
    ) nv N.Map.empty

end

type nv = NV.t

type relop = [`Eq|`Geq|`Gt|`Leq|`Lt]

(* OCaml version *)
module OCaml_V: sig
  include ABSTRACT
  val current: unit -> t option
  val compare: t -> relop -> t -> bool
end = struct
  include Base

  let current () =
    match Run.ocaml_version () with
    | None   -> None
    | Some o -> Some (of_string o)

  let compare v1 r v2 =
    let v1 = to_string v1 in
    let v2 = to_string v2 in
    match r with
    | `Eq  -> Debian.Version.equal v1 v2
    | `Geq -> Debian.Version.compare v1 v2 >= 0
    | `Gt  -> Debian.Version.compare v1 v2 > 0
    | `Leq -> Debian.Version.compare v1 v2 <= 0
    | `Lt  -> Debian.Version.compare v1 v2 < 0

end

module Alias: ABSTRACT = Base

(* OPAM version *)
module OPAM_V: ABSTRACT = Base

(* Repositories *)

(* OPAM repositories *)
module Repository: sig
  include ABSTRACT
  val create: name:string -> kind:string -> address:string -> t
  val default: t
  val name: t -> string
  val kind: t -> string
  val address: t -> dirname
  val with_kind: t -> string -> t
end = struct

  type t = {
    name: string;
    kind: string;
    address: dirname;
  }

  let create ~name ~kind ~address =
    let address =
      if Pcre.pmatch (Pcre.regexp "://") address
      || Utils.is_inet_address address then
        Dirname.raw address
      else
        Dirname.of_string (Run.real_path address) in
    { name; kind; address }

  let of_string _ =
    failwith "Use Repository.create instead"

  let name t = t.name

  let kind t = t.kind

  let address t = t.address

  let default = {
    name   = Globals.default_repository_name;
    kind    = Globals.default_repository_kind;
    address = Dirname.raw Globals.default_repository_address;
  }

  let with_kind r kind = { r with kind }

  let to_string r =
    Printf.sprintf "%s(%s %s)" r.name (Dirname.to_string r.address) r.kind

  module O = struct
    type tmp = t
    type t = tmp
    let compare = compare
    let to_string = to_string
  end
  module Set = Set.Make(O)
  module Map = Map.Make(O)

end
type repository = Repository.t

(* Variable names *)

(* Variable names are used in .config files *)
module Variable: sig
  include ABSTRACT
  val installed: t
  val enable: t
end = struct
  include Base
  let installed = of_string "installed"
  let enable = of_string "enable"
end

type variable = Variable.t

type variable_contents =
  | B of bool
  | S of string

let string_of_variable_contents = function
  | B b -> string_of_bool b
  | S s -> s

module Section: sig
  include ABSTRACT
  module G : Graph.Sig.I with type V.t = t
  val graph_iter : (G.V.t -> unit) -> G.t -> unit
end = struct
  include Base
  module C = struct
    include O
    let equal = (=)
    let hash = Hashtbl.hash
  end
  module G = Graph.Imperative.Digraph.ConcreteBidirectional(C)
  module Topo = Graph.Topological.Make (G)
  let graph_iter = Topo.iter
end

type section = Section.t

module Full_section: sig
  include ABSTRACT
  val package: t -> name
  val section: t -> section option
  val create: name -> section -> t
  val all: name -> t
end = struct

  type t = {
    package: name;
    section: section option;
  }

  let create package section =
    { package; section = Some section }

  let all package =
    { package; section = None }

  let package t = t.package

  let section t = t.section

  let of_string str =
    match Utils.cut_at str '.' with
    | Some (n,s) ->
        { package = N.of_string n;
          section = Some (Section.of_string s) }
    | None ->
        { package = N.of_string str;
          section = None }

  let to_string t =
    let n = N.to_string t.package in
    match t.section with
    | None   -> n
    | Some s -> Printf.sprintf "%s.%s" n (Section.to_string s)

  module O = struct
    type tmp = t
    type t = tmp
    let compare = compare
    let to_string = to_string
  end
  module Set = Set.Make (O)
  module Map = Map.Make (O)
end

type full_section = Full_section.t

module Full_variable: sig
  include ABSTRACT
  val create_local: name -> section -> variable -> t
  val create_global: name -> variable -> t
  val package: t -> name
  val section: t -> section option
  val full_section: t -> full_section
  val variable: t -> variable
end = struct

  type t = {
    full_section: full_section;
    variable: variable;
  }

  let variable t = t.variable
  let full_section t = t.full_section
  let section t = Full_section.section t.full_section
  let package t = Full_section.package t.full_section

  let create_local package section variable =
    { full_section = Full_section.create package section;
      variable }

  let create_global package variable =
    { full_section = Full_section.all package;
      variable }

  let of_string s =
      match Utils.rcut_at s ':' with
      | None ->
          create_global
            (N.of_string Globals.default_package)
            (Variable.of_string s)
      | Some (p,v) ->
          let v = Variable.of_string v in
          match Utils.cut_at p '.' with
          | None -> create_global (N.of_string p) v
          | Some (p,s) -> create_local (N.of_string p) (Section.of_string s) v

  let to_string t =
    let package =
      let n = N.to_string (package t) in
      if n = Globals.default_package then
        ""
      else
        n in
    let section = match section t with
      | None   -> ""
      | Some s -> "." ^ Section.to_string s in
    let prefix = package ^ section in
    let prefix =
      if prefix = "" then
        ""
      else
        prefix ^ ":" in
    prefix ^ Variable.to_string t.variable

  module O = struct
    type tmp = t
    type t = tmp
    let compare = compare
    let to_string = to_string
  end
  module Set = Set.Make(O)
  module Map = Map.Make(O)

end

type full_variable = Full_variable.t

type ppflag =
  | Camlp4 of string list
  | Cmd of string list

(* Command line arguments *)

(* Upload arguments *)
type upload = {
  opam   : filename;
  descr  : filename;
  archive: filename;
}

let string_of_upload u =
  Printf.sprintf "opam=%s descr=%s archive=%s"
    (Filename.to_string u.opam)
    (Filename.to_string u.descr)
    (Filename.to_string u.archive)

(* Remote arguments *)
type remote =
  | List
  | Add of repository
  | Rm of string

let string_of_remote = function
  | List  -> "list"
  | Add s -> Printf.sprintf "add %s" (Repository.to_string s)
  | Rm  s -> Printf.sprintf "rm %s" s

type config_option = {
  is_rec : bool;
  is_byte: bool;
  is_link: bool;
  options: full_section list;
}

type pin_option =
  | Version of version
  | Path of dirname
  | Unpin

let pin_option_of_string s =
  let d = Run.real_path s in
  if s = "none" then
    Unpin
  else if Sys.file_exists d then
    Path (Dirname.of_string s)
  else
    Version (V.of_string s)
    
type pin = {
  pin_package: name;
  pin_arg: pin_option;
}

let string_of_pin_option = function
  | Version v -> V.to_string v
  | Path p    -> Dirname.to_string p
  | Unpin     -> "none"

let string_of_pin p =
  Printf.sprintf "{package=%s; arg=%s}"
    (N.to_string p.pin_package)
    (string_of_pin_option p.pin_arg)

type config =
  | Env
  | List_vars
  | Variable of full_variable
  | Includes of bool * (name list)
  | Compil   of config_option
  | Subst    of basename list

let full_sections l =
  String.concat " " (List.map Full_section.to_string l)

let string_of_config_option t =
  Printf.sprintf "rec=%b bytecode=%b link=%b options=%s"
    t.is_rec t.is_byte t.is_link (full_sections t.options)

let string_of_config = function
  | Env        -> "env"
  | List_vars  -> "list-vars"
  | Variable v -> Printf.sprintf "var(%s)" (Full_variable.to_string v)
  | Compil c   -> string_of_config_option c
  | Subst l    -> String.concat "," (List.map Basename.to_string l)
  | Includes (b,l) ->
      Printf.sprintf "include(%b,%s)"
        b (String.concat "," (List.map N.to_string l))

type atom_formula = Debian.Format822.vpkg
type and_formula = atom_formula list
type cnf_formula = Debian.Format822.vpkgformula
type ocaml_constraint = relop * OCaml_V.t

let string_of_atom_formula = function
  | ((n,_), None)       -> n
  | ((n,_), Some (r,c)) -> Printf.sprintf "%s (%s %s)" n r c

module Remote_file: sig
  include ABSTRACT
  val base: t -> basename
  val md5: t -> string
  val perm: t -> int option
  val create: basename -> string -> int -> t
end = struct

  type t = {
    base: basename;
    md5 : string;
    perm: int option;
  }

  let base t = t.base
  let md5 t = t.md5
  let perm t = t.perm

  let create base md5 perm =
    { base; md5; perm=Some perm }

  let to_string t =
    let perm = match t.perm with
      | None   -> ""
      | Some p -> Printf.sprintf " 0o%o" p in
    Printf.sprintf "%s %s%s" (Basename.to_string t.base) t.md5 perm

  let of_string s =
    match Utils.split s ' ' with
    | [base; md5]      -> { base=Basename.of_string base; md5; perm=None }
    | [base;md5; perm] -> { base=Basename.of_string base; md5; perm=Some (int_of_string perm) }
    | k                -> Globals.error_and_exit "Remote_file: %s" (String.concat " " k)

  module O = struct
    type tmp = t
    type t = tmp
    let to_string = to_string
    let compare = compare
  end
  module Set = Set.Make(O)
  module Map = Map.Make(O)
end
