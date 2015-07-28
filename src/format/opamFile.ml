(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved.This file is distributed under the terms of the   *)
(*  GNU Lesser General Public License version 3.0 with linking            *)
(*  exception.                                                            *)
(*                                                                        *)
(*  OPAM is distributed in the hope that it will be useful, but WITHOUT   *)
(*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    *)
(*  or FITNESS FOR A PARTICULAR PURPOSE.See the GNU General Public        *)
(*  License for more details.                                             *)
(*                                                                        *)
(**************************************************************************)

open OpamTypes
open OpamTypesBase
open OpamStd.Op

module Pp = OpamFormat.Pp
open Pp.Op

module X = struct


  (** I - Raw text files (no parsing) *)

  (** Compiler and package description files
      (<repo>/packages/.../descr, <repo>/compilers/.../<v>.descr):
      one-line title and content *)

  module Descr = struct

    let internal = "descr"

    type t = string * string

    let empty = "", ""

    let synopsis = fst
    let body = snd

    let full (x,y) = x ^ "\n" ^ y

    let of_channel _ ic =
      let x =
        try input_line ic
        with End_of_file | Sys_error _ -> "" in
      let y =
        try OpamSystem.string_of_channel ic
        with End_of_file | Sys_error _ -> ""
      in
      x, y

    let create str =
      let head, tail =
        match OpamStd.String.cut_at str '\n' with
        | None       -> str, ""
        | Some (h,t) -> h, t in
      head, tail

    let of_string _ = create

    let to_string _ = full

  end

  module Comp_descr = Descr


  (** Raw file interface used for variable expansions ( *.in ) *)

  module Subst = struct

    let internal = "subst"

    type t = string

    let empty = ""

    let of_channel _ ic =
      OpamSystem.string_of_channel ic

    let of_string _ str = str

    let to_string _ t = t

  end



  (** II - Base word list list parser and associated file types *)

  module Lines = struct

    (* Lines of space separated words *)
    type t = string list list

    let empty = []

    let internal = "lines"

    let find_escapes s len =
      let rec aux acc i =
        if i < 0 then acc else
        let acc =
          match s.[i] with
          | '\\' | ' ' | '\t' | '\n' ->
            let esc,count = acc in
            i::esc, count + 1
          | _ -> acc in
        aux acc (i-1) in
      aux ([],0) (len - 1)

    let escape_spaces str =
      let len = String.length str in
      match find_escapes str len with
      | [], _ -> str
      | escapes, n ->
        let buf = Bytes.create (len + n) in
        let rec aux i = function
          | ofs1::(ofs2::_ as r) ->
            Bytes.blit_string str ofs1 buf (ofs1+i) (ofs2-ofs1);
            Bytes.set buf (ofs2+i) '\\';
            aux (i+1) r
          | [ofs] ->
            Bytes.blit_string str ofs buf (ofs+i) (len-ofs);
            buf
          | [] -> assert false
        in
        Bytes.to_string (aux 0 (0::escapes))

    let of_channel (_:filename) ic =
      OpamLineLexer.main (Lexing.from_channel ic)

    let of_string (_:filename) str =
      OpamLineLexer.main (Lexing.from_string str)

    let to_string (_:filename) (lines: t) =
      let buf = Buffer.create 1024 in
      List.iter (fun l ->
          (match l with
           | [] -> ()
           | w::r ->
             Buffer.add_string buf (escape_spaces w);
             List.iter (fun w ->
                 Buffer.add_char buf ' ';
                 Buffer.add_string buf (escape_spaces w))
               r);
          Buffer.add_string buf "\n"
        ) lines;
      Buffer.contents buf

    let pp_string =
      Pp.pp
        (fun ~pos:_ s -> OpamLineLexer.main (Lexing.from_string s))
        (fun lines -> to_string (OpamFilename.of_string "") lines)

    let pp_channel ic oc =
      Pp.pp
        (fun ~pos:_ () -> OpamLineLexer.main (Lexing.from_channel ic))
        (List.iter (function
             | [] -> ()
             | w::r ->
               output_string oc (escape_spaces w);
               List.iter (fun w ->
                   output_char oc ' ';
                   output_string oc (escape_spaces w))
                 r;
               output_char oc '\n'))

  end

  module type LineFileArg = sig
    val internal: string
    type t
    val empty: t
    val pp: (string list list, t) Pp.t
  end

  module LineFile (X: LineFileArg) = struct
    include X

    let to_string _ t = Pp.print (Lines.pp_string -| pp) t

    let of_channel filename ic =
      Pp.parse (Lines.pp_channel ic stdout -| pp) ~pos:(pos_file filename) ()

    let of_string filename str =
      Pp.parse (Lines.pp_string -| pp) ~pos:(filename,0,0) str
  end

  (** (1) Internal usage only *)

  (** Compiler aliases definitions (aliases): table
      <name> <compiler> *)

  module Aliases = LineFile(struct

      let internal = "aliases"

      type t = compiler switch_map

      let empty = OpamSwitch.Map.empty

      let pp =
        OpamSwitch.Map.(Pp.lines_map empty add fold) @@
        Pp.of_module "switch-name" (module OpamSwitch) ^+
        (Pp.last -| Pp.of_module "compiler" (module OpamCompiler))

    end)

  (** Indices of items and their associated source repository: table
      <fullname> <repo-name> <dir-prefix> *)

  module Repo_index (A : OpamStd.ABSTRACT) = LineFile(struct

      let internal = "repo-index"

      type t = (repository_name * string option) A.Map.t

      let empty = A.Map.empty

      let pp =
        Pp.lines_map empty A.Map.safe_add A.Map.fold @@
        Pp.of_module "name" (module A) ^+
        Pp.of_module "repository" (module OpamRepositoryName) ^+
        Pp.opt Pp.last
    end)

  module Package_index = Repo_index(OpamPackage)

  module Compiler_index = Repo_index(OpamCompiler)

  (** List of packages (<switch>/installed, <switch>/installed-roots,
      <switch>/reinstall): table
      <package> <version> *)

  module PkgList = struct

    type t = package_set

    let empty = OpamPackage.Set.empty

    let pp =
      Pp.lines_set empty OpamPackage.Set.add OpamPackage.Set.fold @@
      (Pp.of_module "pkg-name" (module OpamPackage.Name) ^+
       Pp.last -| Pp.of_module "pkg-version" (module OpamPackage.Version))
      -| Pp.pp
        (fun ~pos:_ (n,v) -> OpamPackage.create n v)
        (fun nv -> OpamPackage.name nv, OpamPackage.version nv)

  end

  module Installed = LineFile(struct
      let internal = "installed"
      include PkgList
    end)

  module Installed_roots = LineFile(struct
      let internal = "installed.roots"
      include PkgList
    end)

  module Reinstall = LineFile(struct
      let internal = "reinstall"
      include PkgList
    end)

  (** Lists of pinned packages (<switch>/pinned): table
      <name> <pin-kind> <target> *)

  let pp_pin =
    Pp.pp
      ~name:"?pin-kind pin-target"
      (fun ~pos ->function
         | [x] -> pin_option_of_string x
         | [k;x] -> pin_option_of_string ~kind:(pin_kind_of_string k) x
         | _ -> OpamFormat.bad_format ~pos "Invalid number of fields")
      (fun x -> [string_of_pin_kind (kind_of_pin_option x);
                 string_of_pin_option x])

  module Pinned = LineFile(struct

      let internal = "pinned"

      type t = pin_option OpamPackage.Name.Map.t

      let empty = OpamPackage.Name.Map.empty

      let pp =
        OpamPackage.Name.Map.(Pp.lines_map empty safe_add fold) @@
        Pp.of_module "pkg-name" (module OpamPackage.Name) ^+
        pp_pin

    end)

  (** (2) Part of the public repository format *)

  (** repository index files ("urls.txt"): table
      <filename> <md5> <perms> *)

  module File_attributes = LineFile(struct

      let internal = "file_attributes"

      type t = file_attribute_set

      let empty = OpamFilename.Attribute.Set.empty

      let pp =
        OpamFilename.Attribute.Set.(Pp.lines_set empty add fold) @@
        (Pp.of_module "file" (module OpamFilename.Base) ^+
         Pp.check ~name:"md5" OpamFilename.valid_digest ^+
         Pp.opt (Pp.last -| Pp.of_pair "perm" (int_of_string, string_of_int))
        ) -|
        Pp.pp
          (fun ~pos:_ (base,(md5,perm)) ->
             OpamFilename.Attribute.create base md5 perm)
          (fun att -> OpamFilename.Attribute.(base att, (md5 att, perm att)))

    end)

  (** (3) Available in interface *)

  (** Switch export/import format: table
      <name> <version> <installed-state> [pinning-kind] [pinning-url] *)

  module Export = LineFile(struct

      let internal = "export"

      module M = OpamPackage.Name.Map

      type t = package_set * package_set * pin_option M.t

      let empty = (OpamPackage.Set.empty, OpamPackage.Set.empty, M.empty)

      let pp_state =
        Pp.pp ~name:"pkg-state"
          (fun ~pos:_ -> function
            | "root" -> `Root
            | "noroot" | "installed" -> `Installed
            | "uninstalled" -> `Uninstalled
            | _ -> Pp.unexpected ())
          (function
            | `Root -> "root"
            | `Installed -> "installed"
            | `Uninstalled -> "uninstalled")

      let pp_lines =
        Pp.lines_map M.empty M.safe_add M.fold @@
        Pp.of_module "pkg-name" (module OpamPackage.Name) ^+
        Pp.of_module "pkg-version" (module OpamPackage.Version) ^+
        (Pp.opt (pp_state ^+ Pp.opt pp_pin) -| Pp.default (`Root, None))

      (* Convert from one name-map to set * set * map *)
      let pp =
        pp_lines -| Pp.pp
          (fun ~pos:_ map ->
             M.fold
               (fun name (version,(state,pin)) (installed,roots,pinned) ->
                  let nv = OpamPackage.create name version in
                  (match state with
                   | `Installed | `Root -> OpamPackage.Set.add nv installed
                   | `Uninstalled -> installed),
                  (match state with
                   | `Root -> OpamPackage.Set.add nv roots
                   | `Installed | `Uninstalled -> roots),
                  (match pin with
                   | Some pin -> M.add name pin pinned
                   | None -> pinned))
               map
               (OpamPackage.Set.empty, OpamPackage.Set.empty, M.empty))
          (fun (installed,roots,pinned) ->
             M.empty |>
             OpamPackage.Set.fold (fun nv ->
                 M.add (OpamPackage.name nv)
                   (OpamPackage.version nv, (`Installed, None)))
               installed |>
             OpamPackage.Set.fold (fun nv ->
                 M.add (OpamPackage.name nv)
                   (OpamPackage.version nv, (`Root, None)))
               roots |>
             M.fold (fun name pin map ->
                 try
                   let v, (state, _) = M.find name map in
                   M.add name (v, (state, Some pin)) map
                 with Not_found ->
                   let v = OpamPackage.Version.of_string "--" in
                   M.add name (v, (`Uninstalled, Some pin)) map)
               pinned)

  end)



  (** III - Opam Syntax parser and associated file types *)

  module Syntax = struct

    type t = file

    let of_channel (filename:filename) (ic:in_channel) =
      let lexbuf = Lexing.from_channel ic in
      let filename = OpamFilename.to_string filename in
      lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with
                                    Lexing.pos_fname = filename };
      OpamParser.main OpamLexer.token lexbuf filename

    let of_string (filename:filename) str =
      let lexbuf = Lexing.from_string str in
      let filename = OpamFilename.to_string filename in
      lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with
                                    Lexing.pos_fname = filename };
      OpamParser.main OpamLexer.token lexbuf filename

    let to_string (t: t) =
      OpamFormat.string_of_file ~simplify:true t

    let s_opam_version = "opam-version"

    let check_opam_version =
      let already_warned = ref false in
      fun ?(allow_major=false) f v ->
        let diff_full = OpamVersion.(compare current v) in
        if diff_full >= 0 then true else
        let diff_major = OpamVersion.(compare (major current) (major v)) in
        if OpamFormatConfig.(!r.strict) then
          OpamConsole.error_and_exit
            "Strict mode: %s refers to OPAM %s, this is %s."
            f.file_name (OpamVersion.to_string v) OpamVersion.(to_string current);
        if diff_major < 0 && not allow_major then
          OpamFormat.bad_format
            ~pos:OpamFormat.(assoc f.file_contents s_opam_version value_pos)
            "Can't read OPAM %s files yet, this is OPAM %s."
            (OpamVersion.to_string v) OpamVersion.(to_string current);
        if not (!already_warned) then
          OpamConsole.note
            "File %s is written for OPAM %s, and this is %s.\n\
             It may depend on new features, consider upgrading."
            f.file_name (OpamVersion.to_string v) OpamVersion.(to_string current);
        already_warned := true;
        false

    (* Prints warnings or fails in strict mode; returns true if permissive mode
       is enabled *)
    let check ?allow_major ?(versioned=true) ?allow_extensions =
      fun f fields ->
        if not OpamFormatConfig.(!r.strict) && not (OpamConsole.debug ())
        then true
        else
        let f_opam_version =
          if List.mem s_opam_version fields then
            OpamFormat.assoc_option f.file_contents s_opam_version
              (OpamFormat.parse_string @> OpamVersion.of_string)
          else None
        in
        (* Reading a file with a newer minor version triggers permissive
           mode: silently ignore new fields and try to be more tolerant *)
        let permissive_mode = match f_opam_version with
          | Some v -> not (check_opam_version ?allow_major f v)
          | None ->
            if versioned then (
              if OpamFormatConfig.(!r.strict) then
                OpamConsole.error_and_exit
                  "Strict mode: %s missing the opam-version field"
                  OpamFilename.(prettify (of_string f.file_name));
              OpamConsole.warning
                "%s is missing the 'opam-version:' field."
                OpamFilename.(prettify (of_string f.file_name));
              true
            ) else false
        in
        if not permissive_mode &&
           not (OpamFormat.is_valid f.file_contents fields) then
          let invalids =
            OpamFormat.invalid_fields ?allow_extensions f.file_contents fields
          in
          let too_many, invalids =
            List.partition (fun x -> List.mem x fields) invalids
          in
          if too_many <> [] then
            OpamConsole.warning "In %s:\n  duplicate fields %s"
              f.file_name
              (OpamStd.List.to_string (fun x -> x) too_many);
          let is_, s_ =
            if List.length invalids <= 1 then "is an", "" else "are", "s" in
          if invalids <> [] then
            OpamConsole.warning "In %s:\n  %s %s unknown field%s."
              f.file_name
              (OpamStd.Format.pretty_list invalids)
              is_ s_;
          if OpamFormatConfig.(!r.strict) then
            OpamConsole.error_and_exit "Strict mode: bad fields in %s"
              f.file_name;
          false
        else permissive_mode

    let to_1_0 file =
      let file_contents = List.map (function
          | Variable (pos, v, _) as c ->
            if v = s_opam_version then
              Variable (pos,s_opam_version, OpamFormat.make_string "1")
            else c
          | c -> c
        ) file.file_contents in
      { file with file_contents; file_format = OpamVersion.of_string "1" }

  end

  (** (1) Internal files *)

  (** General opam configuration (config) *)

  module Config = struct

    let internal = "config"

    type t = {
      opam_version : opam_version;
      repositories : repository_name list ;
      switch : switch;
      jobs : int;
      dl_tool : arg list option;
      dl_jobs : int;
      solver_criteria : (solver_criteria * string) list;
      solver : arg list option;
    }

    let opam_version t = t.opam_version
    let repositories t = t.repositories
    let switch t = t.switch
    let jobs t = t.jobs
    let dl_tool t = t.dl_tool
    let dl_jobs t = t.dl_jobs
    let criteria t = t.solver_criteria
    let criterion kind t =
      try Some (List.assoc kind t.solver_criteria)
      with Not_found -> None
    let solver t = t.solver

    let with_opam_version t opam_version = { t with opam_version }
    let with_repositories t repositories = { t with repositories }
    let with_switch t switch = { t with switch }
    let with_jobs t jobs = { t with jobs }
    let with_dl_tool t dl_tool = { t with dl_tool = Some dl_tool }
    let with_dl_jobs t dl_jobs = { t with dl_jobs }
    let with_criteria t solver_criteria = { t with solver_criteria }
    let with_criterion kind t criterion =
      { t with solver_criteria =
                 (kind,criterion)::List.remove_assoc kind t.solver_criteria }
    let with_solver t solver = { t with solver = Some solver }

    let create switch repositories ?(criteria=[]) ?solver jobs ?download_tool dl_jobs =
      { opam_version = OpamVersion.current;
        repositories ; switch ; jobs ; dl_tool = download_tool; dl_jobs ;
        solver_criteria = criteria; solver }

    let empty = {
      opam_version = OpamVersion.current;
      repositories = [];
      switch = OpamSwitch.of_string "<empty>";
      jobs = 1;
      dl_tool = None;
      dl_jobs = 1;
      solver_criteria = [];
      solver = None;
    }

    let fields =
      let with_switch t sw =
        if t.switch = empty.switch then with_switch t sw
        else OpamFormat.bad_format "Multiple switch specifications"
      in
      [
        "opam-version", Pp.ppacc
          with_opam_version opam_version
          (Pp.V.string -| Pp.of_module "opam-version" (module OpamVersion));
        "repositories", Pp.ppacc
          with_repositories repositories
          (Pp.V.map_list
             (Pp.V.string -|
              Pp.of_module "repository" (module OpamRepositoryName)));
        "switch", Pp.ppacc
          with_switch switch
          (Pp.V.string -| Pp.of_module "switch" (module OpamSwitch));
        "jobs", Pp.ppacc
          with_jobs jobs
          Pp.V.pos_int;
        "download-command", Pp.ppacc_opt
          with_dl_tool dl_tool
          (Pp.V.map_list Pp.V.arg);
        "download-jobs", Pp.ppacc
          with_dl_jobs dl_jobs
          Pp.V.pos_int;
        "solver-criteria", Pp.ppacc_opt
          (with_criterion `Default) (criterion `Default)
          Pp.V.string;
        "solver-upgrade-criteria", Pp.ppacc_opt
          (with_criterion `Upgrade) (criterion `Upgrade)
          Pp.V.string;
        "solver-fixup-criteria", Pp.ppacc_opt
          (with_criterion `Fixup) (criterion `Fixup)
          Pp.V.string;
        "solver", Pp.ppacc_opt
          with_solver solver
          (Pp.V.map_list Pp.V.arg);

        (* deprecated fields *)
        "alias", Pp.ppacc_opt
          with_switch OpamStd.Option.none
          (Pp.V.string -| Pp.of_module "switch-name" (module OpamSwitch));
        "ocaml-version", Pp.ppacc_opt
          with_switch OpamStd.Option.none
          (Pp.V.string -| Pp.of_module "switch-name" (module OpamSwitch));
        "cores", Pp.ppacc_opt
          with_jobs OpamStd.Option.none
          Pp.V.pos_int;
        "system_ocaml-version", Pp.ppacc_ignore;
        "system-ocaml-version", Pp.ppacc_ignore;
      ]

    let pp =
      let name = internal in
      Pp.I.check_fields ~name fields -|
      Pp.I.fields ~name ~empty fields -|
      Pp.check ~name (fun t -> t.switch <> empty.switch)
        ~errmsg:"Missing switch"

    let of_syntax filename s =
      Pp.parse pp ~pos:(pos_file filename) s.file_contents

    let of_channel filename ic =
      of_syntax filename (Syntax.of_channel filename ic)

    let of_string filename str =
      of_syntax filename (Syntax.of_string filename str)

    let to_string filename t =
      let s = {
        file_format   = OpamVersion.current;
        file_name     = OpamFilename.to_string filename;
        file_contents = Pp.print pp t;
      } in
      Syntax.to_string s
  end

  (** Local repository config file (repo/<repo>/config) *)

  module Repo_config = struct

    let internal = "repo-config"

    type t = repository

    let empty = {
      repo_name     = OpamRepositoryName.of_string "<none>";
      repo_address  = ("<none>", None);
      repo_root     = OpamFilename.raw_dir "<none>";
      repo_kind     = `local;
      repo_priority = 0;
    }

    let fields = [
      "name", Pp.ppacc
        (fun r repo_name -> {r with repo_name})
        (fun r -> r.repo_name)
        (Pp.V.string -|
         Pp.of_module "repository-name" (module OpamRepositoryName));
      "address", Pp.ppacc
        (fun r (repo_address,_kind) -> {r with repo_address})
        (fun r -> r.repo_address, r.repo_kind)
        Pp.V.url;
      "kind", Pp.ppacc
        (fun r repo_kind -> {r with repo_kind})
        (fun r -> r.repo_kind)
        (Pp.V.string -|
         Pp.of_pair "repository-kind"
           (repository_kind_of_string, string_of_repository_kind));
      "priority", Pp.ppacc
        (fun r repo_priority -> {r with repo_priority})
        (fun r -> r.repo_priority)
        Pp.V.int;
      "root", Pp.ppacc
        (fun r repo_root -> {r with repo_root})
        (fun r -> r.repo_root)
        (Pp.V.string -|
         Pp.of_module "directory" (module OpamFilename.Dir));
    ]

    let pp =
      let name = internal in
      Pp.I.check_fields fields -|
      Pp.I.fields ~name:"repo-file" ~empty fields -|
      Pp.check ~name (fun r -> r.repo_root <> empty.repo_root)
        ~errmsg:"Missing 'root:'" -|
      Pp.check ~name (fun r -> r.repo_address <> empty.repo_address)
        ~errmsg:"Missing 'address:'" -|
      Pp.check ~name (fun r -> r.repo_name <> empty.repo_name)
        ~errmsg:"Missing 'name:'"

    let of_syntax filename s =
      Pp.parse pp ~pos:(pos_file filename) s.file_contents

    let of_channel filename ic =
      of_syntax filename (Syntax.of_channel filename ic)

    let of_string filename str =
      of_syntax filename (Syntax.of_string filename str)

    let to_string filename t =
      let s = {
        file_format   = OpamVersion.current;
        file_name     = OpamFilename.to_string filename;
        file_contents = Pp.print pp t;
      } in
      Syntax.to_string s

  end

  (** Global or package switch-local configuration variables.
      This file has free fields.
      (<switch>/config/global-config.config,
      <switch>/lib/<pkgname>/opam.config) *)

  module Dot_config = struct

    let internal = ".config"

    type t = (variable * variable_contents) list

    let create variables = variables

    let empty = []

    let pp =
      Pp.I.items -|
      Pp.map_list
        (Pp.map_pair
           (Pp.of_module "variable" (module OpamVariable))
           Pp.V.variable_contents)

    let of_syntax filename file =
      Pp.parse pp ~pos:(pos_file filename) file.file_contents

    let of_channel filename ic =
      of_syntax filename (Syntax.of_channel filename ic)

    let of_string filename str =
      of_syntax filename (Syntax.of_string filename str)

    let to_string filename t =
      Syntax.to_string {
        file_format   = OpamVersion.current;
        file_name     = OpamFilename.to_string filename;
        file_contents = Pp.print pp t;
      }

    let variables t = List.rev_map fst t

    let bindings t = t

    let variable t s =
      try Some (List.assoc s t)
      with Not_found -> None

    let set t k v =
      let t = List.remove_assoc k t in
      match v with
      | Some v -> (k,v) :: t
      | None -> t

  end


  (** (2) General, public repository format *)

  (** Public repository definition file (<repo>/repo) *)

  module Repo = struct

    let internal = "repo"

    type t = {
      opam_version : OpamVersion.t;
      browse       : string option;
      upstream     : string option;
      redirect     : (string * filter option) list;
    }

    let create
        ?browse ?upstream ?(opam_version=OpamVersion.current_nopatch)
        ?(redirect=[]) () =
      { opam_version; browse; upstream; redirect; }

    let empty = create ()

    let opam_version t = t.opam_version
    let browse t = t.browse
    let upstream t = t.upstream
    let redirect t = t.redirect

    let with_opam_version t opam_version = { t with opam_version }
    let with_browse t browse = { t with browse = Some browse }
    let with_upstream t upstream = { t with upstream = Some upstream }
    let with_redirect t redirect = { t with redirect }

    let fields = [
      "opam-version", Pp.ppacc
        with_opam_version opam_version
        (Pp.V.string -| Pp.of_module "opam-version" (module OpamVersion));
      "browse", Pp.ppacc_opt with_browse browse Pp.V.string;
      "upstream", Pp.ppacc_opt with_upstream upstream Pp.V.string;
      "redirect", Pp.ppacc
        with_redirect redirect
        (Pp.V.map_list (Pp.V.map_option Pp.V.string (Pp.opt Pp.V.filter)));
    ]

    let pp =
      let name = internal in
      Pp.I.check_fields ~name fields -|
      Pp.I.fields ~name ~empty fields

    let of_syntax filename s =
      Pp.parse pp ~pos:(pos_file filename) s.file_contents

    let of_channel filename ic =
      of_syntax filename (Syntax.of_channel filename ic)

    let of_string filename str =
      of_syntax filename (Syntax.of_string filename str)

    let to_string filename t =
      let s = {
        file_format   = OpamVersion.current;
        file_name     = OpamFilename.to_string filename;
        file_contents = Pp.print pp t;
      } in
      Syntax.to_string s

  end


  (** (3) Opam package format *)

  module OPAM = struct

    let internal = "opam"

    type t = {
      opam_version: opam_version;

      (* Package ident *)
      name       : OpamPackage.Name.t option;
      version    : OpamPackage.Version.t option;

      (* Relationships; solver and availability info *)
      depends    : ext_formula;
      depopts    : ext_formula;
      conflicts  : formula;
      available  : filter;
      ocaml_version: compiler_constraint option;
      os         : (bool * string) generic_formula;
      flags      : package_flag list;

      (* Build instructions *)
      build      : command list;
      build_test : command list;
      build_doc  : command list;
      install    : command list;
      remove     : command list;

      (* Auxiliary data affecting the build *)
      substs     : basename list;
      patches    : (basename * filter option) list;
      build_env  : (string * string * string) list;
      features   : (OpamVariable.t * string * filter) list;
      extra_sources: (address * string * basename option) list;

      (* User-facing data used by opam *)
      messages   : (string * filter option) list;
      post_messages: (string * filter option) list;
      depexts    : tags option;
      libraries  : (string * filter option) list;
      syntax     : (string * filter option) list;
      dev_repo   : pin_option option;

      (* Package database details *)
      maintainer : string list;
      author     : string list;
      license    : string list;
      tags       : string list;
      homepage   : string list;
      doc        : string list;
      bug_reports: string list;

      (* Extension fields (x-foo: "bar") *)
      extensions  : (pos * value) OpamStd.String.Map.t;
    }

    let empty = {
      opam_version = OpamVersion.current_nopatch;

      name       = None;
      version    = None;

      depends    = OpamFormula.Empty;
      depopts    = OpamFormula.Empty;
      conflicts  = OpamFormula.Empty;
      available  = FBool true;
      ocaml_version = None;
      os         = Empty;
      flags      = [];

      build      = [];
      build_test = [];
      build_doc  = [];
      install    = [];
      remove     = [];

      substs     = [];
      patches    = [];
      build_env  = [];
      features   = [];
      extra_sources = [];

      messages   = [];
      post_messages = [];
      depexts    = None;
      libraries  = [];
      syntax     = [];
      dev_repo   = None;

      maintainer = [];
      author     = [];
      license    = [];
      tags       = [];
      homepage   = [];
      doc        = [];
      bug_reports = [];

      extensions  = OpamStd.String.Map.empty;
    }

    let create nv =
      let name = Some (OpamPackage.name nv) in
      let version = Some (OpamPackage.version nv) in
      { empty with name; version }

    let check name = function
      | None    ->
        OpamFormat.bad_format "Invalid OPAM file (missing field %S)" name
      | Some n -> n

    let ext_field_prefix = "x-"
    let is_ext_field = OpamStd.String.starts_with ~prefix:ext_field_prefix

    (* Getters *)

    let opam_version t = t.opam_version
    let name t = check "name" t.name
    let name_opt t = t.name
    let version t = check "version" t.version
    let version_opt t = t.version
    let package t = OpamPackage.create (name t) (version t)

    let depends t = t.depends
    let depopts t = t.depopts
    let conflicts t = t.conflicts
    let available t = t.available
    let ocaml_version t = t.ocaml_version
    let os t = t.os
    let flags t = t.flags
    let has_flag f t = List.mem f t.flags

    let build t = t.build
    let build_test t = t.build_test
    let build_doc t = t.build_doc
    let install t = t.install
    let remove t = t.remove

    let substs t = t.substs
    let patches t = t.patches
    let build_env t = t.build_env
    let features t = t.features
    let extra_sources t = t.extra_sources

    let messages t = t.messages
    let post_messages t = t.post_messages
    let depexts t = t.depexts
    let libraries t = t.libraries
    let syntax t = t.syntax
    let dev_repo t = t.dev_repo

    let maintainer t = t.maintainer
    let author t = t.author
    let license t = t.license
    let tags t = t.tags
    let homepage t = t.homepage
    let doc t = t.doc
    let bug_reports t = t.bug_reports

    let extensions t = OpamStd.String.Map.map snd t.extensions
    let extended t fld parse =
      if not (is_ext_field fld) then invalid_arg "OpamFile.OPAM.extended";
      try
        let pos, s = OpamStd.String.Map.find fld t.extensions in
        (try Some (parse s) with
         | OpamFormat.Bad_format _ as e -> raise (OpamFormat.add_pos pos e))
      with Not_found -> None

    (* Setters *)

    let with_opam_version t opam_version = { t with opam_version }

    let with_name t name = { t with name = Some name }
    let with_name_opt t name = { t with name }
    let with_version t version = { t with version = Some version }
    let with_version_opt t version = { t with version }
    let with_nv t nv = { t with name = Some (OpamPackage.name nv);
                                version = Some (OpamPackage.version nv) }

    let with_depends t depends = { t with depends }
    let with_depopts t depopts = { t with depopts }
    let with_conflicts t conflicts = {t with conflicts }
    let with_available t available = { t with available }
    let with_ocaml_version t ocaml_version =
      { t with ocaml_version = Some ocaml_version }
    let with_ocaml_version_opt t ocaml_version = { t with ocaml_version }
    let with_os t os = { t with os }
    let with_flags t flags = { t with flags }
    let add_flags t flags =
      { t with flags = OpamStd.List.sort_nodup compare (flags @ t.flags) }

    let with_build t build = { t with build }
    let with_build_test t build_test = { t with build_test }
    let with_build_doc t build_doc = { t with build_doc }
    let with_install t install = { t with install }
    let with_remove t remove = { t with remove }

    let with_substs t substs = { t with substs }
    let with_patches t patches = { t with patches }
    let with_build_env t build_env = { t with build_env }
    let with_features t features = {t with features }
    let with_extra_sources t extra_sources = { t with extra_sources }

    let with_messages t messages = { t with messages }
    let with_post_messages t post_messages = { t with post_messages }
    let with_depexts t depexts = { t with depexts = Some depexts }
    let with_libraries t libraries = { t with libraries }
    let with_syntax t syntax = { t with syntax }
    let with_dev_repo t dev_repo = { t with dev_repo = Some dev_repo }

    let with_maintainer t maintainer = { t with maintainer }
    let with_author t author = { t with author }
    let with_license t license = { t with license }
    let with_tags t tags = { t with tags }
    let with_homepage t homepage = { t with homepage }
    let with_doc t doc = { t with doc }
    let with_bug_reports t bug_reports = { t with bug_reports }

    let with_extensions t extensions =
      if not (OpamStd.String.Map.for_all (fun k _ -> is_ext_field k) extensions)
      then invalid_arg "OpamFile.OPAM.with_extensions";
      {t with
       extensions = OpamStd.String.Map.map (fun s -> pos_null, s) extensions }
    let add_extension t fld syn =
      if not (is_ext_field fld) then invalid_arg "OpamFile.OPAM.add_extension";
      {t with
       extensions = OpamStd.String.Map.add fld (pos_null,syn) t.extensions }

    (* Post-processing functions used for some fields (optional, because we
       don't want them when validating) *)

    let cleanup_name _opam_version ~pos:(file,_,_ as pos) name =
      match OpamPackage.of_filename file with
      | Some nv when OpamPackage.name nv <> name ->
        Pp.warn ~pos "This file is for package '%s' but its 'name:' field \
                      advertises '%s'."
          (OpamPackage.name_to_string nv) (OpamPackage.Name.to_string name);
        OpamPackage.name nv
      | _ -> name

    let cleanup_version _opam_version ~pos:(file,_,_ as pos) version =
      match OpamPackage.of_filename file with
      | Some nv when OpamPackage.version nv <> version ->
        Pp.warn ~pos "This file is for version '%s' but its 'version:' field \
                      advertises '%s'."
          (OpamPackage.version_to_string nv) (OpamPackage.Version.to_string version);
        OpamPackage.version nv
      | _ -> version

    let cleanup_depflags _opam_version ~pos ext_formula =
      (* remove unknown dependency flags *)
      OpamFormula.map (fun (name, (flags, cstr)) ->
          let unknown_flags =
            OpamStd.List.filter_map (function
                | Depflag_Unknown n -> Some n
                | _ -> None)
              flags in
          if unknown_flags <> [] then
            Pp.warn ~pos "Unknown flags %s ignored for dependency %s"
              (OpamStd.Format.pretty_list unknown_flags)
              (OpamPackage.Name.to_string name);
          let known_flags = List.filter (function
              | Depflag_Unknown _ -> false
              | _ -> true)
              flags in
          Atom (name, (known_flags, cstr)))
        ext_formula

    let cleanup_depopts opam_version ~pos depopts =
      let depopts = cleanup_depflags opam_version ~pos depopts in
      if OpamFormatConfig.(!r.skip_version_checks) ||
         OpamVersion.compare opam_version (OpamVersion.of_string "1.2") < 0
      then depopts
      else
      (* Make sure depopts are a pure disjunction, without constraints *)
      let rec aux acc disjunction =
        List.fold_left (fun acc -> function
            | OpamFormula.Atom (_, (_,Empty)) as atom -> atom :: acc
            | OpamFormula.Atom (name, (flags, cstr)) ->
              Pp.warn ~pos
                "Version constraint (%s) no longer allowed in optional \
                 dependency (ignored).\n\
                 Use the 'conflicts' field instead."
                (OpamFormula.string_of_formula (fun (r,v) ->
                     OpamFormula.string_of_relop r ^" "^
                     OpamPackage.Version.to_string v)
                    cstr);
              OpamFormula.Atom (name, (flags, Empty)) :: acc
            | f ->
              Pp.warn "Optional dependencies must be a disjunction. \
                       Treated as such.";
              aux acc
                (OpamFormula.fold_left (fun acc a -> OpamFormula.Atom a::acc)
                   [] f)
          )
          acc disjunction
      in
      OpamFormula.ors_to_list depopts
      |> aux []
      |> List.rev
      |> OpamFormula.ors

    let cleanup_conflicts opam_version ~pos conflicts =
      (* Conflicts were encoded as a conjunction before 1.3, which didn't match
         the semantics. The rewrite is done for all versions, but on 1.3+ it
         should be an error. *)
      let is_disjunction f =
        List.for_all (function Atom _ -> true | _ -> false)
          OpamFormula.(ors_to_list (to_atom_formula f))
      in
      if is_disjunction conflicts then conflicts else
      let force_disjunction f =
        OpamFormula.map_formula (function
            | And (a, b) -> Or (a, b)
            | f -> f)
          f
      in
      if OpamVersion.(compare opam_version (of_string "1.3") >= 0) then
        Pp.warn ~pos "Conflicts must be a disjunction, '&' is not \
                      supported (treated as '|').";
      OpamFormula.map (fun (n,cs) -> Atom (n, force_disjunction cs)) conflicts
      |> force_disjunction

    let cleanup_flags _opam_version ~pos flags =
      let known_flags =
        List.filter (function Pkgflag_Unknown _ -> false | _ -> true)
          flags in
      if known_flags <> flags then
        Pp.warn ~pos
          "Unknown package flags %s ignored"
          (OpamStd.Format.pretty_list (OpamStd.List.filter_map (function
               | Pkgflag_Unknown s -> Some s
               | _ -> None)
               flags));
      known_flags

    let pp_basename =
      Pp.V.string -|
      Pp.of_module "file" (module OpamFilename.Base)

    (* Field parser-printers *)

    (* [field name, (pure pp, pp including cleanup/check function)] *)
    let fields_gen =
      let no_cleanup (ppacc: ?cleanup:(pos:_ -> _) -> _) set get pp =
        let p = ppacc set get pp in p, p
      in
      let with_cleanup cleanup (ppacc: ?cleanup:(pos:_ -> _) -> _) set get pp =
        let cleanup ~pos acc x = cleanup acc.opam_version ~pos x in
        ppacc set get pp,
        ppacc set get ~cleanup pp
      in
      [
        "opam-version", no_cleanup Pp.ppacc with_opam_version opam_version
          (Pp.V.string -| Pp.of_module "opam-version" (module OpamVersion));
        "name", with_cleanup cleanup_name Pp.ppacc_opt with_name name_opt
          (Pp.V.string -| Pp.of_module "name" (module OpamPackage.Name));
        "version", with_cleanup cleanup_version
          Pp.ppacc_opt with_version version_opt
          (Pp.V.string -| Pp.of_module "version" (module OpamPackage.Version));

        "depends", with_cleanup cleanup_depflags Pp.ppacc with_depends depends
          (Pp.V.package_formula `Conj Pp.V.ext_constraints);
        "depopts", with_cleanup cleanup_depopts Pp.ppacc with_depopts depopts
          (Pp.V.package_formula `Disj Pp.V.ext_constraints);
        "conflicts", with_cleanup cleanup_conflicts
          Pp.ppacc with_conflicts conflicts
          (Pp.V.package_formula `Disj Pp.V.constraints);
        "available", no_cleanup Pp.ppacc with_available available
          (Pp.V.list -| Pp.V.filter);
        "ocaml-version", no_cleanup
          Pp.ppacc_opt with_ocaml_version ocaml_version
          (Pp.V.list -| Pp.V.constraints Pp.V.compiler_version);
        "os", no_cleanup Pp.ppacc with_os os
          Pp.V.os_constraint;
        "flags", with_cleanup cleanup_flags Pp.ppacc add_flags flags
          (Pp.V.map_list @@
           Pp.V.ident -|
           Pp.of_pair "package-flag" (pkg_flag_of_string, string_of_pkg_flag));

        "build", no_cleanup Pp.ppacc with_build build
          (Pp.V.map_list Pp.V.command);
        "build-test", no_cleanup Pp.ppacc with_build_test build_test
          (Pp.V.map_list Pp.V.command);
        "build-doc", no_cleanup Pp.ppacc with_build_doc build_doc
          (Pp.V.map_list Pp.V.command);
        "install", no_cleanup Pp.ppacc with_install install
          (Pp.V.map_list Pp.V.command);
        "remove", no_cleanup Pp.ppacc with_remove remove
          (Pp.V.map_list Pp.V.command);

        "substs", no_cleanup Pp.ppacc with_substs substs
          (Pp.V.map_list pp_basename);
        "patches", no_cleanup Pp.ppacc with_patches patches
          (Pp.V.map_list @@ Pp.V.map_option pp_basename (Pp.opt Pp.V.filter));
        "build-env", no_cleanup Pp.ppacc with_build_env build_env
          (Pp.V.map_list Pp.V.env_binding);
        "features", no_cleanup Pp.ppacc with_features features
          Pp.V.features;
        "extra-sources", no_cleanup Pp.ppacc with_extra_sources extra_sources
          (Pp.V.map_list @@
           Pp.V.map_pair
             (Pp.V.map_option
                Pp.V.address
                (Pp.opt @@ Pp.singleton -| pp_basename))
             (Pp.V.string -| Pp.check ~name:"md5" OpamFilename.valid_digest)
           -| Pp.pp
             (fun ~pos:_ ((u,md5),f) -> u,f,md5)
             (fun (u,f,md5) -> (u,md5),f));

        "messages", no_cleanup Pp.ppacc with_messages messages
          (Pp.V.map_list (Pp.V.map_option Pp.V.string_tr (Pp.opt Pp.V.filter)));
        "post-messages", no_cleanup Pp.ppacc with_post_messages post_messages
          (Pp.V.map_list (Pp.V.map_option Pp.V.string_tr (Pp.opt Pp.V.filter)));
        "depexts", no_cleanup Pp.ppacc_opt with_depexts depexts
          (let string_set name =
             Pp.V.map_list Pp.V.string -|
             Pp.of_pair name OpamStd.String.Set.(of_list, elements)
           in
           Pp.V.map_list
             (Pp.V.map_pair
                (string_set "system-id") (string_set "system-package")) -|
           Pp.of_pair "depext-bindings"
             OpamStd.String.SetMap.(of_list, bindings));
        "libraries", no_cleanup Pp.ppacc with_libraries libraries
          (Pp.V.map_list (Pp.V.map_option Pp.V.string (Pp.opt Pp.V.filter)));
        "syntax", no_cleanup Pp.ppacc with_syntax syntax
          (Pp.V.map_list (Pp.V.map_option Pp.V.string (Pp.opt Pp.V.filter)));
        "dev-repo", no_cleanup Pp.ppacc_opt with_dev_repo dev_repo
          (Pp.V.url -|
           Pp.check ~errmsg:"Not a version-control or http url"
             (function _, (`http | #version_control) -> true | _ -> false) -|
           Pp.of_pair "pin-address" (pin_of_url, url_of_pin));

        "maintainer", no_cleanup Pp.ppacc with_maintainer maintainer
          (Pp.V.map_list Pp.V.string);
        "author", no_cleanup Pp.ppacc
          with_author author
          (Pp.V.map_list Pp.V.string);
        "authors", no_cleanup Pp.ppacc
          (fun t a -> if t.author = [] then with_author t a else
              OpamFormat.bad_format "multiple \"author:\" fields" author)
          (fun _ -> [])
          (Pp.V.map_list Pp.V.string);
        "license", no_cleanup Pp.ppacc with_license license
          (Pp.V.map_list Pp.V.string);
        "tags", no_cleanup Pp.ppacc with_tags tags
          (Pp.V.map_list Pp.V.string);
        "homepage", no_cleanup Pp.ppacc with_homepage homepage
          (Pp.V.map_list Pp.V.string);
        "doc", no_cleanup Pp.ppacc with_doc doc
          (Pp.V.map_list Pp.V.string);
        "bug-reports", no_cleanup Pp.ppacc with_bug_reports bug_reports
          (Pp.V.map_list Pp.V.string);

        "configure-style", (Pp.ppacc_ignore, Pp.ppacc_ignore); (* deprecated *)
      ]

    let fields =
      List.map (fun (name, (_, cleaned_up_pp)) -> name, cleaned_up_pp)
        fields_gen

    let raw_fields =
      List.map (fun (name, (raw_pp, _)) -> name, raw_pp)
        fields_gen

    let handle_flags_in_tags =
      (* Allow 'flag:xxx' tags as flags, for compat *)
      let prefix = "flags:" in
      let flag_of_tag tag =
         if OpamStd.String.starts_with ~prefix tag then
           Some (pkg_flag_of_string (OpamStd.String.remove_prefix ~prefix tag))
         else None
      in
      let parse ~pos t =
        let flags =
          List.fold_left (fun flags tag ->
              match flag_of_tag tag with
              | Some flag -> flag :: flags
              | None -> flags)
            t.flags t.tags
        in
        {t with flags}
      in
      let print t =
        let flags, tags =
          List.fold_left (fun (flags, tags) tag ->
              match flag_of_tag tag with
              | Some flag ->
                if List.mem flag flags then
                  List.filter ((<>) flag) flags, tag::tags
                else flags, tags
              | None -> flags, tag::tags)
            (t.flags,[]) (List.rev t.tags)
        in
        {t with flags; tags}
      in
      Pp.pp parse print

    let pp =
      Pp.I.check_opam_version () -|
      Pp.I.check_fields ~name:"opam-file" ~allow_extensions:true fields -|
      Pp.I.partition_fields is_ext_field -| Pp.map_pair
        (Pp.I.items -|
         OpamStd.String.Map.(Pp.pp (fun ~pos:_ -> of_list) bindings))
        (Pp.I.fields ~name:"opam-file" ~empty fields -|
         handle_flags_in_tags) -|
      Pp.pp
        (fun ~pos (extensions, t) -> with_extensions t extensions)
        (fun t -> extensions t, t)

(*
    let opam_1_0_fields = [
      s_opam_version;
      s_maintainer;
      s_substs;
      s_build;
      s_remove;
      s_depends;
      s_depopts;
      s_conflicts;
      s_libraries;
      s_syntax;
      s_ocaml_version;
      s_build_env;
      s_patches;
      s_os;
      s_license;
      s_authors;
      s_homepage;
      s_doc;
      s_build_test;
      s_build_doc;
      s_depexts;
      s_tags;
      s_version;
      s_name;
      s_configure_style;
    ]

    let opam_1_1_fields = [
      s_author;
      s_available;
      s_messages;
      s_post_messages;
      s_bug_reports;
    ]

    let opam_1_2_fields = [
      s_flags;
      s_dev_repo;
      s_install;
      s_features;
    ]

    let opam_1_3_fields = [
      s_extra_sources;
    ]

    let to_1_0_fields k v =
      if List.mem k opam_1_1_fields || List.mem k opam_1_2_fields
         || List.mem k opam_1_3_fields then
        if k = s_author then Some (s_authors, v)
        else None
      else if k = s_maintainer || k = s_homepage || k = s_license then
        match v with
        | List (_,v::_) -> Some (k, v)
        | v           -> Some (k, v)
      else Some (k, v)

    let to_1_0 file =
      let file = OpamFormat.map to_1_0_fields file in
      Syntax.to_1_0 file

    let valid_fields =
      opam_1_0_fields @ opam_1_1_fields @ opam_1_2_fields @ opam_1_3_fields

    let is_explicit filename =
      try
        let ic = OpamFilename.open_in filename in
        try
          let file = Syntax.of_channel filename ic in
          let fields = OpamFormat.variables file.file_contents in
          List.exists (fun (f,_) -> f = s_name || f = s_version) fields
        with e ->
          close_in ic;
          raise e
      with e -> OpamStd.Exn.fatal e; false

    let to_string filename t =
      let make_file =
        OpamFormat.make_option (OpamFilename.Base.to_string @> OpamFormat.make_string)
          OpamFormat.make_filter in
      let option c s f = match c with
        | None   -> []
        | Some v -> [ OpamFormat.make_variable (s, f v) ] in
      let list c s f = match c with
        | [] -> []
        | l  -> [ OpamFormat.make_variable (s, f l) ] in
      let listm c s f = match c with
        | [] -> []
        | l  -> [ OpamFormat.make_variable (s, OpamFormat.make_list f l) ] in
      let formula c s f = match c with
        | Empty -> []
        | x     -> [ OpamFormat.make_variable (s, f x) ] in
      let filter c s f = match c with
        | FBool true -> []
        | x     -> [ OpamFormat.make_variable (s, f x) ] in
      let name_and_version = match OpamPackage.of_filename filename with
        | Some _ -> []
        | None ->
          option t.name s_name
            (OpamPackage.Name.to_string @> OpamFormat.make_string) @
          option t.version s_version
            (OpamPackage.Version.to_string @> OpamFormat.make_string)
      in
      let extra_source (url,hash,fname) = OpamFormat.(
          make_pair
            (make_option (string_of_address @> make_string)
               (OpamFilename.Base.to_string @> (fun x -> [make_string x])))
            make_string
            ((url,fname),hash)
        ) in
      let s = {
        file_format   = t.opam_version;
        file_name     = OpamFilename.to_string filename;
        file_contents = [
          OpamFormat.make_variable (s_opam_version,
                    (OpamVersion.to_string @> OpamFormat.make_string) t.opam_version);
        ] @ name_and_version
          @ list    t.maintainer    s_maintainer    OpamFormat.make_string_list
          @ list    t.author        s_authors       OpamFormat.make_string_list
          @ list    t.homepage      s_homepage      OpamFormat.make_string_list
          @ list    t.bug_reports   s_bug_reports   OpamFormat.make_string_list
          @ list    t.license       s_license       OpamFormat.make_string_list
          @ list    t.doc           s_doc           OpamFormat.make_string_list
          @ list    t.tags          s_tags          OpamFormat.make_string_list
          @ option  t.dev_repo      s_dev_repo
            (string_of_pin_option @> OpamFormat.make_string)
          @ listm   t.substs s_substs
              (OpamFilename.Base.to_string @> OpamFormat.make_string)
          @ listm   t.build_env     s_build_env     OpamFormat.make_env_variable
          @ listm   t.build         s_build         OpamFormat.make_command
          @ listm   t.install       s_install       OpamFormat.make_command
          @ listm   t.build_test    s_build_test    OpamFormat.make_command
          @ listm   t.build_doc     s_build_doc     OpamFormat.make_command
          @ listm   t.remove        s_remove        OpamFormat.make_command
          @ formula t.depends       s_depends       OpamFormat.(make_formula `Conj make_ext_constraints)
          @ formula t.depopts       s_depopts       OpamFormat.(make_formula `Disj make_ext_constraints)
          @ option  t.depexts       s_depexts       OpamFormat.make_tags
          @ formula t.conflicts     s_conflicts     OpamFormat.(make_formula `Disj make_constraints)
          @ list    t.features      s_features      OpamFormat.make_features
          @ list    t.libraries     s_libraries     OpamFormat.make_libraries
          @ list    t.syntax        s_syntax        OpamFormat.make_libraries
          @ list    t.patches       s_patches       (OpamFormat.make_list make_file)
          @ option  t.ocaml_version s_ocaml_version OpamFormat.make_compiler_constraint
          @ formula t.os            s_os            OpamFormat.make_os_constraint
          @ filter  t.available     s_available
              (OpamFormat.make_filter @> OpamFormat.make_list (fun x -> x))
          @ list    t.messages      s_messages
              OpamFormat.(make_list (make_option make_string make_filter))
          @ list    t.post_messages s_post_messages
              OpamFormat.(make_list (make_option make_string make_filter))
          @ list    t.flags         s_flags
              OpamFormat.(make_list make_flag)
          @ listm   t.extra_sources s_extra_sources extra_source
          @ OpamStd.String.Map.fold (fun fld (_,syn) acc ->
              OpamFormat.make_variable (fld, syn) :: acc) t.extensions []
      } in
      Syntax.to_string s

    let check_name ?(permissive=false) n s =
      let name_f =
        try OpamFormat.assoc_option s s_name
              (OpamFormat.parse_package_name ?expected:n)
        with OpamFormat.Bad_format _ when permissive -> None
      in
      OpamStd.Option.Op.(name_f ++ n)

    let check_version ?(permissive=false) v s =
      let version_f =
        try OpamFormat.assoc_option s s_version
              (OpamFormat.parse_package_version ?expected:v)
        with OpamFormat.Bad_format _ when permissive -> None
      in
      OpamStd.Option.Op.(version_f ++ v)

    let of_syntax ?(permissive=false) ?(conservative=false) f nv =
      let safe default f x y z =
        try f x y z with
        | OpamFormat.Bad_format _ as bf
          when not conservative && not OpamFormatConfig.(!r.strict) ->
          if not permissive then
            OpamConsole.warning "%s" (OpamFormat.string_of_bad_format bf);
          default
      in
      let assoc_option x y z = safe None OpamFormat.assoc_option x y z in
      let assoc_list x y z = safe [] OpamFormat.assoc_list x y z in
      let assoc_default dft = safe dft (OpamFormat.assoc_default dft) in
      let s = f.file_contents in
      let name =
        check_name ~permissive (OpamStd.Option.map OpamPackage.name nv) s
      in
      let opam_version = OpamFormat.assoc s s_opam_version
          (OpamFormat.parse_string @> OpamVersion.of_string) in
      let version =
        check_version ~permissive (OpamStd.Option.map OpamPackage.version nv) s
      in
      let maintainer =
        assoc_list s s_maintainer OpamFormat.parse_string_list in
      let substs =
        assoc_list s s_substs
          (OpamFormat.parse_list (OpamFormat.parse_string @>
                                  OpamFilename.Base.of_string)) in
      let build_env =
        assoc_list s s_build_env
          (OpamFormat.parse_list OpamFormat.parse_env_variable) in
      let build = assoc_list s s_build OpamFormat.parse_commands in
      let install = assoc_list s s_install OpamFormat.parse_commands in
      let remove = assoc_list s s_remove OpamFormat.parse_commands in
      let check_depflags ~pos ext_formula =
        if conservative then ext_formula else
        OpamFormula.map (fun (name, (flags, cstr)) ->
              let known_flags =
                List.filter
                  (function Depflag_Unknown _ -> false | _ -> true) flags in
              if not permissive && known_flags <> flags then
                OpamConsole.warning
                  "At %s:\n  Unknown flags %s ignored for dependency %s"
                  (string_of_pos pos)
                  (OpamStd.Format.pretty_list (OpamStd.List.filter_map (function
                       | Depflag_Unknown s -> Some s
                       | _ -> None)
                       flags))
                  (OpamPackage.Name.to_string name);
              Atom (name, (known_flags, cstr)))
          ext_formula
      in
      let depends =
        assoc_default OpamFormula.Empty s s_depends
          (fun v ->
             OpamFormat.(parse_formula `Conj parse_ext_constraints v) |>
             check_depflags ~pos:(OpamFormat.value_pos v)) in
      let depopts =
        let rec cleanup ~pos acc disjunction =
          List.fold_left (fun acc -> function
              | OpamFormula.Atom (_, (_,Empty)) as atom -> atom :: acc
              | OpamFormula.Atom (name, (flags, cstr)) ->
                OpamConsole.warning
                  "At %s:\n\
                   Version constraint (%s) no longer allowed in optional \
                   dependency (ignored).\n\
                   Use the 'conflicts' field instead."
                  (string_of_pos pos)
                  (OpamFormula.string_of_formula (fun (r,v) ->
                       OpamFormula.string_of_relop r ^" "^
                       OpamPackage.Version.to_string v)
                      cstr);
                OpamFormula.Atom (name, (flags, Empty)) :: acc
              | f ->
                OpamConsole.warning
                  "At %s:\n\
                   Optional dependencies must be a disjunction. Treated as such."
                  (string_of_pos pos);
                cleanup ~pos acc
                  (OpamFormula.fold_left (fun acc a -> OpamFormula.Atom a::acc) [] f)
            )
            acc disjunction
        in
        assoc_default OpamFormula.Empty s s_depopts @@ fun value ->
        let f =
          OpamFormat.(parse_formula `Disj parse_ext_constraints) value |>
          check_depflags ~pos:(OpamFormat.value_pos value) in
        if not conservative &&
           not OpamFormatConfig.(!r.skip_version_checks) &&
           OpamVersion.compare opam_version (OpamVersion.of_string "1.2") >= 0
        then
            OpamFormula.ors_to_list f
            |> cleanup ~pos:(OpamFormat.value_pos value) []
            |> List.rev
            |> OpamFormula.ors
        else f
      in
      let conflicts =
        let is_disjunction f =
          List.for_all (function Atom _ -> true | _ -> false)
            OpamFormula.(ors_to_list (to_atom_formula f))
        in
        let force_disjunction f =
          OpamFormula.map_formula (function
              | And (a, b) -> Or (a, b)
              | f -> f)
            f
        in
        let cleanup ~pos f =
          if is_disjunction f then f else
          (if OpamVersion.compare opam_version (OpamVersion.of_string "1.3") >= 0
           then
             OpamConsole.warning
               "At %s:\n\
                Conflicts must be a disjunction, '&' is not supported \
                (treated as '|')."
               (string_of_pos pos);
           f
           |> OpamFormula.map (fun (n,cs) -> Atom (n, force_disjunction cs))
           |> force_disjunction)
        in
        assoc_default OpamFormula.Empty s s_conflicts @@ fun value ->
        OpamFormat.(parse_formula `Disj parse_constraints value) |> fun f ->
        if conservative then f
        else cleanup ~pos:(OpamFormat.value_pos value) f
      in
      let features = OpamFormat.assoc_default [] s s_features
          OpamFormat.parse_features in
      let libraries = assoc_list s s_libraries OpamFormat.parse_libraries in
      let syntax = assoc_list s s_syntax OpamFormat.parse_libraries in
      let ocaml_version = assoc_option s s_ocaml_version
          OpamFormat.parse_compiler_constraint in
      let os = assoc_default OpamFormula.Empty s s_os
          OpamFormat.parse_os_constraint in
      let available = assoc_default (FBool true) s s_available
          (OpamFormat.parse_list (fun x -> x) @> OpamFormat.parse_filter) in
      let parse_file =
        OpamFormat.parse_option
          (OpamFormat.parse_string @> OpamFilename.Base.of_string)
          OpamFormat.parse_filter in
      let patches = assoc_list s s_patches
          (OpamFormat.parse_list parse_file) in
      let homepage = assoc_list s s_homepage OpamFormat.parse_string_list in
      let author =
        let x = assoc_list s s_authors OpamFormat.parse_string_list in
        let y = assoc_list s s_author  OpamFormat.parse_string_list in
        x @ y in
      let license = assoc_list s s_license OpamFormat.parse_string_list in
      let doc = assoc_list s s_doc OpamFormat.parse_string_list in
      let tags = assoc_list s s_tags OpamFormat.parse_string_list in
      let build_test = assoc_list s s_build_test OpamFormat.parse_commands in
      let build_doc = assoc_list s s_build_doc OpamFormat.parse_commands in
      let depexts = assoc_option s s_depexts OpamFormat.parse_tags in
      let messages = assoc_list s s_messages OpamFormat.parse_messages in
      let bug_reports =
        assoc_list s s_bug_reports OpamFormat.parse_string_list in
      let post_messages =
        assoc_list s s_post_messages OpamFormat.parse_messages in
      let flags =
        let parse v =
          let allflags = OpamFormat.(parse_list parse_flag v) in
          if conservative then allflags else
          let known_flags =
            List.filter (function Pkgflag_Unknown _ -> false | _ -> true)
              allflags in
          if not permissive && known_flags <> allflags then
            OpamConsole.warning
              "At %s:\n  Unknown package flags %s ignored"
              (string_of_pos (OpamFormat.value_pos v))
              (OpamStd.Format.pretty_list (OpamStd.List.filter_map (function
                   | Pkgflag_Unknown s -> Some s
                   | _ -> None)
                   allflags));
          known_flags in
        assoc_list s s_flags parse in
      let dev_repo =
        assoc_option s s_dev_repo @@ fun v ->
        OpamFormat.parse_string v |> address_of_string |>
        (fun addr ->
           try parse_url addr with
           | Invalid_argument msg ->
             OpamFormat.bad_format ~pos:(OpamFormat.value_pos v) "%s" msg) |>
        (fun url ->
           try pin_of_url url with
           | Failure msg ->
             OpamFormat.bad_format ~pos:(OpamFormat.value_pos v) "%s" msg) |>
        (function
          | Git _ | Darcs _ | Hg _ as pin -> pin
          | Http u -> if conservative then Http u else Git u
          | _ ->
            OpamFormat.bad_format ~pos:(OpamFormat.value_pos v)
              "Unrecognised version-control address")
      in
      let extra_sources =
        assoc_list s s_extra_sources OpamFormat.(
            parse_list
              (parse_pair
                 (parse_single_option
                    (parse_string @> address_of_string)
                    (parse_string @> OpamFilename.Base.of_string))
                 parse_string
               @> fun ((url,fname),hash) -> url, hash, fname)
          )
      in
      let extensions =
        List.fold_left (fun acc -> function
            | Variable (pos,fld,v)
              when OpamStd.String.starts_with ~prefix:"x-" fld ->
              OpamStd.String.Map.add fld (pos,v) acc
            | _ -> acc)
          OpamStd.String.Map.empty s
      in
      { opam_version; name; version; maintainer; substs; build; install; remove;
        depends; depopts; conflicts; features; libraries; syntax;
        patches; ocaml_version; os; available; build_env;
        homepage; author; license; doc; tags;
        build_test; build_doc; depexts; messages; post_messages;
        bug_reports; flags; dev_repo; extra_sources; extensions;
      }
*)

    let of_syntax filename s =
      let pos = pos_file filename in
      let t = Pp.parse pp ~pos s.file_contents in
      match OpamPackage.of_filename filename, t.name, t.version with
      | Some nv, Some tname, _ when OpamPackage.name nv <> tname ->
        Pp.warn ~pos
          "Field 'name: %S' doesn't match the name %S implied by the \
           file name"
          (OpamPackage.Name.to_string tname)
          (OpamPackage.name_to_string nv);
        with_nv t nv
      | Some nv, _, Some tversion when OpamPackage.version nv <> tversion ->
        Pp.warn ~pos
          "Field 'version: %S' doesn't match the version %S implied by the \
           file name"
          (OpamPackage.Version.to_string tversion)
          (OpamPackage.version_to_string nv);
        with_nv t nv
      | Some nv, _, _ -> with_nv t nv
      | None, _, _ -> t

    let of_channel filename ic =
      of_syntax filename (Syntax.of_channel filename ic)

    let of_string filename str =
      of_syntax filename (Syntax.of_string filename str)

    let to_string filename t =
      let s = {
        file_format   = OpamVersion.current;
        file_name     = OpamFilename.to_string filename;
        file_contents = Pp.print pp t;
      } in
      Syntax.to_string s

    let template nv =
      let t = create nv in
      let maintainer =
        let from_git = try
            match
              OpamSystem.read_command_output
                ["git"; "config"; "--get"; "user.name"],
              OpamSystem.read_command_output
                ["git"; "config"; "--get"; "user.email"]
            with
            | [name], [email] ->
              Some [Printf.sprintf "%s <%s>" name email]
            | _ -> raise Not_found
          with e -> OpamStd.Exn.fatal e; None
        in
        match from_git with
        | Some u -> u
        | None ->
          let email =
            try Some (Sys.getenv "EMAIL") with Not_found -> None in
          try
            let open Unix in
            let pw = getpwuid (getuid ()) in
            let email = match email with
              | Some e -> e
              | None -> pw.pw_name^"@"^gethostname () in
            match OpamStd.String.split pw.pw_gecos ',' with
            | name::_ -> [Printf.sprintf "%s <%s>" name email]
            | _ -> [email]
          with Not_found -> match email with
            | Some e -> [e]
            | None -> []
      in
      { t with
        maintainer;
        build      = [[CString "./configure", None;
                       CString "--prefix=%{prefix}%", None], None;
                      [CIdent "make", None], None];
        install    = [[CIdent "make", None; CString "install", None], None];
        remove     = [[CString "ocamlfind", None; CString "remove", None;
                       CString
                         (OpamPackage.Name.to_string (OpamPackage.name nv)),
                       None],
                      None];
        depends    = Atom (OpamPackage.Name.of_string "ocamlfind",
                           ([Depflag_Build], Empty));
        author     = maintainer;
        homepage   = [""];
        license    = [""];
        dev_repo   = Some (Local (OpamFilename.Dir.of_string ""));
        bug_reports= [""];
      }

    let validate t =
      let cond num level msg ?detail cd =
        if cd then
          let msg = match detail with
            | Some d ->
              Printf.sprintf "%s: \"%s\"" msg (String.concat "\", \"" d)
            | None -> msg
          in
          Some (num, level, msg)
        else None
      in
      let names_of_formula flag f =
        OpamPackage.Name.Set.of_list @@
        List.map fst OpamFormula.(
            atoms @@ filter_deps ~dev:true ~build:true ~test:flag ~doc:flag f
          )
      in
      let all_commands =
        t.build @ t.install @ t.remove @ t.build_test @ t.build_doc
      in
      let all_filters =
        OpamStd.List.filter_map snd t.patches @
        OpamStd.List.filter_map snd t.messages @
        OpamStd.List.filter_map snd t.post_messages @
        [t.available] @
        List.map (fun (_,_,f) -> f) t.features
      in
      let all_variables =
        OpamFilter.commands_variables all_commands @
        List.fold_left (fun acc f -> OpamFilter.variables f @ acc)
          [] all_filters
      in
      let all_depends =
        OpamPackage.Name.Set.union
          (names_of_formula true t.depends)
          (names_of_formula true t.depopts)
      in
      let warnings = [
        cond 20 `Warning
          "Field 'opam-version' refers to the patch version of opam, it \
           should be of the form MAJOR.MINOR"
          ~detail:[OpamVersion.to_string t.opam_version]
          (OpamVersion.nopatch t.opam_version <> t.opam_version);
        cond 21 `Error
          "Field 'opam-version' doesn't match the current version, \
           validation may not be accurate"
          ~detail:[OpamVersion.to_string t.opam_version]
          (OpamVersion.compare t.opam_version OpamVersion.current_nopatch <> 0
           && OpamVersion.compare t.opam_version (OpamVersion.of_string "1.2")
              <> 0);
(*
          cond (t.name = None)
            "Missing field 'name' or directory in the form 'name.version'";
          cond (t.version = None)
            "Missing field 'version' or directory in the form 'name.version'";
*)
        (let empty_fields =
           OpamStd.List.filter_map (function n,[""] -> Some n | _ -> None)
             ["maintainer", t.maintainer; "homepage", t.homepage;
              "author", t.author; "license", t.license; "doc", t.doc;
              "tags", t.tags; "bug_reports", t.bug_reports]
         in
         cond 22 `Error
          "Some fields are present but empty; remove or fill them"
          ~detail:empty_fields
          (empty_fields <> []));
        cond 23 `Error
          "Missing field 'maintainer'"
          (t.maintainer = []);
        cond 24 `Error
          "Field 'maintainer' has the old default value"
          (List.mem "contact@ocamlpro.com" t.maintainer &&
           not (List.mem "org:ocamlpro" t.tags));
        cond 25 `Error
          "Missing field 'authors'"
          (t.author = []);
        cond 26 `Warning
          "No field 'install', but a field 'remove': install instructions \
           probably part of 'build'. Use the 'install' field or a .install \
           file"
          (t.install = [] && t.build <> [] && t.remove <> []);
        cond 27 `Warning
          "No field 'remove' while a field 'install' is present, uncomplete \
           uninstallation suspected"
          (t.install <> [] && t.remove = []);
        (let unk_flags =
           OpamStd.List.filter_map (function
               | Pkgflag_Unknown s -> Some s
               | _ -> None)
             t.flags
         in
         cond 28 `Error
           "Unknown package flags found"
           ~detail:unk_flags
           (unk_flags <> []));
        (let unk_depflags =
           OpamFormula.fold_left (fun acc (_, (flags, _)) ->
               OpamStd.List.filter_map
                 (function Depflag_Unknown s -> Some s | _ -> None)
                 flags
               @ acc)
             [] (OpamFormula.ands [t.depends;t.depopts])
         in
         cond 29 `Error
           "Unknown dependency flags in depends or depopts"
           ~detail:unk_depflags
           (unk_depflags <> []));
        cond 30 `Error
          "Field 'depopts' contains formulas or version constraints"
          (List.exists (function
               | OpamFormula.Atom (_, (_,Empty)) -> false
               | _ -> true)
              (OpamFormula.ors_to_list t.depopts));
        (let dup_depends =
           OpamPackage.Name.Set.inter
             (names_of_formula false t.depends)
             (names_of_formula true t.depopts)
         in
         cond 31 `Error
           "Fields 'depends' and 'depopts' refer to the same package names"
           ~detail:OpamPackage.Name.
                     (List.map to_string (Set.elements dup_depends))
           (not (OpamPackage.Name.Set.is_empty dup_depends)));
        cond 32 `Error
          "Field 'ocaml-version' is deprecated, use 'available' and the \
           'ocaml-version' variable instead"
          (t.ocaml_version <> None);
        cond 33 `Error
          "Field 'os' is deprecated, use 'available' and the 'os' variable \
           instead"
          (t.os <> Empty);
        (let pkg_vars =
           List.filter (fun v -> not (OpamVariable.Full.is_global v))
             (OpamFilter.variables t.available)
         in
         cond 34 `Error
           "Field 'available' contains references to package-local variables. \
            It should only be determined from global configuration variables"
           ~detail:(List.map OpamVariable.Full.to_string pkg_vars)
           (pkg_vars <> []));
        cond 35 `Error
          "Missing field 'homepage'"
          (t.homepage = []);
        (* cond (t.doc = []) *)
        (*   "Missing field 'doc'"; *)
        cond 36 `Warning
          "Missing field 'bug-reports'"
          (t.bug_reports = []);
        cond 37 `Warning
          "Missing field 'dev-repo'"
          (t.dev_repo = None);
(*
        cond 38 `Warning
          "Package declares 'depexts', but has no 'post-messages' to help \
           the user out when they are missing"
          (t.depexts <> None && t.post_messages = []);
*)
        cond 39 `Error
          "Command 'make' called directly, use the built-in variable \
           instead"
          (List.exists (function
               | (CString "make", _)::_, _ -> true
               | _ -> false
             ) all_commands);
        cond 40 `Warning
          "Field 'features' is still experimental and not yet to be used on \
           the official repo"
          (t.features <> []);
        (let alpha_flags =
           OpamStd.List.filter_map (function
               | Pkgflag_LightUninstall | Pkgflag_Unknown _ -> None
               | f -> Some OpamFormat.(string_of_value (make_flag f)))
             t.flags
         in
         cond 40 `Warning
           "Package uses flags that aren't recognised by earlier versions in \
            OPAM 1.2 branch. At the moment, you should use a tag \"flags:foo\" \
            instead for compatibility"
           ~detail:alpha_flags
           (alpha_flags <> []));
        (let undep_pkgs =
           List.fold_left
             (fun acc v ->
                match OpamVariable.Full.package v with
                | Some n when
                    t.name = Some n &&
                    not (OpamPackage.Name.Set.mem n all_depends) ->
                  OpamPackage.Name.Set.add n acc
                | _ -> acc)
             OpamPackage.Name.Set.empty all_variables
         in
         cond 41 `Warning
           "Some packages are mentionned in package scripts of features, but \
            there is no dependency or depopt toward them"
           ~detail:OpamPackage.Name.
                     (List.map to_string (Set.elements undep_pkgs))
           (not (OpamPackage.Name.Set.is_empty undep_pkgs)));
        cond 42 `Error
          "The 'dev-repo' field doesn't specify an explicit VCS. You may use \
           URLs of the form \"git+https://\" or a \".hg\" or \".git\" suffix"
          (match t.dev_repo with
           | None | Some (Git _ | Darcs _ | Hg _) -> false
           | Some (Version _ | Local _ | Http _) -> true);
        cond 43 `Error
          "Conjunction used in 'conflicts:' field. Only '|' is allowed"
          (OpamVersion.compare t.opam_version (OpamVersion.of_string "1.3") >= 0 &&
           List.exists (function Atom _ -> false | _ -> true) @@
           OpamFormula.(ors_to_list (to_atom_formula t.conflicts)));
      ]
      in
      OpamStd.List.filter_map (fun x -> x) warnings

    let validate_gen reader filename =
      let warnings, t =
        try
          let f = reader filename in
          let _, good_items, invalid_items =
            Pp.parse ~pos:(pos_file filename)
              (Pp.I.good_fields ~name:"opam-file" ~allow_extensions:true fields)
              f.file_contents
          in
          let warnings =
            List.map (function
                | Section (pos, s) ->
                  3, `Error, Printf.sprintf "Invalid section: %s at %s"
                    s.section_name (string_of_pos pos)
                | Variable (pos, f, _) ->
                  3, `Error, Printf.sprintf "Invalid field: %s at %s"
                    f (string_of_pos pos))
              invalid_items
          in
          let t, warnings =
            let pp =
              Pp.I.fields ~name:"opam-file" ~empty raw_fields -|
              handle_flags_in_tags
            in
            try
              Some (Pp.parse ~pos:(pos_file filename) pp good_items),
              warnings
            with OpamFormat.Bad_format (pos,_,msg) ->
              None,
              warnings @
              [ 2, `Error, Printf.sprintf "File format error: %s%s"
                  (match pos with
                   | Some p -> Printf.sprintf "at %s, " (string_of_pos p)
                   | None -> "")
                  msg ]
          in
          let warnings =
            match OpamPackage.of_filename filename, t with
            | None, _ | _, None -> warnings
            | Some nv, Some t ->
              let name = OpamPackage.name nv in
              let version = OpamPackage.version nv in
              warnings @
              (match t.name with
               | Some tname when tname <> name ->
                 [ 4, `Warning,
                   Printf.sprintf
                     "Field 'name: %S' while the directory name or pinning \
                      implied %S"
                     (OpamPackage.Name.to_string tname)
                     (OpamPackage.Name.to_string name) ]
               | _ -> []) @
              (match t.version with
               | Some tversion when tversion <> version ->
                 [ 4, `Warning,
                   Printf.sprintf
                     "Field 'version: %S' while the directory name or pinning \
                      implied %S"
                     (OpamPackage.Version.to_string tversion)
                     (OpamPackage.Version.to_string version) ]
               | _ -> [])
          in
          warnings, t
        with
        | OpamSystem.File_not_found _ ->
          OpamConsole.error "%s not found" (OpamFilename.prettify filename);
          [0, `Error, "File does not exist"], None
        | Lexer_error _ | Parsing.Parse_error ->
          [1, `Error, "File does not parse"], None
      in
      warnings @ (match t with Some t -> validate t | None -> []),
      t

    let validate_file filename =
      let reader filename =
        let ic = OpamFilename.open_in filename in
        try
          let f = Syntax.of_channel filename ic in
          close_in ic; f
        with e -> close_in ic; raise e
      in
      validate_gen reader filename

    let validate_string filename string =
      let reader filename = Syntax.of_string filename string in
      validate_gen reader filename

    let warns_to_string ws =
      OpamStd.List.concat_map "\n"
        (fun (n, w, s) ->
           let ws = match w with
             | `Warning -> OpamConsole.colorise `yellow "warning"
             | `Error -> OpamConsole.colorise `red "error"
           in
           OpamStd.Format.reformat ~indent:14
             (Printf.sprintf "  %15s %2d: %s" ws n s))
        ws

  end

  (** Package url files (<repo>/packages/.../url) *)

  module URL = struct

    let internal = "url-file"

    type t = {
      url     : address;
      mirrors : address list;
      kind    : repository_kind;
      checksum: string option;
    }

    let create kind ?(mirrors=[]) url =
      {
        url; mirrors; kind; checksum = None;
      }

    let empty_url = "", None

    let empty = {
      url     = empty_url;
      mirrors = [];
      kind    = `local;
      checksum= None;
    }

    let url t = t.url
    let mirrors t = t.mirrors
    let kind t = t.kind
    let checksum t = t.checksum

    let with_url t (url,kind) = { t with url; kind }
    let with_mirrors t mirrors = { t with mirrors }
    let with_checksum t checksum = { t with checksum = Some checksum }

    let fields =
      let with_url ?kind () t (url,autokind) =
        if t.url <> empty_url then OpamFormat.bad_format "Too many URLS"
        else with_url t (url, OpamStd.Option.default autokind kind)
      and url t = (t.url,t.kind) in
      let none _ = None in
      [
        "archive", Pp.ppacc_opt (with_url ()) none Pp.V.url;
        "src", Pp.ppacc (with_url ()) url Pp.V.url;
        "http", Pp.ppacc_opt (with_url ~kind:`http ()) none Pp.V.url;
        "git", Pp.ppacc_opt (with_url ~kind:`git ()) none Pp.V.url;
        "darcs", Pp.ppacc_opt (with_url ~kind:`darcs ()) none Pp.V.url;
        "hg", Pp.ppacc_opt (with_url ~kind:`hg ()) none Pp.V.url;
        "local", Pp.ppacc_opt (with_url ~kind:`local ()) none Pp.V.url;
        "checksum", Pp.ppacc_opt with_checksum checksum
          (Pp.V.string -| Pp.check ~name:"checksum" OpamFilename.valid_digest);
        "mirrors", Pp.ppacc with_mirrors mirrors (Pp.V.map_list Pp.V.address);
      ]

    let pp =
      let name = internal in
      Pp.I.check_fields ~name fields -|
      Pp.I.fields ~name ~empty fields -|
      Pp.check ~name (fun t -> t.url <> empty_url) ~errmsg:"Missing URL"

    let of_syntax filename s =
      Pp.parse pp ~pos:(pos_file filename) s.file_contents

    let of_channel filename ic =
      of_syntax filename (Syntax.of_channel filename ic)

    let of_string filename str =
      of_syntax filename (Syntax.of_string filename str)

    let to_string filename t =
      let s = {
        file_format   = OpamVersion.current;
        file_name     = OpamFilename.to_string filename;
        file_contents = Pp.print pp t;
      } in
      Syntax.to_string s


  end


  (** Optional package.install files (<source>/<pkgname>.install,
      <repo>/packages/.../files/<pkgname>.install) *)

  module Dot_install = struct

    let internal = ".install"

    type t =  {
      bin     : (basename optional * basename option) list;
      sbin    : (basename optional * basename option) list;
      lib     : (basename optional * basename option) list;
      toplevel: (basename optional * basename option) list;
      stublibs: (basename optional * basename option) list;
      share   : (basename optional * basename option) list;
      share_root: (basename optional * basename option) list;
      etc     : (basename optional * basename option) list;
      doc     : (basename optional * basename option) list;
      man     : (basename optional * basename option) list;
      libexec : (basename optional * basename option) list;
      misc    : (basename optional * filename) list;
    }

    let empty = {
      lib      = [];
      bin      = [];
      sbin     = [];
      toplevel = [];
      stublibs = [];
      misc     = [];
      share    = [];
      share_root = [];
      etc      = [];
      man      = [];
      libexec  = [];
      doc      = [];
    }

    let bin t = t.bin
    let sbin t = t.sbin
    let lib t = t.lib
    let toplevel t = t.toplevel
    let stublibs t = t.stublibs
    let misc t = t.misc
    let share t = t.share
    let share_root t = t.share_root
    let etc t = t.etc
    let doc t = t.doc
    let libexec t = t.libexec

    let add_man_section_dir src =
      let file = Filename.basename (OpamFilename.Base.to_string src.c) in
      let section =
        let base =
          if Filename.check_suffix file ".gz"
          then Filename.chop_suffix file ".gz" else file
        in
        try
          let dot = String.rindex base '.' in
          if dot < String.length base - 1 then match base.[dot+1] with
            | '1'..'8' as c -> Printf.sprintf "man%c" c
            | _ -> raise Not_found
          else raise Not_found
        with Not_found ->
          OpamConsole.error_and_exit
            "Manpage %s does not have a recognised suffix, \
             and no destination is specified" (OpamFilename.Base.to_string src.c)
      in
      OpamFilename.Base.of_string (Filename.concat section file)

    let man t =
      List.map (fun (src, dst) ->
          src,
          match dst with
          | Some _ -> dst
          | None -> Some (add_man_section_dir src)
        ) t.man

    let s_lib      = "lib"
    let s_bin      = "bin"
    let s_sbin     = "sbin"
    let s_misc     = "misc"
    let s_toplevel = "toplevel"
    let s_stublibs = "stublibs"
    let s_share    = "share"
    let s_share_root = "share_root"
    let s_etc      = "etc"
    let s_doc      = "doc"
    let s_man      = "man"
    let s_libexec  = "libexec"

    let valid_fields = [
      Syntax.s_opam_version;
      s_lib;
      s_bin;
      s_sbin;
      s_toplevel;
      s_stublibs;
      s_misc;
      s_share;
      s_share_root;
      s_etc;
      s_doc;
      s_man;
      s_libexec;
    ]

    (* Filenames starting by ? are not always present. *)
    let optional_of_string str =
      let mk = OpamFilename.Base.of_string in
      if String.length str > 0 && str.[0] = '?' then
        { optional = true;
          c        = mk (String.sub str 1 (String.length str - 1)) }
      else
        { optional = false;
          c        = mk str }

    let string_of_optional t =
      let o = if t.optional then "?" else "" in
      Printf.sprintf "%s%s" o (OpamFilename.Base.to_string t.c)

    let to_string filename t =
      let open OpamFormat in
      let mk =
        make_list
          (make_option
             (fun src -> make_string (string_of_optional src))
             (fun dst ->
                [make_string (OpamFilename.Base.to_string dst)])) in
      let mk_misc =
        make_list
          (fun (src,dst) ->
             make_option
               (fun src -> make_string (string_of_optional src))
               (fun dst ->
                  [make_string (OpamFilename.to_string dst)])
               (src, Some dst)) in
      let s = {
        file_format   = OpamVersion.current;
        file_name     = OpamFilename.to_string filename;
        file_contents = [
          make_variable (s_bin     , mk      t.bin);
          make_variable (s_sbin    , mk      t.sbin);
          make_variable (s_lib     , mk      t.lib);
          make_variable (s_toplevel, mk      t.toplevel);
          make_variable (s_stublibs, mk      t.stublibs);
          make_variable (s_share   , mk      t.share);
          make_variable (s_share_root, mk    t.share_root);
          make_variable (s_etc     , mk      t.etc);
          make_variable (s_doc     , mk      t.doc);
          make_variable (s_man     , mk      t.man);
          make_variable (s_libexec , mk      t.libexec);
          make_variable (s_misc    , mk_misc t.misc);
        ]
      } in
      Syntax.to_string s

    let of_syntax _ s =
      let _permissive = Syntax.check ~versioned:false s valid_fields in
      let src = OpamFormat.parse_string @> optional_of_string in
      let mk field =
        let dst = OpamFormat.parse_string @> OpamFilename.Base.of_string in
        let fn = OpamFormat.parse_single_option src dst in
        OpamFormat.assoc_list s.file_contents field (OpamFormat.parse_list fn) in
      let misc =
        let absolute_filename s =
          if not (Filename.is_relative s) then
            OpamFilename.of_string s
          else
            OpamSystem.internal_error "%s is not an absolute filename." s in
        let dst = OpamFormat.parse_string @> absolute_filename in
        let fn = OpamFormat.parse_pair src dst in
        OpamFormat.assoc_list s.file_contents s_misc (OpamFormat.parse_list fn) in
      let bin      = mk s_bin      in
      let sbin     = mk s_sbin     in
      let lib      = mk s_lib      in
      let toplevel = mk s_toplevel in
      let stublibs = mk s_stublibs in
      let share    = mk s_share    in
      let share_root = mk s_share_root in
      let etc      = mk s_etc      in
      let doc      = mk s_doc      in
      let man      = mk s_man      in
      let libexec  = mk s_libexec  in
      { lib; bin; sbin; misc; toplevel; stublibs; share; share_root; etc; doc; man; libexec }

    let of_channel filename ic =
      of_syntax filename (Syntax.of_channel filename ic)

    let of_string filename str =
      of_syntax filename (Syntax.of_string filename str)

  end

  module Comp = struct

    let internal = "comp"

    type t = {
      opam_version : opam_version ;
      name         : compiler ;
      version      : compiler_version ;
      preinstalled : bool;
      src          : address option ;
      kind         : repository_kind ;
      patches      : filename list ;
      configure    : string list ;
      make         : string list ;
      build        : command list ;
      packages     : formula ;
      env          : (string * string * string) list;
      tags         : string list;
    }

    let empty = {
      opam_version = OpamVersion.current;
      name         = OpamCompiler.of_string "<none>";
      version      = OpamCompiler.Version.of_string "<none>";
      src          = None;
      kind         = `local;
      preinstalled = false;
      patches   = [];
      configure = [];
      make      = [];
      build     = [];
      packages  = OpamFormula.Empty;
      env       = [];
      tags      = [];
    }

    let create_preinstalled name version packages env =
      let mk n = Atom (n, Empty) in
      let packages = OpamFormula.ands (List.map mk packages) in
      { empty with name; version; preinstalled = true; packages; env }

    let s_opam_version = "opam-version"
    let s_name      = "name"
    let s_version   = "version"
    let s_patches   = "patches"
    let s_configure = "configure"
    let s_make      = "make"
    let s_build     = "build"
    let s_packages  = "packages"
    let s_env       = "env"
    let s_preinstalled = "preinstalled"
    let s_tags      = "tags"
    let s_src       = "src"
    let s_http      = "http"
    let s_archive   = "archive"
    let s_git       = "git"
    let s_darcs     = "darcs"
    let s_hg        = "hg"
    let s_local     = "local"

    let opam_1_0_fields = [
      s_opam_version;
      s_name;
      s_version;
      s_src;
      s_patches;
      s_configure;
      s_make;
      s_build;
      s_packages;
      s_env;
      s_preinstalled;
      s_tags;
    ]

    let opam_1_1_fields = [
      s_archive;
      s_http;
      s_git;
      s_darcs;
      s_hg;
      s_local;
    ]

    let to_1_0_fields k v =
      if List.mem k opam_1_1_fields then
        if k = s_archive
        || k = s_http
        || k = s_git
        || k = s_darcs
        || k = s_hg
        || k = s_local then Some (s_src, v)
        else None
      else Some (k, v)

    let to_1_0 file =
      let file = OpamFormat.map to_1_0_fields file in
      Syntax.to_1_0 file

    let valid_fields =
      opam_1_0_fields @ opam_1_1_fields

    let name t = t.name
    let version t = t.version
    let patches t = t.patches
    let configure t = t.configure
    let make t = t.make
    let build t = t.build
    let src t = t.src
    let kind t = t.kind
    let opam_version t = t.opam_version

    let packages t = t.packages
    let preinstalled t = t.preinstalled
    let env t = t.env
    let tags t = t.tags

    let with_src t src = {t with src}
    let with_patches t patches = {t with patches}
    let with_configure t configure = {t with configure}
    let with_make t make = {t with make}
    let with_build t build = {t with build}
    let with_packages t packages = {t with packages}

    let url_and_kind ~src ~archive ~http ~git ~darcs ~hg ~local =
      let extract =
        match src, archive, http, git, darcs, hg, local with
        | None  , None  , None  , None  , None  , None  , None   -> None
        | Some x, None  , None  , None  , None  , None  , None
        | None  , Some x, None  , None  , None  , None  , None   -> Some (x, None)
        | None  , None  , Some x, None  , None  , None  , None   -> Some (x, Some `http)
        | None  , None  , None  , Some x, None  , None  , None   -> Some (x, Some `git)
        | None  , None  , None  , None  , Some x, None  , None   -> Some (x, Some `darcs)
        | None  , None  , None  , None  , None  , Some x, None   -> Some (x, Some `hg)
        | None  , None  , None  , None  , None  , None  , Some x -> Some (x, Some `local)
        | _ -> OpamFormat.bad_format "Too many URLS"
      in
      match extract with
      | None -> None
      | Some (url, kind_opt) ->
        try
          let url, kind = parse_url url in
          Some (url, OpamStd.Option.default kind kind_opt)
        with Invalid_argument s -> OpamFormat.bad_format "%s" s

    let of_syntax filename file =
      let permissive = Syntax.check file valid_fields in
      let s = file.file_contents in
      let opam_version = OpamFormat.assoc s s_opam_version
          (OpamFormat.parse_string @> OpamVersion.of_string) in
      let name_d, version_d = match OpamCompiler.of_filename filename with
        | None   ->
          OpamFormat.bad_format "Filename %S isn't in the form <name>.<version>"
            (OpamFilename.to_string filename)
        | Some c -> c, OpamCompiler.version c in
      let name =
        try OpamFormat.assoc_default name_d s s_name
              (OpamFormat.parse_string @> OpamCompiler.of_string)
        with OpamFormat.Bad_format _ when permissive -> name_d
      in
      if name_d <> name then (
        OpamConsole.warning "The file %s contains a bad 'name' field: %s instead of %s"
          (OpamFilename.to_string filename)
          (OpamCompiler.to_string name)
          (OpamCompiler.to_string name_d);
        if OpamFormatConfig.(!r.strict) then
          OpamConsole.error_and_exit "Strict mode: bad compiler name"
      );
      let version =
        try OpamFormat.assoc_default version_d s s_version
              OpamFormat.parse_compiler_version
        with OpamFormat.Bad_format _ when permissive -> version_d
      in
      if name <> OpamCompiler.system && version_d <> version then (
        OpamConsole.warning
          "The file %s contains a bad 'version' field: %s instead of %s"
          (OpamFilename.to_string filename)
          (OpamCompiler.Version.to_string version)
          (OpamCompiler.Version.to_string version_d);
        if OpamFormatConfig.(!r.strict) then
          OpamConsole.error_and_exit "Strict mode: bad compiler version"
      );
      let address field =
        try OpamFormat.assoc_option s field
              (OpamFormat.parse_string @> address_of_string)
        with OpamFormat.Bad_format _ when permissive -> None
      in
      let src = address s_src in
      let archive = address s_archive in
      let http = address s_http in
      let git = address s_git in
      let darcs = address s_darcs in
      let hg = address s_hg in
      let local = address s_local in
      let src, kind =
        try match url_and_kind ~src ~archive ~http ~git ~darcs ~hg ~local
          with Some (u,k) -> Some u, k | None -> None, `http
        with OpamFormat.Bad_format _ when permissive -> None, `http
      in
      let safe dft f x y z =
        try f x y z
        with OpamFormat.Bad_format _ when permissive -> dft
      in
      let assoc_string_list x y =
        try OpamFormat.assoc_string_list x y
        with OpamFormat.Bad_format _ when permissive -> []
      in
      let patches =
        safe [] OpamFormat.assoc_list s s_patches
          (OpamFormat.parse_list (OpamFormat.parse_string @> OpamFilename.raw)) in
      let configure = assoc_string_list s s_configure in
      let make = assoc_string_list s s_make in
      let build = safe [] OpamFormat.assoc_list s s_build OpamFormat.parse_commands in
      let env = safe [] OpamFormat.assoc_list s s_env
          (OpamFormat.parse_list_list OpamFormat.parse_env_variable) in
      let packages = safe OpamFormula.Empty
          (OpamFormat.assoc_default OpamFormula.Empty)
          s s_packages OpamFormat.(parse_formula `Conj parse_constraints) in
      let preinstalled = safe false
          (OpamFormat.assoc_default false) s s_preinstalled OpamFormat.parse_bool in
      let tags = assoc_string_list s s_tags in

      if build <> [] && (configure @ make) <> [] && not permissive then
        OpamConsole.error_and_exit
          "%s: You cannot use 'build' and 'make'/'configure' fields at the same time."
          (OpamFilename.to_string filename);
      { opam_version; name; version; src; kind;
        patches; configure; make; build;
        packages; preinstalled; env;
        tags;
      }

    let of_channel filename ic =
      of_syntax filename (Syntax.of_channel filename ic)

    let of_string filename str =
      of_syntax filename (Syntax.of_string filename str)

    let to_string filename s =
      let open OpamFormat in
      let src = match s.src with
        | Some x -> Some (string_of_repository_kind s.kind, x)
        | None -> None in
      let s = {
        file_format   = s.opam_version;
        file_name     = OpamFilename.to_string filename;
        file_contents = [
          make_variable (s_opam_version,
                    make_string (OpamVersion.to_string s.opam_version))
        ] @ (
            match OpamCompiler.of_filename filename with
            | None   -> [make_variable (s_name,
                                   make_string
                                     (OpamCompiler.to_string s.name))]
            | Some _ -> []
          ) @ [
            make_variable (s_version,
                      make_string
                        (OpamCompiler.Version.to_string s.version))
          ] @ (
            match src with
            | None       -> []
            | Some (s,c) -> [make_variable (s, make_string (string_of_address c))]
          ) @ [
            make_variable (s_patches,
                      make_list
                        (OpamFilename.to_string @> make_string)
                        s.patches);
            make_variable (s_configure, make_string_list s.configure);
            make_variable (s_make, make_string_list s.make);
            make_variable (s_build, make_commands s.build);
            make_variable (s_packages, make_formula `Conj make_constraints s.packages);
            make_variable (s_env, make_list make_env_variable s.env);
            make_variable (s_tags, make_string_list s.tags);
          ] @ (
            if not s.preinstalled then []
            else [ make_variable (s_preinstalled, make_bool s.preinstalled) ])
      } in
      Syntax.to_string s

  end

end







module type F = sig
  val internal : string
  type t
  val empty : t
  val of_channel : filename -> in_channel  -> t
  val of_string : filename -> string -> t
  val to_string : filename -> t -> string
end

let read_files = ref []
let write_files = ref []
let print_stats () =
  let aux kind = function
    | [] -> ()
    | l  ->
      OpamConsole.msg "%d files %s:\n  %s\n%!"
        (List.length !read_files) kind (String.concat "\n  " l)
  in
  aux "read" !read_files;
  aux "write" !write_files

module Make (F : F) = struct

  let log ?level fmt =
    OpamConsole.log (Printf.sprintf "FILE(%s)" F.internal) ?level fmt
  let slog = OpamConsole.slog

  let write f v =
    let filename = OpamFilename.prettify f in
    let chrono = OpamConsole.timer () in
    OpamFilename.write f (F.to_string f v);
    write_files := filename :: !write_files;
    log "Wrote %s in %.3fs" filename (chrono ())

  let read f =
    let filename = OpamFilename.prettify f in
    read_files := filename :: !read_files;
    let chrono = OpamConsole.timer () in
    try
      let ic = OpamFilename.open_in f in
      try
        let r = F.of_channel f ic in
        close_in ic;
        log ~level:3 "Read %s in %.3fs" filename (chrono ());
        r
      with e -> close_in ic; raise e
    with
      | OpamSystem.File_not_found s ->
        OpamSystem.internal_error "File %s does not exist" s
      | Lexer_error _ | Parsing.Parse_error as e ->
        if OpamFormatConfig.(!r.strict) then
          OpamConsole.error_and_exit "Strict mode: aborting"
        else raise e (* Message already printed *)
      | e ->
        OpamStd.Exn.fatal e;
        OpamConsole.error "%s" (OpamFormat.string_of_bad_format ~file:f e);
        if OpamFormatConfig.(!r.strict) then OpamStd.Sys.exit 66
        else raise e

  let safe_read f =
    if OpamFilename.exists f then
      try read f with OpamFormat.Bad_format _ ->
        OpamConsole.msg "[skipped]\n";
        F.empty
    else (
      log ~level:2 "Cannot find %a" (slog OpamFilename.to_string) f;
      F.empty
    )

  let dummy_file = OpamFilename.raw "<dummy>"

  let read_from_f f input =
    try f input with
    | OpamFormat.Bad_format _ as e ->
      OpamConsole.error "%s" (OpamFormat.string_of_bad_format e);
      if OpamFormatConfig.(!r.strict) then
        OpamConsole.error_and_exit "Strict mode: aborting"
      else raise e

  let read_from_channel ?(filename=dummy_file) ic =
    read_from_f (F.of_channel filename) ic

  let read_from_string ?(filename=dummy_file) str =
    read_from_f (F.of_string filename) str

  let write_to_channel oc str =
    output_string oc (F.to_string dummy_file str)

end

open X

module Syntax = Syntax

module type IO_FILE = sig
  type t
  val empty: t
  val write: filename -> t -> unit
  val read : filename -> t
  val safe_read: filename -> t
  val read_from_channel: ?filename:filename -> in_channel -> t
  val read_from_string: ?filename:filename -> string -> t
  val write_to_channel: out_channel -> t -> unit
end

module Lines = struct
  include Lines
  include Make (Lines)
end

module Config = struct
  include Config
  include Make (Config)
end

module Package_index = struct
  include Package_index
  include Make (Package_index)
end

module Compiler_index = struct
  include Compiler_index
  include Make (Compiler_index)
end

module Pinned = struct
  include Pinned
  include Make (Pinned)
end

module Repo = struct
  include Repo
  include Make(Repo)
end

module Repo_config = struct
  include Repo_config
  include Make (Repo_config)
end

module Descr = struct
  include Descr
  include Make (Descr)
end

module Aliases = struct
  include Aliases
  include Make (Aliases)
end

module Reinstall = struct
  include Reinstall
  include Make (Reinstall)
end

module OPAM = struct
  include OPAM
  include Make (OPAM)
end

module Dot_install = struct
  include Dot_install
  include Make (Dot_install)
end

module Dot_config = struct
  include Dot_config
  include Make (Dot_config)
end

module Export = struct
  include Export
  include Make(Export)
end

module Installed = struct
  include Installed
  include Make (Installed)
end

module Installed_roots = struct
  include Installed_roots
  include Make (Installed_roots)
end

module Subst = struct
  include Subst
  include Make (Subst)
end

module Comp = struct
  include Comp
  include Make (Comp)
end

module URL = struct
  include URL
  include Make (URL)
end

module File_attributes = struct
  include File_attributes
  include Make(File_attributes)
end
