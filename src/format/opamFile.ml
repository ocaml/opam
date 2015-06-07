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

module X = struct

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

  end

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
    let check ?allow_major ?(versioned=true) =
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
          let invalids = OpamFormat.invalid_fields f.file_contents fields in
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

  module Prefix = struct

    let internal = "prefix"

    type t = string name_map

    let empty = OpamPackage.Name.Map.empty

    let of_channel filename ic =
      let lines = Lines.of_channel filename ic in
      List.fold_left (fun map -> function
        | []          -> map
        | [nv;prefix] -> OpamPackage.Name.Map.add (OpamPackage.Name.of_string nv)
                           prefix map
        | s ->
          OpamConsole.error_and_exit
            "%S is not a valid prefix line"
            (String.concat " " s)
      ) OpamPackage.Name.Map.empty lines

    let to_string filename s =
      let lines =
        OpamPackage.Name.Map.fold (fun nv prefix l ->
          [OpamPackage.Name.to_string nv; prefix] :: l
        ) s [] in
      Lines.to_string filename lines

  end

  module Filenames = struct

    let internal = "filenames"

    type t = filename_set

    let empty = OpamFilename.Set.empty

    let of_channel filename ic =
      let lines = Lines.of_channel filename ic in
      let lines = OpamStd.List.filter_map (function
          | []  -> None
          | [f] -> Some (OpamFilename.of_string f)
          | s   -> OpamConsole.error_and_exit "%S is not a valid filename"
                     (String.concat " " s)
        ) lines in
      OpamFilename.Set.of_list lines

    let to_string filename s =
      let lines =
        List.rev_map
          (fun f -> [OpamFilename.to_string f])
          (OpamFilename.Set.elements s) in
      Lines.to_string filename lines

  end

  module File_attributes = struct

    let internal = "file_attributes"

    type t = file_attribute_set

    let empty = OpamFilename.Attribute.Set.empty

    let of_channel filename ic =
      let lines = Lines.of_channel filename ic in
      let rs = OpamStd.List.filter_map (function
          | [] -> None
          | [s] -> (* backwards-compat *)
            Some (OpamFilename.Attribute.of_string_list (OpamStd.String.split s ' '))
          | l  ->
            Some (OpamFilename.Attribute.of_string_list l)
        ) lines in
      OpamFilename.Attribute.Set.of_list rs

    let to_string filename t =
      let lines =
        List.rev_map
          (fun r -> OpamFilename.Attribute.to_string_list r)
          (OpamFilename.Attribute.Set.elements t) in
      Lines.to_string filename lines

  end

  module URL = struct

    let internal = "url"

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

    let empty = {
      url     = "<none>", None;
      mirrors = [];
      kind    = `local;
      checksum= None;
    }

    let s_archive = "archive"
    let s_src = "src"
    let s_http = "http"
    let s_checksum = "checksum"
    let s_git = "git"
    let s_darcs = "darcs"
    let s_hg = "hg"
    let s_local = "local"
    let s_mirrors = "mirrors"

    let valid_fields = [
      Syntax.s_opam_version;
      s_archive;
      s_src;
      s_http;
      s_git;
      s_darcs;
      s_hg;
      s_local;
      s_checksum;
      s_mirrors;
    ]

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

    let of_channel filename ic =
      let s = Syntax.of_channel filename ic in
      let permissive = Syntax.check ~versioned:false s valid_fields in
      let get f =
        try
          OpamFormat.assoc_option s.file_contents f
            (OpamFormat.parse_string @> address_of_string)
        with OpamFormat.Bad_format _ when permissive -> None
      in
      let archive  = get s_archive in
      let http     = get s_http in
      let src      = get s_src in
      let git      = get s_git in
      let darcs    = get s_darcs in
      let hg       = get s_hg in
      let local    = get s_local in
      let mirrors =
        OpamFormat.assoc_list s.file_contents s_mirrors
          (OpamFormat.parse_list (OpamFormat.parse_string @> address_of_string)) in
      let checksum =
        OpamFormat.assoc_option s.file_contents s_checksum OpamFormat.parse_string in
      let url, kind =
        match url_and_kind ~src ~archive ~http ~git ~darcs ~hg ~local
        with Some x -> x | None -> OpamFormat.bad_format "URL is missing" in
      { url; mirrors; kind; checksum }

    let to_string filename t =
      let url_name = string_of_repository_kind t.kind in
      let s = {
        file_format   = OpamVersion.current;
        file_name     = OpamFilename.to_string filename;
        file_contents = [
          OpamFormat.make_variable (
                    url_name ,
                    OpamFormat.make_string (string_of_address t.url));
        ] @ (
            if t.mirrors = [] then [] else
              [OpamFormat.make_variable (s_mirrors ,
                         OpamFormat.make_list
                           (string_of_address @> OpamFormat.make_string)
                           t.mirrors)]
        ) @ match t.checksum with
            | None   -> []
            | Some c -> [OpamFormat.make_variable (s_checksum, OpamFormat.make_string c)]
      } in
      Syntax.to_string s

    let url t = t.url
    let mirrors t = t.mirrors
    let kind t = t.kind
    let checksum t = t.checksum

    let with_checksum t checksum = { t with checksum = Some checksum }

  end

  module Export = struct

    let internal = "export"

    type t = package_set * package_set * pin_option OpamPackage.Name.Map.t

    let empty = (OpamPackage.Set.empty, OpamPackage.Set.empty, OpamPackage.Name.Map.empty)

    let of_channel filename ic =
      let lines = Lines.of_channel filename ic in
      let state = function
        | "root" -> `Root
        | "noroot" | "installed" -> `Installed
        | "uninstalled" -> `Uninstalled
        | s ->
          OpamConsole.error_and_exit "Invalid installation status (col. 3) in %s: %S"
            (OpamFilename.to_string filename) s
      in
      let add (installed,roots,pinned) n v state p =
        let name = OpamPackage.Name.of_string n in
        let nv = OpamPackage.create name (OpamPackage.Version.of_string v) in
        let installed =
          if state <> `Uninstalled then OpamPackage.Set.add nv installed
          else installed in
        let roots =
          if state = `Root then OpamPackage.Set.add nv roots
          else roots in
        let pinned = match p with
          | None -> pinned
          | Some (kind,p) ->
            OpamPackage.Name.Map.add name (pin_option_of_string ~kind p) pinned
        in
        installed, roots, pinned
      in
      List.fold_left (fun acc -> function
          | []        -> acc
          | [n; v]    -> add acc n v `Root None
          | [n; v; r] -> add acc n v (state r) None
          | [n; v; r; pk; p] ->
            add acc n v (state r) (Some (pin_kind_of_string pk,p))
          | l ->
            OpamConsole.error_and_exit "Invalid line in %s: %S"
              (OpamFilename.to_string filename)
              (String.concat " " l)
        )
        (OpamPackage.Set.empty, OpamPackage.Set.empty, OpamPackage.Name.Map.empty)
        lines

    let to_string _ (installed, roots, pinned) =
      let buf = Buffer.create 1024 in
      let print_pin pin =
        Printf.sprintf "\t%s\t%s"
          (string_of_pin_kind (kind_of_pin_option pin))
          (string_of_pin_option pin) in
      OpamPackage.Set.iter (fun nv ->
          let name = OpamPackage.name nv in
          Printf.bprintf buf "%s\t%s\t%s%s\n"
            (OpamPackage.Name.to_string (OpamPackage.name nv))
            (OpamPackage.Version.to_string (OpamPackage.version nv))
            (if OpamPackage.Set.mem nv roots then "root" else "installed")
            (try print_pin (OpamPackage.Name.Map.find name pinned)
             with Not_found -> "")
        ) installed;
      let installed_names = OpamPackage.names_of_packages installed in
      OpamPackage.Name.Map.iter (fun name pin ->
          if not (OpamPackage.Name.Set.mem name installed_names) then
            Printf.bprintf buf "%s\t--\tuninstalled\t%s\n"
              (OpamPackage.Name.to_string name)
              (print_pin pin)
        ) pinned;
      Buffer.contents buf

  end

  module Installed = struct

    let internal = "installed"

    type t = package_set

    let empty = OpamPackage.Set.empty

    let check t =
      let map = OpamPackage.to_map t in
      OpamPackage.Name.Map.iter (fun n vs ->
        if OpamPackage.Version.Set.cardinal vs <> 1 then
          OpamConsole.error_and_exit "Multiple versions installed for package %s: %s"
            (OpamPackage.Name.to_string n) (OpamPackage.Version.Set.to_string vs)
      ) map

    let of_channel filename ic =
      let lines = Lines.of_channel filename ic in
      let map,_ =
        List.fold_left (fun (map,i) -> function
            | [] -> map, i+1
            | [name; version] ->
              OpamPackage.Set.add
                (OpamPackage.create
                   (OpamPackage.Name.of_string name)
                   (OpamPackage.Version.of_string version))
                map,
              i+1
            | s ->
              OpamConsole.error "At %s:%d:\n  skipped invalid line %S"
                (OpamFilename.prettify filename) i (String.concat " " s);
              map, i+1
          ) (empty,1) lines in
      map

    let to_string _ t =
      check t;
      let buf = Buffer.create 1024 in
      OpamPackage.Set.iter
        (fun nv ->
          Printf.bprintf buf "%s %s\n"
            (OpamPackage.Name.to_string (OpamPackage.name nv))
            (OpamPackage.Version.to_string (OpamPackage.version nv)))
        t;
      Buffer.contents buf

  end

  module Installed_roots = struct

    include Installed

    let internal = "installed.roots"

  end

  module Reinstall = struct

    include Installed

    let internal = "reinstall"

  end

  module Repo_index (A : OpamStd.ABSTRACT) = struct

    let internal = "repo-index"

    type t = (repository_name * string option) A.Map.t

    let empty = A.Map.empty

    let of_channel filename ic =
      let lines = Lines.of_channel filename ic in
      List.fold_left (fun map -> function
          | [] | [_]                 -> map
          | a_s :: repos_s :: prefix ->
            let a = A.of_string a_s in
            if A.Map.mem a map then
              OpamConsole.error_and_exit "multiple lines for %s" a_s
            else
              let repo_name = OpamRepositoryName.of_string repos_s in
              let prefix = match prefix with
                | []  -> None
                | [p] -> Some p
                | _   -> OpamConsole.error_and_exit "Too many prefixes" in
              A.Map.add a (repo_name, prefix) map
        ) A.Map.empty lines

    let to_string filename map =
      let lines = A.Map.fold (fun nv (repo_name, prefix) lines ->
          let repo_s = OpamRepositoryName.to_string repo_name in
          let prefix_s = match prefix with
            | None   -> []
            | Some p -> [p] in
          (A.to_string nv :: repo_s :: prefix_s) :: lines
        ) map [] in
      Lines.to_string filename (List.rev lines)

  end

  module Package_index = Repo_index(OpamPackage)

  module Compiler_index = Repo_index(OpamCompiler)

  module Pinned = struct

    let internal = "pinned"

    type t = pin_option OpamPackage.Name.Map.t

    let empty = OpamPackage.Name.Map.empty

    let of_channel filename ic =
      let lines = Lines.of_channel filename ic in
      let add name_s pin map =
        let name = OpamPackage.Name.of_string name_s in
        if OpamPackage.Name.Map.mem name map then
          OpamConsole.error_and_exit "multiple lines for package %s" name_s
        else
          OpamPackage.Name.Map.add name pin map in
      List.fold_left (fun map -> function
        | []           -> map
        | [name_s; x]  -> add name_s (pin_option_of_string x) map
        | [name_s;k;x] ->
          let kind = Some (pin_kind_of_string k) in
          add name_s (pin_option_of_string ?kind x) map
        | _     -> OpamConsole.error_and_exit "too many pinning options"
      ) OpamPackage.Name.Map.empty lines

    let to_string filename map =
      let lines = OpamPackage.Name.Map.fold (fun name pin lines ->
          let kind = kind_of_pin_option pin in
          let l = [
            OpamPackage.Name.to_string name;
            string_of_pin_kind kind;
            string_of_pin_option pin
          ] in
          l :: lines
        ) map [] in
      Lines.to_string filename (List.rev lines)

  end

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

    let s_name = "name"
    let s_kind = "kind"
    let s_address = "address"
    let s_priority = "priority"
    let s_root = "root"

    let of_channel filename ic =
      let s = Syntax.of_channel filename ic in
      let repo_name =
        OpamFormat.assoc s.file_contents s_name
          (OpamFormat.parse_string @> OpamRepositoryName.of_string) in
      let repo_address =
        OpamFormat.assoc s.file_contents s_address
          (OpamFormat.parse_string @> address_of_string) in
      let repo_kind =
        OpamFormat.assoc s.file_contents s_kind
          (OpamFormat.parse_string @> repository_kind_of_string) in
      let repo_priority =
        OpamFormat.assoc_default 0 s.file_contents s_priority OpamFormat.parse_int in
      let repo_root =
        match OpamFormat.assoc_option s.file_contents s_root
                (OpamFormat.parse_string @> OpamFilename.raw_dir)
        with None   -> assert false
           | Some f -> f in
      { repo_name; repo_address; repo_kind; repo_priority; repo_root }

    let to_string filename t =
      let s = {
        file_format   = OpamVersion.current;
        file_name     = OpamFilename.to_string filename;
        file_contents = [
          OpamFormat.make_variable (s_name    ,
                    OpamFormat.make_string (OpamRepositoryName.to_string t.repo_name));
          OpamFormat.make_variable (s_address ,
                    OpamFormat.make_string (string_of_address t.repo_address));
          OpamFormat.make_variable (s_kind    ,
                    OpamFormat.make_string (string_of_repository_kind t.repo_kind));
          OpamFormat.make_variable (s_priority,
                    OpamFormat.make_int t.repo_priority);
          OpamFormat.make_variable (s_root,
                    OpamFormat.make_string (OpamFilename.Dir.to_string t.repo_root));
        ] } in
      Syntax.to_string s

  end

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

    let of_string str =
      let head, tail =
        match OpamStd.String.cut_at str '\n' with
        | None       -> str, ""
        | Some (h,t) -> h, t in
      head, tail

    let to_string _ = full

  end

  module Aliases = struct

    let internal = "aliases"

    type t = compiler switch_map

    let empty = OpamSwitch.Map.empty

    let to_string filename t =
      let l =
        OpamSwitch.Map.fold (fun switch compiler lines ->
          [OpamSwitch.to_string switch; OpamCompiler.to_string compiler] :: lines
        ) t [] in
      Lines.to_string filename l

    let of_channel filename ic =
      let l = Lines.of_channel filename ic in
      List.fold_left (fun map -> function
        | []            -> map
        | [switch; comp] -> OpamSwitch.Map.add (OpamSwitch.of_string switch)
                              (OpamCompiler.of_string comp) map
        | _             -> failwith "switches"
      ) OpamSwitch.Map.empty l

  end

  module Config = struct

    let internal = "config"

    type t = {
      opam_version  : opam_version;
      repositories  : repository_name list ;
      switch        : switch;
      jobs          : int;
      dl_tool       : arg list option;
      dl_jobs       : int;
      solver_criteria      : (solver_criteria * string) list;
      solver        : arg list option;
    }

    let with_repositories t repositories = { t with repositories }
    let with_switch t switch = { t with switch }
    let with_current_opam_version t = { t with opam_version = OpamVersion.current_nopatch }
    let with_criteria t solver_criteria = { t with solver_criteria }
    let with_solver t solver = { t with solver }

    let opam_version t = t.opam_version
    let repositories t = t.repositories
    let switch t = t.switch
    let jobs t = t.jobs
    let dl_tool t = t.dl_tool
    let dl_jobs t = t.dl_jobs
    let criteria t = t.solver_criteria
    let solver t = t.solver

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

    let s_opam_version = "opam-version"
    let s_repositories = "repositories"
    let s_switch = "switch"
    let s_switch1 = "alias"
    let s_switch2 = "ocaml-version"

    let s_jobs = "jobs"
    let s_dl_tool = "download-command"
    let s_dl_jobs = "download-jobs"
    let s_criteria = "solver-criteria"
    let s_upgrade_criteria = "solver-upgrade-criteria"
    let s_fixup_criteria = "solver-fixup-criteria"
    let s_solver = "solver"

    let s_cores = "cores"

    let s_system_version1 = "system_ocaml-version"
    let s_system_version2 = "system-ocaml-version"

    let valid_fields = [
      s_opam_version;
      s_repositories;
      s_switch;
      s_jobs;
      s_dl_tool;
      s_dl_jobs;
      s_criteria;
      s_upgrade_criteria;
      s_fixup_criteria;
      s_solver;

      (* these fields are no longer useful, but we keep them for backward
         compatibility *)
      s_switch1;
      s_switch2;
      s_system_version1;
      s_system_version2;
      s_cores;
    ]

    let of_channel filename ic =
      let s = Syntax.of_channel filename ic in
      let permissive = Syntax.check s valid_fields in
      let opam_version = OpamFormat.assoc s.file_contents s_opam_version
          (OpamFormat.parse_string @> OpamVersion.of_string) in
      let repositories =
        try
          OpamFormat.assoc_list s.file_contents s_repositories
            (OpamFormat.parse_list
               (OpamFormat.parse_string @> OpamRepositoryName.of_string))
        with OpamFormat.Bad_format _ when permissive -> []
      in
      let mk_switch str =
        OpamFormat.assoc_option s.file_contents str
          (OpamFormat.parse_string @> OpamSwitch.of_string) in
      let switch  = mk_switch s_switch in
      let switch1 = mk_switch s_switch1 in
      let switch2 = mk_switch s_switch2 in
      let switch =
        match OpamStd.Option.Op.(switch ++ switch1 ++ switch2) with
        | Some v -> v
        | None -> OpamConsole.error_and_exit
                    "No current switch defined in %s."
                    (OpamFilename.to_string filename) in
      let jobs =
        try
        let mk str =
          OpamFormat.assoc_option s.file_contents str OpamFormat.parse_int in
        match OpamStd.Option.Op.(mk s_jobs ++ mk s_cores) with
        | Some i -> i
        | None -> 1
        with OpamFormat.Bad_format _ when permissive -> 1
      in

      let dl_tool =
        try
          OpamFormat.assoc_option s.file_contents s_dl_tool
            OpamFormat.parse_single_command
        with OpamFormat.Bad_format _ when permissive -> None
      in

      let dl_jobs =
        try
        match OpamFormat.assoc_option s.file_contents s_dl_jobs
                OpamFormat.parse_int with
        | Some i -> i
        | None -> 1
        with OpamFormat.Bad_format _ when permissive -> 1
      in

      let criteria =
        let crit kind fld acc =
          match
            OpamFormat.assoc_option s.file_contents fld OpamFormat.parse_string
          with
          | Some c -> (kind,c)::acc
          | None -> acc in
        []
        |> crit `Fixup s_fixup_criteria
        |> crit `Upgrade s_upgrade_criteria
        |> crit `Default s_criteria
      in

      let solver =
        try
          OpamFormat.assoc_option s.file_contents s_solver
            OpamFormat.parse_single_command
        with OpamFormat.Bad_format _ when permissive -> None
      in
      { opam_version; repositories; switch; jobs; dl_tool; dl_jobs;
        solver_criteria = criteria; solver }

    let to_string filename t =
      let criteria =
        let mk kind s acc =
          try
            let c = List.assoc kind t.solver_criteria in
            OpamFormat.make_variable (s, OpamFormat.make_string c) :: acc
          with Not_found -> acc in
        []
        |> mk `Fixup s_fixup_criteria
        |> mk `Upgrade s_upgrade_criteria
        |> mk `Default s_criteria
      in
      let solver = match t.solver with
        | None -> []
        | Some s ->
          [OpamFormat.make_variable
             (s_solver, OpamFormat.make_single_command s)]
      in
      let download_tool = match t.dl_tool with
        | None -> []
        | Some dlt ->
          [OpamFormat.make_variable
             (s_dl_tool, OpamFormat.make_single_command dlt)]
      in
      let s = {
        file_format   = OpamVersion.current;
        file_name     = OpamFilename.to_string filename;
        file_contents = [
          OpamFormat.make_variable (s_opam_version,
                    OpamFormat.make_string (OpamVersion.to_string t.opam_version));
          OpamFormat.make_variable (s_repositories,
                    OpamFormat.make_list
                      (OpamRepositoryName.to_string @> OpamFormat.make_string)
                      t.repositories);
          OpamFormat.make_variable (s_jobs , OpamFormat.make_int t.jobs);
          OpamFormat.make_variable (s_dl_jobs , OpamFormat.make_int t.dl_jobs);
          OpamFormat.make_variable (s_switch, OpamFormat.make_string (OpamSwitch.to_string t.switch))
        ] @ criteria
          @ solver
          @ download_tool
      } in
      Syntax.to_string s
  end

  module OPAM = struct

    let internal = "opam"

    type t = {
      opam_version: opam_version;
      name       : OpamPackage.Name.t option;
      version    : OpamPackage.Version.t option;
      maintainer : string list;
      substs     : basename list;
      build_env  : (string * string * string) list;
      build      : command list;
      install    : command list;
      remove     : command list;
      depends    : ext_formula;
      depopts    : ext_formula;
      conflicts  : formula;
      features   : (OpamVariable.t * string * filter) list;
      libraries  : (string * filter option) list;
      syntax     : (string * filter option) list;
      patches    : (basename * filter option) list;
      ocaml_version: compiler_constraint option;
      os         : (bool * string) generic_formula;
      available  : filter;
      homepage   : string list;
      author     : string list;
      license    : string list;
      doc        : string list;
      tags       : string list;
      build_test : command list;
      build_doc  : command list;
      depexts    : tags option;
      messages   : (string * filter option) list;
      bug_reports : string list;
      post_messages: (string * filter option) list;
      flags      : package_flag list;
      dev_repo   : pin_option option;
    }

    let empty = {
      opam_version = OpamVersion.current_nopatch;
      name       = None;
      version    = None;
      maintainer = [];
      substs     = [];
      build_env  = [];
      build      = [];
      install    = [];
      remove     = [];
      depends    = OpamFormula.Empty;
      depopts    = OpamFormula.Empty;
      conflicts  = OpamFormula.Empty;
      features   = [];
      libraries  = [];
      syntax     = [];
      patches    = [];
      ocaml_version = None;
      os         = Empty;
      available  = FBool true;
      homepage   = [];
      author     = [];
      license    = [];
      doc        = [];
      tags       = [];
      build_test = [];
      build_doc  = [];
      depexts    = None;
      messages   = [];
      post_messages = [];
      bug_reports = [];
      flags      = [];
      dev_repo   = None;
    }

    let create nv =
      let name = Some (OpamPackage.name nv) in
      let version = Some (OpamPackage.version nv) in
      { empty with name; version }

    let s_opam_version = "opam-version"
    let s_version     = "version"
    let s_name        = "name"
    let s_maintainer  = "maintainer"
    let s_substs      = "substs"
    let s_build       = "build"
    let s_install     = "install"
    let s_build_env   = "build-env"
    let s_remove      = "remove"
    let s_depends     = "depends"
    let s_depopts     = "depopts"
    let s_conflicts   = "conflicts"
    let s_features    = "features"
    let s_libraries   = "libraries"
    let s_syntax      = "syntax"
    let s_ocaml_version = "ocaml-version"
    let s_patches     = "patches"
    let s_configure_style = "configure-style"
    let s_os          = "os"
    let s_available   = "available"
    let s_homepage    = "homepage"
    let s_author      = "author"
    let s_authors     = "authors"
    let s_license     = "license"
    let s_doc         = "doc"
    let s_tags        = "tags"
    let s_build_test  = "build-test"
    let s_build_doc   = "build-doc"
    let s_depexts     = "depexts"
    let s_messages    = "messages"
    let s_post_messages = "post-messages"
    let s_bug_reports = "bug-reports"
    let s_flags       = "flags"
    let s_dev_repo    = "dev-repo"

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

    let to_1_0_fields k v =
      if List.mem k opam_1_1_fields || List.mem k opam_1_2_fields then
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
      opam_1_0_fields @ opam_1_1_fields @ opam_1_2_fields

    let check name = function
      | None    ->
        OpamConsole.error_and_exit "Invalid OPAM file (missing field %S)" name
      | Some n -> n

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

    let name t = check "name" t.name
    let name_opt t = t.name
    let version t = check "version" t.version
    let version_opt t = t.version
    let maintainer t = t.maintainer
    let substs t = t.substs
    let build t = t.build
    let install t = t.install
    let remove t = t.remove
    let depends t = t.depends
    let depopts t = t.depopts
    let conflicts t = t.conflicts
    let features t = t.features
    let libraries t = t.libraries
    let syntax t = t.syntax
    let ocaml_version t = t.ocaml_version
    let build_env t = t.build_env
    let patches t = t.patches
    let os t = t.os
    let available t = t.available
    let homepage t = t.homepage
    let author t = t.author
    let license t = t.license
    let doc t = t.doc
    let tags t = t.tags
    let build_doc t = t.build_doc
    let build_test t = t.build_test
    let depexts t = t.depexts
    let messages t = t.messages
    let post_messages t = t.post_messages
    let opam_version t = t.opam_version
    let bug_reports t = t.bug_reports
    let flags t = t.flags
    let has_flag f t =
      List.mem f t.flags ||
      (* Allow in tags for compatibility *)
      List.mem ("flags:"^OpamFormat.(parse_ident (make_flag f))) t.tags
    let dev_repo t = t.dev_repo

    let with_opam_version t opam_version = { t with opam_version }
    let with_name t name = { t with name = Some name }
    let with_name_opt t name = { t with name }
    let with_version t version = { t with version = Some version }
    let with_version_opt t version = { t with version }
    let with_depends t depends = { t with depends }
    let with_depopts t depopts = { t with depopts }
    let with_conflicts t conflicts = {t with conflicts }
    let with_features t features = {t with features }
    let with_build t build = { t with build }
    let with_install t install = { t with install }
    let with_remove t remove = { t with remove }
    let with_libraries t libraries = { t with libraries }
    let with_syntax t syntax = { t with syntax }
    let with_substs t substs = { t with substs }
    let with_ocaml_version t ocaml_version = { t with ocaml_version }
    let with_os t os = { t with os }
    let with_available t available = { t with available }
    let with_maintainer t maintainer = { t with maintainer }
    let with_patches t patches = { t with patches }
    let with_bug_reports t bug_reports = { t with bug_reports }
    let with_depexts t depexts = { t with depexts }
    let with_messages t messages = { t with messages }
    let with_post_messages t post_messages = { t with post_messages }
    let with_flags t flags = { t with flags }
    let with_dev_repo t dev_repo = {t with dev_repo }

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
          @ formula t.depends       s_depends       OpamFormat.make_ext_formula
          @ formula t.depopts       s_depopts       OpamFormat.make_opt_formula
          @ option  t.depexts       s_depexts       OpamFormat.make_tags
          @ formula t.conflicts     s_conflicts     OpamFormat.make_formula
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
             OpamFormat.parse_ext_formula v |>
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
          OpamFormat.parse_opt_formula value |>
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
      let conflicts = assoc_default OpamFormula.Empty s s_conflicts
          OpamFormat.parse_formula in
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
          | Http u -> Git u
          | _ ->
            OpamFormat.bad_format ~pos:(OpamFormat.value_pos v)
              "Unrecognised version-control address")
      in
      { opam_version; name; version; maintainer; substs; build; install; remove;
        depends; depopts; conflicts; features; libraries; syntax;
        patches; ocaml_version; os; available; build_env;
        homepage; author; license; doc; tags;
        build_test; build_doc; depexts; messages; post_messages;
        bug_reports; flags; dev_repo
      }

    let of_channel filename ic =
      let nv = OpamPackage.of_filename filename in
      let f = Syntax.of_channel filename ic in
      let permissive = Syntax.check f valid_fields in
      of_syntax ~permissive f nv

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
                       CString "--prefix=%{prefix}%", None;], None;
                      [CIdent "make", None], None];
        install    = [[CIdent "make", None; CString "install", None], None];
        remove     = [[CString "ocamlfind", None; CString "remove", None;
                       CString (OpamPackage.Name.to_string (OpamPackage.name nv)), None],
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
            atoms @@ filter_deps ~build:true ~test:flag ~doc:flag f
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
             [s_maintainer, t.maintainer; s_homepage, t.homepage;
              s_author, t.author; s_license, t.license; s_doc, t.doc;
              s_tags, t.tags; s_bug_reports, t.bug_reports]
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
      ]
      in
      OpamStd.List.filter_map (fun x -> x) warnings

    let validate_gen reader filename =
      let warnings, t =
        try
          let f, name, version = reader filename in
          let invalid_fields =
            OpamFormat.invalid_fields f.file_contents valid_fields
          in
          let warnings =
            List.map (fun f -> 3, `Error, Printf.sprintf "Invalid field: %s" f)
              invalid_fields
          in
          let t, warnings =
            try
              Some (of_syntax ~permissive:false ~conservative:true f None),
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
            if t = None then warnings else
            try
              ignore (check_name name f.file_contents);
              warnings
            with OpamFormat.Bad_format (_,_,msg) ->
              [ 4, `Warning,
                Printf.sprintf "%s, the directory name or pinning implied %s"
                  msg
                  OpamStd.Option.Op.((name >>| OpamPackage.Name.to_string) +! "" )
              ]
          in
          let warnings =
            if t = None then warnings else
            try
              ignore (check_version version f.file_contents);
              warnings
            with OpamFormat.Bad_format (_,_,msg) ->
              [ 5, `Warning,
                Printf.sprintf "%s, the directory name or pinning implied %s"
                  msg
                  OpamStd.Option.Op.((version >>| OpamPackage.Version.to_string) +! "" )
              ]
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
        let nv = OpamPackage.of_filename filename in
        let name = OpamStd.Option.map OpamPackage.name nv in
        let version = OpamStd.Option.map OpamPackage.version nv in
        let f =
          let ic = OpamFilename.open_in filename in
          try
            let f = Syntax.of_channel filename ic in
            close_in ic; f
          with e -> close_in ic; raise e
        in
        f, name, version
      in
      validate_gen reader filename

    let validate_string filename string =
      let reader filename =
        let nv = OpamPackage.of_filename filename in
        let name = OpamStd.Option.map OpamPackage.name nv in
        let version = OpamStd.Option.map OpamPackage.version nv in
        Syntax.of_string filename string, name, version
      in
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

    let of_channel filename ic =
      let s = Syntax.of_channel filename ic in
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

  end

  module Dot_config = struct

    let internal = ".config"

    let s str = S str
    let b bool = B bool

    type t = (variable * variable_contents) list

    let create variables = variables

    let empty = []

    let of_channel filename ic =
      let file = Syntax.of_channel filename ic in
      let parse_value = OpamFormat.parse_or [
          "string", (OpamFormat.parse_string @> s);
          "bool"  , (OpamFormat.parse_bool   @> b);
        ] in
      let parse_variables items =
        let l = OpamFormat.variables items in
        List.rev_map (fun (k,v) -> OpamVariable.of_string k, parse_value v) l in
      let variables = parse_variables file.file_contents in
      variables

    let to_string filename t =
      let open OpamFormat in
      let of_value = function
        | B b -> make_bool b
        | S s -> make_string s in
      let of_variables l =
        List.rev_map (fun (k,v) -> make_variable (OpamVariable.to_string k, of_value v)) l in
      Syntax.to_string {
        file_format   = OpamVersion.current;
        file_name     = OpamFilename.to_string filename;
        file_contents = of_variables t
      }

    let variables t = List.rev_map fst t

    let bindings t = t

    let variable t s =
      try Some (List.assoc s t)
      with Not_found -> None

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

    let of_channel filename ic =
      let file = Syntax.of_channel filename ic in
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
        try match URL.url_and_kind ~src ~archive ~http ~git ~darcs ~hg ~local
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
          s s_packages OpamFormat.parse_formula in
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
            make_variable (s_packages, make_formula s.packages);
            make_variable (s_env, make_list make_env_variable s.env);
            make_variable (s_tags, make_string_list s.tags);
          ] @ (
            if not s.preinstalled then []
            else [ make_variable (s_preinstalled, make_bool s.preinstalled) ])
      } in
      Syntax.to_string s

  end

  module Comp_descr = Descr

  module Subst = struct

    let internal = "subst"

    type t = string

    let empty = ""

    let of_channel _ ic =
      OpamSystem.string_of_channel ic

    let to_string _ t = t

  end

  module Repo = struct

    let internal = "repo"

    type t = {
      opam_version : OpamVersion.t;
      browse       : string option;
      upstream     : string option;
      redirect     : (string * filter option) list;
    }

    let version_of_maybe_string vs = OpamVersion.of_string begin
      match vs with
      | None   -> "0.7.5"
      | Some v -> v
    end

    let create ?browse ?upstream ?opam_version ?(redirect=[]) () = {
      opam_version = version_of_maybe_string opam_version;
      browse; upstream; redirect;
    }

    let empty = create ()

    let s_opam_version = "opam-version"
    let s_browse       = "browse"
    let s_upstream     = "upstream"
    let s_redirect     = "redirect"

    let valid_fields = [
      s_opam_version;
      s_browse;
      s_upstream;
      s_redirect;
    ]

    let of_channel filename ic =
      let s = Syntax.of_channel filename ic in
      let permissive =
        Syntax.check ~allow_major:true ~versioned:false s valid_fields in
      let get f =
        try OpamFormat.assoc_option s.file_contents f
              OpamFormat.parse_string
        with OpamFormat.Bad_format _ when permissive -> None
      in
      let opam_version = version_of_maybe_string (get s_opam_version) in
      let browse   = get s_browse in
      let upstream = get s_upstream in
      let redirect =
        try OpamFormat.assoc_list s.file_contents s_redirect
              (OpamFormat.parse_list
                 (OpamFormat.parse_option OpamFormat.parse_string OpamFormat.parse_filter))
        with OpamFormat.Bad_format _ when permissive -> []
      in
      { opam_version; browse; upstream; redirect }

    let to_string filename t =
      let opam_version = OpamVersion.to_string t.opam_version in
      let s = {
        file_format   = t.opam_version;
        file_name     = OpamFilename.to_string filename;
        file_contents =
          (OpamFormat.make_variable (s_opam_version, OpamFormat.make_string opam_version))
          :: (
            match t.upstream with
            | None -> []
            | Some url -> [OpamFormat.make_variable (s_upstream , OpamFormat.make_string url)]
          ) @ (
            match t.browse with
            | None -> []
            | Some url -> [OpamFormat.make_variable (s_browse   , OpamFormat.make_string url)]
          ) @ (
            match t.redirect with
            | [] -> []
            | l  ->
              let value =
                OpamFormat.make_list
                  (OpamFormat.make_option OpamFormat.make_string OpamFormat.make_filter)
                  l in
              [OpamFormat.make_variable(s_redirect, value)]
          );
      } in
      Syntax.to_string s

    let opam_version t = t.opam_version
    let browse t = t.browse
    let upstream t = t.upstream
    let redirect t = t.redirect

  end

end

module type F = sig
  val internal : string
  type t
  val empty : t
  val of_channel : filename -> in_channel  -> t
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

  let read_from_channel ic =
    try F.of_channel dummy_file ic with
    | OpamFormat.Bad_format _ as e ->
      OpamConsole.error "%s" (OpamFormat.string_of_bad_format e);
      if OpamFormatConfig.(!r.strict) then
        OpamConsole.error_and_exit "Strict mode: aborting"
      else raise e

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
  val read_from_channel: in_channel -> t
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

module Filenames = struct
  include Filenames
  include Make(Filenames)
end

module Prefix = struct
  include Prefix
  include Make(Prefix)
end
