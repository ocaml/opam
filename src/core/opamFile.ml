(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2013 OCamlPro                                        *)
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
open OpamMisc.OP

module Lines = struct

  (* Lines of space separated words *)
  type t = string list list

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
      let buf = String.create (len + n) in
      let rec aux i = function
        | ofs1::(ofs2::_ as r) ->
          String.blit str ofs1 buf (ofs1+i) (ofs2-ofs1);
          buf.[ofs2+i] <- '\\';
          aux (i+1) r
        | [ofs] ->
          String.blit str ofs buf (ofs+i) (len-ofs);
          buf
        | [] -> assert false
      in
      aux 0 (0::escapes)

  let of_channel ic =
    OpamLineLexer.main (Lexing.from_channel ic)

  let to_string (lines: t) =
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

  let to_string ignore (t: t) =
    OpamFormat.string_of_file ~simplify:true ~indent:true ~ignore t

  let s_opam_version = "opam-version"

  let check ?(versioned=true) =
    let not_already_warned = ref true in
    fun f fields ->
      if List.mem s_opam_version fields then
        begin match OpamFormat.assoc_option f.file_contents s_opam_version
            (OpamFormat.parse_string @> OpamVersion.of_string) with
            | Some opam_version ->
              if OpamVersion.compare opam_version OpamVersion.current > 0 then (
                OpamGlobals.error
                  "Your version of OPAM (%s) is not recent enough to read \
                   %s. Upgrade OPAM to a more recent version (at least %s) \
                   to read this file correctly."
                  (OpamVersion.to_string OpamVersion.current)
                  (OpamMisc.prettify_path f.file_name)
                  (OpamVersion.to_string opam_version);
                OpamFormat.bad_format "opam-version"
              )
            | None ->
              if versioned then (
                OpamGlobals.error
                  "%s is missing the opam-version field: syntax check failed."
                  (OpamMisc.prettify_path f.file_name);
                OpamFormat.bad_format "opam-version"
              )
        end;
      if not (OpamFormat.is_valid f.file_contents fields) then
        let invalids = OpamFormat.invalid_fields f.file_contents fields in
        let too_many, invalids = List.partition (fun x -> List.mem x fields) invalids in
        if too_many <> [] then
          OpamGlobals.warning "duplicated fields in %s: %s"
            f.file_name
            (OpamMisc.string_of_list (fun x -> x) too_many);
        if !OpamGlobals.strict then (
          if invalids <> [] then
            (let are,s = match invalids with [_] -> "is an","" | _ -> "are","s" in
             OpamGlobals.error "%s %s invalid field name%s in %s. Valid fields: %s\n\
                                Either there is an error in the package, or your \
                                OPAM is not up-to-date."
               (OpamMisc.string_of_list (fun x -> x) invalids)
               are s f.file_name
               (OpamMisc.string_of_list (fun x -> x) fields));
          OpamGlobals.exit 5
        ) else if !not_already_warned then (
          not_already_warned := false;
          let is_, s_ =
            if List.length invalids <= 1 then "is an", "" else "are", "s" in
          if invalids <> [] then
            OpamGlobals.warning "%s %s unknown field%s in %s: is your OPAM up-to-date ?"
              (OpamMisc.pretty_list invalids)
              is_ s_
              f.file_name
        )

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

module X = struct

  module Prefix = struct

    let internal = "prefix"

    type t = string name_map

    let empty = OpamPackage.Name.Map.empty

    let of_channel _ ic =
      let lines = Lines.of_channel ic in
      List.fold_left (fun map -> function
        | []          -> map
        | [nv;prefix] -> OpamPackage.Name.Map.add (OpamPackage.Name.of_string nv)
                           prefix map
        | s ->
          OpamGlobals.error_and_exit
            "%S is not a valid prefix line"
            (String.concat " " s)
      ) OpamPackage.Name.Map.empty lines

    let to_string _ s =
      let lines =
        OpamPackage.Name.Map.fold (fun nv prefix l ->
          [OpamPackage.Name.to_string nv; prefix] :: l
        ) s [] in
      Lines.to_string lines

  end

  module Filenames = struct

    let internal = "filenames"

    type t = filename_set

    let empty = OpamFilename.Set.empty

    let of_channel _ ic =
      let lines = Lines.of_channel ic in
      let lines = OpamMisc.filter_map (function
          | []  -> None
          | [f] -> Some (OpamFilename.of_string f)
          | s   -> OpamGlobals.error_and_exit "%S is not a valid filename"
                     (String.concat " " s)
        ) lines in
      OpamFilename.Set.of_list lines

    let to_string _ s =
      let lines =
        List.rev_map
          (fun f -> [OpamFilename.to_string f])
          (OpamFilename.Set.elements s) in
      Lines.to_string lines

  end

  module File_attributes = struct

    let internal = "file_attributes"

    type t = file_attribute_set

    let empty = OpamFilename.Attribute.Set.empty

    let of_channel _ ic =
      let lines = Lines.of_channel ic in
      let rs = OpamMisc.filter_map (function
          | [] -> None
          | [s] -> (* backwards-compat *)
            Some (OpamFilename.Attribute.of_string_list (OpamMisc.split s ' '))
          | l  ->
            Some (OpamFilename.Attribute.of_string_list l)
        ) lines in
      OpamFilename.Attribute.Set.of_list rs

    let to_string _ t =
      let lines =
        List.rev_map
          (fun r -> OpamFilename.Attribute.to_string_list r)
          (OpamFilename.Attribute.Set.elements t) in
      Lines.to_string lines

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
          Some (url, OpamMisc.Option.default kind kind_opt)
        with Invalid_argument s -> OpamFormat.bad_format "%s" s

    let of_channel filename ic =
      let s = Syntax.of_channel filename ic in
      Syntax.check s valid_fields;
      let get f = OpamFormat.assoc_option s.file_contents f
          (OpamFormat.parse_string @> address_of_string) in
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
      Syntax.to_string [] s

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

    let of_channel f ic =
      let lines = Lines.of_channel ic in
      let state = function
        | "root" -> `Root
        | "noroot" | "installed" -> `Installed
        | "uninstalled" -> `Uninstalled
        | s ->
          OpamGlobals.error_and_exit "Invalid installation status (col. 3) in %s: %S"
            (OpamFilename.to_string f) s
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
            OpamGlobals.error_and_exit "Invalid line in %s: %S"
              (OpamFilename.to_string f)
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
          OpamGlobals.error_and_exit "Multiple versions installed for package %s: %s"
            (OpamPackage.Name.to_string n) (OpamPackage.Version.Set.to_string vs)
      ) map

    let of_channel name ic =
      let lines = Lines.of_channel ic in
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
              OpamGlobals.error "At %s:%d: skipped invalid line %S"
                (OpamFilename.prettify name) i (String.concat " " s);
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

  module Repo_index (A : OpamMisc.ABSTRACT) = struct

    let internal = "repo-index"

    type t = (repository_name * string option) A.Map.t

    let empty = A.Map.empty

    let of_channel _ ic =
      let lines = Lines.of_channel ic in
      List.fold_left (fun map -> function
          | [] | [_]                 -> map
          | a_s :: repos_s :: prefix ->
            let a = A.of_string a_s in
            if A.Map.mem a map then
              OpamGlobals.error_and_exit "multiple lines for %s" a_s
            else
              let repo_name = OpamRepositoryName.of_string repos_s in
              let prefix = match prefix with
                | []  -> None
                | [p] -> Some p
                | _   -> OpamGlobals.error_and_exit "Too many prefixes" in
              A.Map.add a (repo_name, prefix) map
        ) A.Map.empty lines

    let to_string _ map =
      let lines = A.Map.fold (fun nv (repo_name, prefix) lines ->
          let repo_s = OpamRepositoryName.to_string repo_name in
          let prefix_s = match prefix with
            | None   -> []
            | Some p -> [p] in
          (A.to_string nv :: repo_s :: prefix_s) :: lines
        ) map [] in
      Lines.to_string (List.rev lines)

  end

  module Package_index = Repo_index(OpamPackage)

  module Compiler_index = Repo_index(OpamCompiler)

  module Pinned = struct

    let internal = "pinned"

    type t = pin_option OpamPackage.Name.Map.t

    let empty = OpamPackage.Name.Map.empty

    let of_channel _ ic =
      let lines = Lines.of_channel ic in
      let add name_s pin map =
        let name = OpamPackage.Name.of_string name_s in
        if OpamPackage.Name.Map.mem name map then
          OpamGlobals.error_and_exit "multiple lines for package %s" name_s
        else
          OpamPackage.Name.Map.add name pin map in
      List.fold_left (fun map -> function
        | []           -> map
        | [name_s; x]  -> add name_s (pin_option_of_string x) map
        | [name_s;k;x] ->
          let kind = Some (pin_kind_of_string k) in
          add name_s (pin_option_of_string ?kind x) map
        | _     -> OpamGlobals.error_and_exit "too many pinning options"
      ) OpamPackage.Name.Map.empty lines

    let to_string _ map =
      let lines = OpamPackage.Name.Map.fold (fun name pin lines ->
          let kind = kind_of_pin_option pin in
          let l = [
            OpamPackage.Name.to_string name;
            string_of_pin_kind kind;
            string_of_pin_option pin
          ] in
          l :: lines
        ) map [] in
      Lines.to_string (List.rev lines)

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
        with None   -> OpamPath.Repository.create (OpamPath.root ()) repo_name
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
      Syntax.to_string [] s

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
        match OpamMisc.cut_at str '\n' with
        | None       -> str, ""
        | Some (h,t) -> h, t in
      head, tail

    let to_string _ = full

  end

  module Aliases = struct

    let internal = "aliases"

    type t = compiler switch_map

    let empty = OpamSwitch.Map.empty

    let to_string _ t =
      let l =
        OpamSwitch.Map.fold (fun switch compiler lines ->
          [OpamSwitch.to_string switch; OpamCompiler.to_string compiler] :: lines
        ) t [] in
      Lines.to_string l

    let of_channel _ ic =
      let l = Lines.of_channel ic in
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
      dl_jobs       : int;
    }

    let with_repositories t repositories = { t with repositories }
    let with_switch t switch = { t with switch }
    let with_current_opam_version t = { t with opam_version = OpamVersion.current }

    let opam_version t = t.opam_version
    let repositories t = t.repositories
    let switch t = t.switch
    let jobs t = t.jobs
    let dl_jobs t = t.dl_jobs

    let create switch repositories jobs dl_jobs =
      { opam_version = OpamVersion.current;
        repositories ; switch ; jobs ; dl_jobs}

    let empty = {
      opam_version = OpamVersion.current;
      repositories = [];
      switch = OpamSwitch.of_string "<empty>";
      jobs = OpamGlobals.default_jobs;
      dl_jobs = OpamGlobals.default_dl_jobs;
    }

    let s_opam_version = "opam-version"
    let s_repositories = "repositories"
    let s_switch = "switch"
    let s_switch1 = "alias"
    let s_switch2 = "ocaml-version"

    let s_jobs = "jobs"
    let s_dl_jobs = "download-jobs"

    let s_cores = "cores"

    let s_system_version1 = "system_ocaml-version"
    let s_system_version2 = "system-ocaml-version"

    let valid_fields = [
      s_opam_version;
      s_repositories;
      s_switch;
      s_jobs;
      s_dl_jobs;

      (* this fields are no longer useful, but we keep it for backward
         compatibility *)
      s_switch1;
      s_switch2;
      s_system_version1;
      s_system_version2;
      s_cores;
    ]

    let of_channel filename ic =
      let s = Syntax.of_channel filename ic in
      Syntax.check s valid_fields;
      let opam_version = OpamFormat.assoc s.file_contents s_opam_version
          (OpamFormat.parse_string @> OpamVersion.of_string) in
      let repositories =
        OpamFormat.assoc_list s.file_contents s_repositories
          (OpamFormat.parse_list
             (OpamFormat.parse_string @> OpamRepositoryName.of_string)) in
      let mk_switch str =
        OpamFormat.assoc_option s.file_contents str
          (OpamFormat.parse_string @> OpamSwitch.of_string) in
      let switch  = mk_switch s_switch in
      let switch1 = mk_switch s_switch1 in
      let switch2 = mk_switch s_switch2 in
      let switch =
        match switch, switch1, switch2 with
        | Some v, _     , _
        | _     , Some v, _
        | _     , _     , Some v -> v
        | None  , None  , None   -> OpamGlobals.error_and_exit
                                      "No current switch defined." in

      let jobs =
        let mk str = OpamFormat.assoc_option s.file_contents str OpamFormat.parse_int in
        match mk s_jobs, mk s_cores with
        | Some i, _      -> i
        | _     , Some i -> i
        | _              -> 1 in

      let dl_jobs =
        match OpamFormat.assoc_option s.file_contents s_dl_jobs
                OpamFormat.parse_int with
        | Some i -> i
        | None -> OpamGlobals.default_dl_jobs in
      { opam_version; repositories; switch; jobs; dl_jobs; }

    let to_string filename t =
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
        ]
      } in
      Syntax.to_string [] s
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
      remove     : command list;
      depends    : formula;
      depopts    : OpamPackage.Name.t list;
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
    }

    let empty = {
      opam_version = OpamVersion.current;
      name       = None;
      version    = None;
      maintainer = [];
      substs     = [];
      build_env  = [];
      build      = [];
      remove     = [];
      depends    = OpamFormula.Empty;
      depopts    = [];
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
      s_features;
    ]

    let to_1_0_fields k v =
      if List.mem k opam_1_1_fields then
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
      | None    -> OpamGlobals.error_and_exit "Invalid OPAM file (%s)" name
      | Some n -> n

    let name t = check "name" t.name
    let version t = check "version" t.version
    let version_opt t = t.version
    let maintainer t = t.maintainer
    let substs t = t.substs
    let build t = t.build
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

    let with_name t name = { t with name = Some name }
    let with_version t version = { t with version = Some version }
    let with_depends t depends = { t with depends }
    let with_depopts t depopts = { t with depopts }
    let with_build t build = { t with build }
    let with_remove t remove = { t with remove }
    let with_libraries t libraries = { t with libraries }
    let with_syntax t syntax = { t with syntax }
    let with_substs t substs = { t with substs }
    let with_ocaml_version t ocaml_version = { t with ocaml_version }
    let with_maintainer t maintainer = { t with maintainer }
    let with_patches t patches = { t with patches }
    let with_bug_reports t bug_reports = { t with bug_reports }
    let with_depexts t depexts = { t with depexts }

    let to_string filename t =
      let make_file =
        OpamFormat.make_option (OpamFilename.Base.to_string @> OpamFormat.make_string)
          OpamFormat.make_filter in
      let name_and_version = match OpamPackage.of_filename filename with
        | Some _ -> []
        | None ->
          let name n = OpamFormat.make_string (OpamPackage.Name.to_string n) in
          let version v = OpamFormat.make_string (OpamPackage.Version.to_string v) in
          [ OpamFormat.make_variable (s_name, name (check "name" t.name));
            OpamFormat.make_variable (s_version, version (check "version" t.version)) ] in
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
      let s = {
        file_format   = t.opam_version;
        file_name     = OpamFilename.to_string filename;
        file_contents = [
          OpamFormat.make_variable (s_opam_version,
                    (OpamVersion.to_string @> OpamFormat.make_string) t.opam_version);
        ] @ list    t.maintainer    s_maintainer    OpamFormat.make_string_list
          @ list    t.author        s_author        OpamFormat.make_string_list
          @ name_and_version
          @ list    t.homepage      s_homepage      OpamFormat.make_string_list
          @ list    t.bug_reports   s_bug_reports   OpamFormat.make_string_list

          @ list    t.license       s_license       OpamFormat.make_string_list
          @ list    t.doc           s_doc           OpamFormat.make_string_list
          @ list    t.tags          s_tags          OpamFormat.make_string_list
          @ listm   t.substs s_substs
              (OpamFilename.Base.to_string @> OpamFormat.make_string)
          @ listm   t.build_env     s_build_env     OpamFormat.make_env_variable
          @ listm   t.build         s_build         OpamFormat.make_command
          @ listm   t.build_test    s_build_test    OpamFormat.make_command
          @ listm   t.build_doc     s_build_doc     OpamFormat.make_command
          @ listm   t.remove        s_remove        OpamFormat.make_command
          @ formula t.depends       s_depends       OpamFormat.make_formula
          @ listm   t.depopts       s_depopts
            (OpamPackage.Name.to_string @> OpamFormat.make_string)
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
      let s = if !OpamGlobals.compat_mode_1_0 then to_1_0 s else s in
      Syntax.to_string [s_os; s_ocaml_version; s_available] s

    let of_channel filename ic =
      let nv = OpamPackage.of_filename filename in
      let f = Syntax.of_channel filename ic in
      Syntax.check f valid_fields;
      let s = f.file_contents in
      let opam_version = OpamFormat.assoc s s_opam_version
          (OpamFormat.parse_string @> OpamVersion.of_string) in
      let name_f = OpamFormat.assoc_option s s_name
          (OpamFormat.parse_string @> OpamPackage.Name.of_string) in
      let name = match name_f, nv with
        | None  , None    -> None
        | Some n, None    -> Some n
        | None  , Some nv -> Some (OpamPackage.name nv)
        | Some n, Some nv ->
          if OpamPackage.name nv <> n then
            (OpamGlobals.error
               "Package %s has inconsistent 'name: %S' field."
               (OpamPackage.to_string nv)
               (OpamPackage.Name.to_string n);
             OpamSystem.internal_error "inconsistent name")
          else Some n in
      let version_f = OpamFormat.assoc_option s s_version
          (OpamFormat.parse_string @> OpamPackage.Version.of_string) in
      let version = match version_f, nv with
        | None  , None    -> None
        | Some v, None    -> Some v
        | None  , Some nv -> Some (OpamPackage.version nv)
        | Some v, Some nv ->
          if OpamPackage.version nv <> v then
            (OpamGlobals.error
               "Package %s has inconsistent 'version: %S' field."
               (OpamPackage.to_string nv)
               (OpamPackage.Version.to_string v);
             OpamSystem.internal_error "inconsistent version")
          else Some v in
      let maintainer =
        OpamFormat.assoc_list s s_maintainer OpamFormat.parse_string_list in
      let substs =
        OpamFormat.assoc_list s s_substs
          (OpamFormat.parse_list (OpamFormat.parse_string @>
                                    OpamFilename.Base.of_string)) in
      let build_env =
        OpamFormat.assoc_list s s_build_env
          (OpamFormat.parse_list OpamFormat.parse_env_variable) in
      let build = OpamFormat.assoc_list s s_build OpamFormat.parse_commands in
      let remove = OpamFormat.assoc_list s s_remove OpamFormat.parse_commands in
      let depends = OpamFormat.assoc_default OpamFormula.Empty s s_depends
          OpamFormat.parse_formula in
      let depopts =
        OpamFormat.assoc_default [] s s_depopts @@
        if OpamVersion.compare opam_version (OpamVersion.of_string "1.2") < 0
        then
          fun s ->
            let f = OpamFormat.parse_opt_formula s in
            try List.map (function (name,None) -> name
                                 | (_, Some _) -> failwith "version constraint")
                (OpamFormula.to_disjunction f)
            with Failure _ | Invalid_argument _ ->
              OpamGlobals.warning
                "Ignored deprecated use of formula or version constraint in \
                 optional dependency at\n  %s."
                (string_of_pos (OpamFormat.value_pos s));
              List.map fst (OpamFormula.atoms f)
        else
          OpamFormat.parse_string_list
          @> List.map OpamPackage.Name.of_string in
      let conflicts = OpamFormat.assoc_default OpamFormula.Empty s s_conflicts
          OpamFormat.parse_formula in
      let features = OpamFormat.assoc_default [] s s_features
          OpamFormat.parse_features in
      let libraries = OpamFormat.assoc_list s s_libraries OpamFormat.parse_libraries in
      let syntax = OpamFormat.assoc_list s s_syntax OpamFormat.parse_libraries in
      let ocaml_version = OpamFormat.assoc_option s s_ocaml_version
          OpamFormat.parse_compiler_constraint in
      let os = OpamFormat.assoc_default OpamFormula.Empty s s_os
          OpamFormat.parse_os_constraint in
      let available = OpamFormat.assoc_default (FBool true) s s_available
          (OpamFormat.parse_list (fun x -> x) @> OpamFormat.parse_filter) in
      let parse_file = OpamFormat.parse_option
          (OpamFormat.parse_string @> OpamFilename.Base.of_string)
          OpamFormat.parse_filter in
      let patches = OpamFormat.assoc_list s s_patches
          (OpamFormat.parse_list parse_file) in
      let homepage = OpamFormat.assoc_list s s_homepage OpamFormat.parse_string_list in
      let author =
        let x = OpamFormat.assoc_list s s_authors OpamFormat.parse_string_list in
        let y = OpamFormat.assoc_list s s_author  OpamFormat.parse_string_list in
        x @ y in
      let license = OpamFormat.assoc_list s s_license OpamFormat.parse_string_list in
      let doc = OpamFormat.assoc_list s s_doc OpamFormat.parse_string_list in
      let tags = OpamFormat.assoc_list s s_tags OpamFormat.parse_string_list in
      let build_test = OpamFormat.assoc_list s s_build_test OpamFormat.parse_commands in
      let build_doc = OpamFormat.assoc_list s s_build_doc OpamFormat.parse_commands in
      let depexts = OpamFormat.assoc_option s s_depexts OpamFormat.parse_tags in
      let messages = OpamFormat.assoc_list s s_messages OpamFormat.parse_messages in
      let bug_reports =
        OpamFormat.assoc_list s s_bug_reports OpamFormat.parse_string_list in
      let post_messages =
        OpamFormat.assoc_list s s_post_messages OpamFormat.parse_messages in
      let flags = OpamFormat.assoc_list s s_flags
          OpamFormat.(parse_list parse_flag) in
      { opam_version; name; version; maintainer; substs; build; remove;
        depends; depopts; conflicts; features; libraries; syntax;
        patches; ocaml_version; os; available; build_env;
        homepage; author; license; doc; tags;
        build_test; build_doc; depexts; messages; post_messages;
        bug_reports; flags
      }
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
    let man t =
      List.map (fun (src, dst) ->
          src,
          match dst with
          | None ->
            let base = Filename.basename (OpamFilename.Base.to_string src.c) in
            Some (OpamFilename.Base.of_string (Filename.concat "man3" base))
          | _    -> dst
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

    let valid_fields = [
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
          make_variable (s_misc    , mk_misc t.misc);
        ]
      } in
      Syntax.to_string [] s

    let of_channel filename ic =
      let s = Syntax.of_channel filename ic in
      Syntax.check s valid_fields;
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
      { lib; bin; sbin; misc; toplevel; stublibs; share; share_root; etc; doc; man }

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
      Syntax.to_string [] {
        file_format   = OpamVersion.current;
        file_name     = OpamFilename.to_string filename;
        file_contents = of_variables t
      }

    let variables t = List.rev_map fst t

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

    let of_channel filename ic =
      let file = Syntax.of_channel filename ic in
      Syntax.check file valid_fields;
      let s = file.file_contents in
      let opam_version = OpamFormat.assoc s s_opam_version
          (OpamFormat.parse_string @> OpamVersion.of_string) in
      let name_d, version_d = match OpamCompiler.of_filename filename with
        | None   -> OpamSystem.internal_error
                      "%s is not a valid compiler description file."
                      (OpamFilename.to_string filename)
        | Some c -> c, OpamCompiler.version c in
      let name =
        OpamFormat.assoc_default name_d s s_name
          (OpamFormat.parse_string @> OpamCompiler.of_string) in
      if name_d <> name then
        OpamGlobals.warning "The file %s contains a bad 'name' field: %s instead of %s"
          (OpamFilename.to_string filename)
          (OpamCompiler.to_string name)
          (OpamCompiler.to_string name_d);
      let version =
        OpamFormat.assoc_default version_d s s_version
          (OpamFormat.parse_string @> OpamCompiler.Version.of_string) in
      if name <> OpamCompiler.system && version_d <> version then
        OpamGlobals.warning
          "The file %s contains a bad 'version' field: %s instead of %s"
          (OpamFilename.to_string filename)
          (OpamCompiler.Version.to_string version)
          (OpamCompiler.Version.to_string version_d);
      let address field =
        OpamFormat.assoc_option s field
          (OpamFormat.parse_string @> address_of_string) in
      let src = address s_src in
      let archive = address s_archive in
      let http = address s_http in
      let git = address s_git in
      let darcs = address s_darcs in
      let hg = address s_hg in
      let local = address s_local in
      let src, kind =
        match URL.url_and_kind ~src ~archive ~http ~git ~darcs ~hg ~local
        with Some (u,k) -> Some u, k | None -> None, `http in
      let patches =
        OpamFormat.assoc_list s s_patches
          (OpamFormat.parse_list (OpamFormat.parse_string @> OpamFilename.raw)) in
      let configure = OpamFormat.assoc_string_list s s_configure in
      let make = OpamFormat.assoc_string_list s s_make      in
      let build = OpamFormat.assoc_list s s_build OpamFormat.parse_commands in
      let env = OpamFormat.assoc_list s s_env
          (OpamFormat.parse_list_list OpamFormat.parse_env_variable) in
      let packages = OpamFormat.assoc_default
          OpamFormula.Empty s s_packages OpamFormat.parse_formula in
      let preinstalled =
        OpamFormat.assoc_default false s s_preinstalled OpamFormat.parse_bool in
      let tags = OpamFormat.assoc_string_list s s_tags in

      if build <> [] && (configure @ make) <> [] then
        OpamGlobals.error_and_exit
          "%s: You cannot use 'build' and 'make'/'configure' fields at the same time."
          (OpamFilename.to_string filename);
      if not preinstalled && src = None then
        OpamGlobals.error_and_exit
          "%s: You should either specify an url (with 'sources')  or use 'preinstalled: \
           true' to pick the already installed compiler version."
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
      let s = if !OpamGlobals.compat_mode_1_0 then to_1_0 s else s in
      Syntax.to_string [] s

  end

  module Comp_descr = Descr

  module Subst = struct

    let internal = "subst"

    type t = string

    let empty = ""

    let of_channel _ ic =
      OpamSystem.string_of_channel ic

    let to_string _ t = t

    let replace t f = OpamFormat.replace t f

    let replace_string = replace

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
      Syntax.check ~versioned:false s valid_fields;
      let get f = OpamFormat.assoc_option s.file_contents f
        OpamFormat.parse_string in
      let opam_version = version_of_maybe_string (get s_opam_version) in
      let browse   = get s_browse in
      let upstream = get s_upstream in
      let redirect = OpamFormat.assoc_list s.file_contents s_redirect
          (OpamFormat.parse_list
             (OpamFormat.parse_option OpamFormat.parse_string OpamFormat.parse_filter))
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
      Syntax.to_string [] s

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
      OpamGlobals.msg "%d files %s:\n  %s\n%!"
        (List.length !read_files) kind (String.concat "\n  " l)
  in
  aux "read" !read_files;
  aux "write" !write_files

module Make (F : F) = struct

  let log fmt = OpamGlobals.log (Printf.sprintf "FILE(%s)" F.internal) fmt
  let slog = OpamGlobals.slog

  let write f v =
    let filename = OpamFilename.prettify f in
    let chrono = OpamGlobals.timer () in
    OpamFilename.write f (F.to_string f v);
    write_files := filename :: !write_files;
    log "Wrote %s in %.3fs" filename (chrono ())

  let string_of_backtrace_list = function
    | [] | _ when not (Printexc.backtrace_status ()) -> ""
    | btl -> List.fold_left (fun s bts ->
      let bt_lines = OpamMisc.split bts '\n' in
      "\n  Backtrace:\n    "^(String.concat "\n    " bt_lines)^s
    ) "" btl

  let read f =
    let filename = OpamFilename.prettify f in
    read_files := filename :: !read_files;
    let chrono = OpamGlobals.timer () in
    if OpamFilename.exists f then
      try
        let ic = OpamFilename.open_in f in
        let r = F.of_channel f ic in
        close_in ic;
        log "Read %s in %.3fs" filename (chrono ());
        r
      with
      | Lexer_error _ | Parsing.Parse_error as e ->
        raise e (* Message already printed *)
      | e ->
        OpamMisc.fatal e;
        let pos,msg,btl = match e with
          | OpamFormat.Bad_format (Some pos, btl, msg) -> pos, ":\n  "^msg, btl
          | OpamFormat.Bad_format (None, btl, msg) -> (f,-1,-1), ":\n  "^msg, btl
          | _ -> (f,-1,-1),"",[] in
        let e = OpamFormat.add_pos pos e in
        OpamGlobals.error "At %s%s%s"
          (string_of_pos pos) msg (string_of_backtrace_list btl);
        raise e
    else
      OpamSystem.internal_error "File %s does not exist" (OpamFilename.to_string f)

  let safe_read f =
    if OpamFilename.exists f then
      try read f with OpamFormat.Bad_format _ ->
        OpamGlobals.msg "[skipped]\n";
        F.empty
    else (
      log "Cannot find %a" (slog OpamFilename.to_string) f;
      F.empty
    )

  let dummy_file = OpamFilename.raw "<dummy>"

  let read_from_channel ic =
    try F.of_channel dummy_file ic with
    | OpamFormat.Bad_format (Some pos, btl, msg) as e ->
      OpamGlobals.error "At %s: %s%s"
        (string_of_pos pos) msg (string_of_backtrace_list btl);
      raise e
    | OpamFormat.Bad_format (None, btl, msg) as e ->
      OpamGlobals.error "Input error: %s%s" msg (string_of_backtrace_list btl);
      raise e

  let write_to_channel oc str =
    output_string oc (F.to_string dummy_file str)

end

open X

module type IO_FILE = sig
  type t
  val empty: t
  val write: filename -> t -> unit
  val read : filename -> t
  val safe_read: filename -> t
  val read_from_channel: in_channel -> t
  val write_to_channel: out_channel -> t -> unit
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
