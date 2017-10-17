(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2015 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** This module contains the handlers for reading and writing all of OPAM files,
    and defines their internal types (records for most of them).

    We handle three types of files:
    - raw text files, without lexing
    - "table" files, i.e. lexing is just cutting into lines and words, returning
      a string list list. These are mostly used internally
    - files using the "opam syntax" and lexer, parsed using OpamFormat.Pp.V
*)

open OpamTypes
open OpamTypesBase
open OpamStd.Op

module Pp = struct
  include OpamPp
  module V = OpamFormat.V
  module I = OpamFormat.I

  let warn ?pos ?(strict=OpamFormatConfig.(!r.strict)) ?exn fmt =
    if strict then
      match exn with
      | Some e -> raise e
      | None -> bad_format ?pos fmt
    else
      Printf.ksprintf (fun s ->
          if OpamConsole.verbose () then
            match exn with
            | None ->
              OpamConsole.warning "%s"
                (OpamPp.string_of_bad_format (Bad_format (pos, s)))
            | Some e ->
              OpamConsole.warning "%s" (OpamPp.string_of_bad_format e))
        fmt
end

open Pp.Op

type 'a t = filename

type 'a typed_file = 'a t

let make f = (f: 'a t)
let filename f = (f: 'a t :> filename)
let to_string f = OpamFilename.to_string (filename f)
let exists f = OpamFilename.exists (filename f)

module type IO_FILE = sig
  type t
  val empty: t
  val write: 'a typed_file -> t -> unit
  val read : 'a typed_file -> t
  val read_opt: 'a typed_file -> t option
  val safe_read: 'a typed_file -> t
  val read_from_channel: ?filename:'a typed_file -> in_channel -> t
  val read_from_string: ?filename:'a typed_file -> string -> t
  val write_to_channel: ?filename:'a typed_file -> out_channel -> t -> unit
  val write_to_string: ?filename:'a typed_file -> t -> string
end

module type IO_Arg = sig
  val internal : string
  type t
  val empty : t
  val of_channel : 'a typed_file -> in_channel  -> t
  val to_channel : 'a typed_file -> out_channel -> t -> unit
  val of_string : 'a typed_file -> string -> t
  val to_string : 'a typed_file -> t -> string
end

module Stats = struct
  let read_files = ref []
  let write_files = ref []
  let print () =
    let aux kind = function
      | [] -> ()
      | l  ->
        OpamConsole.msg "%d files %s:\n  %s\n%!"
          (List.length !read_files) kind (String.concat "\n  " l)
    in
    aux "read" !read_files;
    aux "write" !write_files
end

let dummy_file = OpamFilename.raw "<none>"

module MakeIO (F : IO_Arg) = struct

  let log ?level fmt =
    OpamConsole.log (Printf.sprintf "FILE(%s)" F.internal) ?level fmt
  let slog = OpamConsole.slog

  let write f v =
    let filename = OpamFilename.to_string f in
    let chrono = OpamConsole.timer () in
    let oc =
      OpamFilename.(mkdir (dirname f));
      try open_out_bin filename
      with Sys_error _ -> raise (OpamSystem.File_not_found filename)
    in
    try
      Unix.lockf (Unix.descr_of_out_channel oc) Unix.F_LOCK 0;
      F.to_channel f oc v;
      close_out oc;
      Stats.write_files := filename :: !Stats.write_files;
      log "Wrote %s in %.3fs" filename (chrono ())
    with e -> close_out oc; OpamFilename.remove f; raise e

  let read_opt f =
    let filename = OpamFilename.prettify f in
    let chrono = OpamConsole.timer () in
    try
      let ic = OpamFilename.open_in f in
      try
        Unix.lockf (Unix.descr_of_in_channel ic) Unix.F_RLOCK 0;
        Stats.read_files := filename :: !Stats.read_files;
        let r = F.of_channel f ic in
        close_in ic;
        log ~level:3 "Read %s in %.3fs" filename (chrono ());
        Some r
      with e -> close_in ic; raise e
    with
    | OpamSystem.File_not_found _ ->
      None
    | e ->
      OpamStd.Exn.fatal e;
      if OpamFormatConfig.(!r.strict) then
        (OpamConsole.error "%s"
           (Pp.string_of_bad_format ~file:(OpamFilename.to_string f) e);
         OpamConsole.error_and_exit `File_error "Strict mode: aborting")
      else raise e

  let read f =
    match read_opt f with
    | Some f -> f
    | None ->
      OpamSystem.internal_error "File %s does not exist or can't be read"
        (OpamFilename.to_string f)

  let safe_read f =
    try
      match read_opt f with
      | Some f -> f
      | None ->
        log ~level:2 "Cannot find %a" (slog OpamFilename.to_string) f;
        F.empty
    with
    | Pp.Bad_format _ as e->
      OpamConsole.error "%s [skipped]\n"
        (Pp.string_of_bad_format ~file:(OpamFilename.to_string f) e);
      F.empty

  let read_from_f f input =
    try f input with
    | Pp.Bad_format _ as e ->
      if OpamFormatConfig.(!r.strict) then
        (OpamConsole.error "%s" (Pp.string_of_bad_format e);
         OpamConsole.error_and_exit `File_error "Strict mode: aborting")
      else raise e

  let read_from_channel ?(filename=dummy_file) ic =
    read_from_f (F.of_channel filename) ic

  let read_from_string ?(filename=dummy_file) str =
    read_from_f (F.of_string filename) str

  let write_to_channel ?(filename=dummy_file) oc t =
    F.to_channel filename oc t

  let write_to_string ?(filename=dummy_file) t =
    F.to_string filename t
end

(** I - Raw text files (no parsing) *)

(** Compiler and package description files
    (<repo>/packages/.../descr, <repo>/compilers/.../<v>.descr):
    one-line title and content *)

module DescrIO = struct

  let internal = "descr"

  type t = string * string

  let empty = "", ""

  let synopsis = fst
  let body = snd

  let full (x,y) =
    match y with
    | "" -> x ^ "\n"
    | y -> String.concat "" [x; "\n\n"; y; "\n"]

  let of_channel _ ic =
    let x =
      try OpamStd.String.strip (input_line ic)
      with End_of_file | Sys_error _ -> "" in
    let y =
      try OpamStd.String.strip (OpamSystem.string_of_channel ic)
      with End_of_file | Sys_error _ -> ""
    in
    x, y

  let to_channel _ oc (x,y) =
    output_string oc x;
    output_char oc '\n';
    if y <> "" then
      (output_char oc '\n';
       output_string oc y;
       output_char oc '\n')

  let create str =
    let head, tail =
      match OpamStd.String.cut_at str '\n' with
      | None       -> str, ""
      | Some (h,t) -> h, t in
    OpamStd.String.strip head, OpamStd.String.strip tail

  let of_string _ = create

  let to_string _ = full

end
module Descr = struct
  include DescrIO
  include MakeIO(DescrIO)
end

(* module Comp_descr = Descr *)

(** Raw file interface used for variable expansions ( *.in ) *)

(*
module SubstIO = struct

  let internal = "subst"

  type t = string

  let empty = ""

  let of_channel _ ic =
    OpamSystem.string_of_channel ic

  let to_channel _ oc t =
    output_string oc t

  let of_string _ str = str

  let to_string _ t = t

end
module Subst = struct
  include SubstIO
  include MakeIO(SubstIO)
end
*)

(** II - Base word list list parser and associated file types *)

module LinesBase = struct

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

  let to_channel (_:filename) oc t =
    List.iter (function
        | [] -> ()
        | w::r ->
          output_string oc (escape_spaces w);
          List.iter (fun w ->
              output_char oc '\t';
              output_string oc (escape_spaces w))
            r;
          output_char oc '\n')
      t

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
               Buffer.add_char buf '\t';
               Buffer.add_string buf (escape_spaces w))
             r);
        Buffer.add_string buf "\n"
      ) lines;
    Buffer.contents buf

  let file_none = OpamFilename.of_string "<none>"

  let pp_string =
    Pp.pp
      (fun ~pos:_ s -> OpamLineLexer.main (Lexing.from_string s))
      (fun lines -> to_string file_none lines)

  let pp_channel ic oc =
    Pp.pp
      (fun ~pos:_ () -> of_channel file_none ic)
      (to_channel file_none oc)

end

module Lines = struct
  include LinesBase
  include MakeIO(LinesBase)
end

module type LineFileArg = sig
  val internal: string
  type t
  val empty: t
  val pp: (string list list, t) Pp.t
end

module LineFile (X: LineFileArg) = struct
  module IO = struct
    include X

    let to_channel _ oc t = Pp.print (Lines.pp_channel stdin oc -| pp) t

    let to_string _ t = Pp.print (Lines.pp_string -| pp) t

    let of_channel filename ic =
      Pp.parse (Lines.pp_channel ic stdout -| pp) ~pos:(pos_file filename) ()

    let of_string filename str =
      Pp.parse (Lines.pp_string -| pp)
        ~pos:(OpamFilename.to_string filename,0,0)
        str
  end

  include IO
  include MakeIO(IO)
end

(** (1) Internal usage only *)

(** Compiler aliases definitions (aliases): table
    <name> <compiler> *)

module Aliases = LineFile(struct

    let internal = "aliases"

    type t = string switch_map

    let empty = OpamSwitch.Map.empty

    let pp =
      OpamSwitch.Map.(OpamFormat.lines_map ~empty ~add ~fold) @@
      Pp.of_module "switch-name" (module OpamSwitch) ^+
      Pp.last

  end)

(** Indices of items and their associated source repository: table
    <fullname> <repo-name> <dir-prefix> *)

module Repo_index (A : OpamStd.ABSTRACT) = LineFile(struct

    let internal = "repo-index"

    type t = (repository_name * string option) A.Map.t

    let empty = A.Map.empty

    let pp =
      OpamFormat.lines_map ~empty ~add:A.Map.safe_add ~fold:A.Map.fold @@
      Pp.of_module "name" (module A) ^+
      Pp.of_module "repository" (module OpamRepositoryName) ^+
      Pp.opt Pp.last
  end)

module Package_index = Repo_index(OpamPackage)

(** List of packages (<switch>/installed, <switch>/installed-roots,
    <switch>/reinstall): table
    <package> <version> *)

module PkgList = LineFile (struct

    let internal = "package-version-list"

    type t = package_set

    let empty = OpamPackage.Set.empty

    let pp =
      OpamPackage.Set.(OpamFormat.lines_set ~empty ~add ~fold) @@
      (Pp.of_module "pkg-name" (module OpamPackage.Name) ^+
       Pp.last -| Pp.of_module "pkg-version" (module OpamPackage.Version))
      -| Pp.pp
        (fun ~pos:_ (n,v) -> OpamPackage.create n v)
        (fun nv -> nv.name, nv.version)

  end)

(** Lists of pinned packages (<switch>/pinned): table
    <name> <pin-kind> <target>

    Backwards-compatibility code, do not use *)
module Pinned_legacy = struct
  type pin_option =
    | Version of version
    | Source of url

  let pp_pin =
    let looks_like_version_re =
      Re.(compile @@
          seq [bos; digit; rep @@ diff any (set "/\\"); eos])
    in
    let pin_option_of_string ?kind s =
      match kind with
      | Some `version ->
        Version (OpamPackage.Version.of_string s)
      | None when Re.execp looks_like_version_re s ->
        Version (OpamPackage.Version.of_string s)
      | Some (#OpamUrl.backend as backend) ->
        Source (OpamUrl.parse ~backend s)
      | None ->
        Source (OpamUrl.parse ~handle_suffix:false s)
    in
    let string_of_pin_kind = function
      | `version -> "version"
      | `rsync   -> "path"
      | #OpamUrl.backend as ub -> OpamUrl.string_of_backend ub
    in
    let pin_kind_of_string = function
      | "version" -> `version
      | "path"    -> `rsync
      | s -> OpamUrl.backend_of_string s
    in
    let string_of_pin_option = function
      | Version v -> OpamPackage.Version.to_string v
      | Source url -> OpamUrl.to_string url
    in
    let kind_of_pin_option = function
      | Version _ -> `version
      | Source url -> (url.OpamUrl.backend :> pin_kind)
    in
    Pp.pp
      ~name:"?pin-kind pin-target"
      (fun ~pos -> function
         | [x] -> pin_option_of_string x
         | [k;x] -> pin_option_of_string ~kind:(pin_kind_of_string k) x
         | _ -> Pp.bad_format ~pos "Invalid number of fields")
      (fun x -> [string_of_pin_kind (kind_of_pin_option x);
                 string_of_pin_option x])

  include LineFile(struct

      let internal = "pinned"

      type t = pin_option OpamPackage.Name.Map.t

      let empty = OpamPackage.Name.Map.empty

      let pp =
        OpamPackage.Name.Map.(OpamFormat.lines_map ~empty ~add:safe_add ~fold) @@
        Pp.of_module "pkg-name" (module OpamPackage.Name) ^+
        pp_pin

    end)
end


(** Cached environment updates (<switch>/.opam-switch/environment) *)

module Environment = LineFile(struct

    let internal = "environment"

    type t = env_update list

    let empty = []

    let pp =
      (OpamFormat.lines_set ~empty:[] ~add:OpamStd.List.cons ~fold:List.fold_right @@
       Pp.identity ^+
       Pp.of_pair "env_update_op"
         (OpamLexer.env_update_op, OpamPrinter.env_update_op) ^+
       Pp.identity ^+
       Pp.opt Pp.singleton)
      -| Pp.pp (fun ~pos:_ -> List.rev) List.rev


    let pp =
       pp -|
       Pp.map_list
         (Pp.pp
           (fun ~pos:_ (a, (b, (c, d))) -> (a, b, c, d))
           (fun (a, b, c, d) -> (a, (b, (c, d)))))

  end)

(** (2) Part of the public repository format *)

(** repository index files ("urls.txt"): table
    <filename> <md5> <perms> *)

module File_attributes = LineFile(struct

    let internal = "file_attributes"

    type t = file_attribute_set

    let empty = OpamFilename.Attribute.Set.empty

    let pp =
      OpamFilename.Attribute.Set.(OpamFormat.lines_set ~empty ~add ~fold) @@
      (Pp.of_module "file" (module OpamFilename.Base) ^+
       Pp.of_pair "checksum" OpamHash.(of_string, contents) ^+
       Pp.opt (Pp.last -| Pp.of_pair "perm" (int_of_string, string_of_int))
      ) -|
      Pp.pp
        (fun ~pos:_ (base,(hash,perm)) ->
           OpamFilename.Attribute.create base hash perm)
        (fun att -> OpamFilename.Attribute.(base att, (md5 att, perm att)))

  end)

(** (3) Available in interface *)

(** Old Switch export/import format: table
    <name> <version> <installed-state> [pinning-kind] [pinning-url] *)

module StateTable = struct

  let internal = "export"

  module M = OpamPackage.Name.Map

  type t = switch_selections

  let empty = {
    sel_installed = OpamPackage.Set.empty;
    sel_roots = OpamPackage.Set.empty;
    sel_compiler = OpamPackage.Set.empty;
    sel_pinned = OpamPackage.Set.empty;
  }

  let pp_state =
    Pp.pp ~name:"pkg-state"
      (fun ~pos:_ -> function
         | "compiler" -> `Compiler
         | "root" -> `Root
         | "noroot" | "installed" -> `Installed
         | "uninstalled" -> `Uninstalled
         | "uninstalled-compiler" -> `Uninstalled_compiler
         | _ -> Pp.unexpected ())
      (function
        | `Compiler -> "compiler"
        | `Root -> "root"
        | `Installed -> "installed"
        | `Uninstalled -> "uninstalled"
        | `Uninstalled_compiler -> "uninstalled-compiler")

  let pp_lines =
    M.(OpamFormat.lines_map ~empty ~add:safe_add ~fold) @@
    Pp.of_module "pkg-name" (module OpamPackage.Name) ^+
    Pp.of_module "pkg-version" (module OpamPackage.Version) ^+
    (Pp.opt (pp_state ^+ Pp.opt Pinned_legacy.pp_pin) -|
     Pp.default (`Root, None))

  (* Convert from one name-map to type t *)
  let pp =
    pp_lines -| Pp.pp
      (fun ~pos:_ map ->
         M.fold
           (fun name (version,(state,pin)) t ->
              let nv = OpamPackage.create name version in
              {
                sel_installed = (match state with
                    | `Installed | `Root | `Compiler ->
                      OpamPackage.Set.add nv t.sel_installed
                    | `Uninstalled | `Uninstalled_compiler ->
                      t.sel_installed);
                sel_roots = (match state with
                    | `Root | `Compiler ->
                      OpamPackage.Set.add nv t.sel_roots
                    | `Installed | `Uninstalled | `Uninstalled_compiler ->
                      t.sel_roots);
                sel_compiler = (match state with
                    | `Compiler | `Uninstalled_compiler ->
                      OpamPackage.Set.add nv t.sel_compiler
                    | `Root | `Installed | `Uninstalled ->
                      t.sel_compiler);
                sel_pinned = (match pin with
                    | Some (Pinned_legacy.Version v) ->
                      OpamPackage.Set.add (OpamPackage.create name v)
                        t.sel_pinned
                    | Some _ ->
                      OpamPackage.Set.add (OpamPackage.create name version)
                        t.sel_pinned
                    | None -> t.sel_pinned);
              })
           map
           empty)
      (fun t ->
         M.empty |>
         OpamPackage.Set.fold (fun nv ->
             M.add nv.name
               (nv.version, (`Installed, None)))
           t.sel_installed |>
         OpamPackage.Set.fold (fun nv ->
             M.add nv.name
               (nv.version, (`Root, None)))
           t.sel_roots |>
         OpamPackage.Set.fold (fun nv acc ->
             let name = nv.name in
             try
               let (v, _) = M.find name acc in
               M.add name (v, (`Compiler, None)) acc
             with Not_found ->
               M.add name
                 (nv.version, (`Uninstalled_compiler, None))
                 acc)
           t.sel_compiler |>
         OpamPackage.Set.fold (fun nv map ->
             let state =
               try let _, (state, _) = M.find nv.name map in state
               with Not_found -> `Uninstalled
             in
             (* Incorrect: marks all pins as version. But this is deprecated. *)
             M.add nv.name
               (nv.version, (state, Some (Pinned_legacy.Version nv.version))) map)
           t.sel_pinned)

end

module LegacyState = struct
  type t = switch_selections
  include (LineFile (StateTable) : IO_FILE with type t := t)
end

(** III - Opam Syntax parser and associated file types *)


module Syntax = struct

  (* Idea: have a [(ic, oc_with_lock * t) pp] that can be used to reading and
     re-writing files with a guarantee that it hasn't been rewritten in the
     meantime *)

  let parser_main lexbuf filename =
    let error msg =
      let curr = lexbuf.Lexing.lex_curr_p in
      let start = lexbuf.Lexing.lex_start_p in
      let pos =
        curr.Lexing.pos_fname,
        start.Lexing.pos_lnum,
        start.Lexing.pos_cnum - start.Lexing.pos_bol
      in
      raise (OpamPp.Bad_format (Some pos, msg))
    in
    let filename = OpamFilename.to_string filename in
    lexbuf.Lexing.lex_curr_p <- { lexbuf.Lexing.lex_curr_p with
                                  Lexing.pos_fname = filename };
    try OpamParser.main OpamLexer.token lexbuf filename with
    | OpamLexer.Error msg -> error msg
    | Parsing.Parse_error -> error "Parse error"


  let pp_channel filename ic oc =
    Pp.pp
      (fun ~pos:_ () ->
         let lexbuf = Lexing.from_channel ic in
         parser_main lexbuf filename)
      (fun file ->
         let fmt = Format.formatter_of_out_channel oc in
         OpamPrinter.format_opamfile fmt file)

  let of_channel (filename:filename) (ic:in_channel) =
    Pp.parse ~pos:(pos_file filename) (pp_channel filename ic stdout) ()

  let to_channel filename oc t =
    Pp.print (pp_channel filename stdin oc) t

  let of_string (filename:filename) str =
    let lexbuf = Lexing.from_string str in
    parser_main lexbuf filename

  let to_string _file_name t =
    OpamPrinter.opamfile t

  let to_string_with_preserved_format
      filename ?(format_from=filename) ?format_from_string
      ~empty ?(sections=[]) ~fields pp t =
    let current_str_opt =
      match format_from_string with
      | Some s -> Some s
      | None ->
        try Some (OpamFilename.read format_from)
        with OpamSystem.File_not_found _ -> None
    in
    match current_str_opt with
    | None -> to_string filename (Pp.print pp (filename, t))
    | Some str ->
    let syn_file = of_string filename str in
    let syn_t = Pp.print pp (filename, t) in
    let it_ident = function
      | Variable (_, f, _) -> `Var f
      | Section (_, {section_kind = k; section_name = n; _}) -> `Sec (k,n)
    in
    let it_pos = function
      | Section (pos,_) | Variable (pos,_,_) -> pos
    in
    let lines_index =
      let rec aux acc s =
        let until =
          try Some (String.index_from s (List.hd acc) '\n')
          with Not_found -> None
        in
        match until with
        | Some until -> aux (until+1 :: acc) s
        | None -> Array.of_list (List.rev acc)
      in
      aux [0] str
    in
    let pos_index (_file, li, col) = lines_index.(li - 1) + col in
    let field_str ident =
      let rec aux = function
        | it1 :: r when it_ident it1 = ident ->
          let start = pos_index (it_pos it1) in
          let stop = match r with
            | it2 :: _ -> pos_index (it_pos it2) - 1
            | [] ->
              let len = ref (String.length str) in
              while str.[!len - 1] = '\n' do decr len done;
              !len
          in
          String.sub str start (stop - start)
        | _ :: r -> aux r
        | [] -> raise Not_found
      in
      aux syn_file.file_contents
    in
    let rem, strs =
      List.fold_left (fun (rem, strs) item ->
          List.filter (fun i -> it_ident i <> it_ident item) rem,
          match item with
          | Variable (pos, name, v) ->
            (try
               let ppa = List.assoc name fields in
               match snd (Pp.print ppa t) with
               | None | Some (List (_, [])) | Some (List (_,[List(_,[])])) ->
                 strs
               | field_syn_t when
                   field_syn_t =
                   snd (Pp.print ppa (Pp.parse ppa ~pos (empty, Some v)))
                 ->
                 (* unchanged *)
                 field_str (`Var name) :: strs
               | _ ->
                 try
                   let f =
                     List.find (fun i -> it_ident i = `Var name) syn_t.file_contents
                   in
                   OpamPrinter.items [f] :: strs
                 with Not_found -> strs
             with Not_found | OpamPp.Bad_format _ ->
               if OpamStd.String.starts_with ~prefix:"x-" name then
                 field_str (`Var name) :: strs
               else strs)
          | Section (pos, {section_kind; section_name; section_items}) ->
            (try
               let ppa = List.assoc section_kind sections in
               let print_sec ppa t =
                 match snd (Pp.print ppa t) with
                 | None -> None
                 | Some v ->
                   try Some (List.assoc section_name v) with Not_found -> None
               in
               let sec_field_t = print_sec ppa t in
               if sec_field_t <> None &&
                  sec_field_t =
                  print_sec ppa
                    (Pp.parse ppa ~pos
                       (empty, Some [section_name, section_items]))
               then
                 (* unchanged *)
                 field_str (`Sec (section_kind, section_name)) :: strs
               else
               try
                 let f =
                   List.filter
                     (fun i -> it_ident i = `Sec (section_kind, section_name))
                     syn_t.file_contents
                 in
                 OpamPrinter.items f :: strs
               with Not_found -> strs
             with Not_found | OpamPp.Bad_format _ -> strs)
        )
        (syn_t.file_contents, []) syn_file.file_contents
    in
    String.concat "\n"
      (List.rev_append strs
         (if rem = [] then [""] else [OpamPrinter.items rem;""]))

end

module type SyntaxFileArg = sig
  val internal: string
  type t
  val empty: t
  val pp: (opamfile, filename * t) Pp.t
end

module SyntaxFile(X: SyntaxFileArg) : IO_FILE with type t := X.t = struct

  module IO = struct
    let to_opamfile filename t = Pp.print X.pp (filename, t)

    let of_channel filename (ic:in_channel) =
      Pp.parse X.pp ~pos:(pos_file filename) (Syntax.of_channel filename ic)
      |> snd

    let to_channel filename oc t =
      Syntax.to_channel filename oc (to_opamfile filename t)

    let of_string (filename:filename) str =
      Pp.parse X.pp ~pos:(pos_file filename) (Syntax.of_string filename str)
      |> snd

    let to_string filename t =
      Syntax.to_string filename (to_opamfile filename t)
  end

  include IO
  include X
  include MakeIO(struct
      include X
      include IO
    end)
end

(** (1) Internal files *)

(** Structure shared by a few file formats *)
module Wrappers = struct

  type t = {
    pre_build : arg list list;
    wrap_build : arg list;
    post_build : arg list list;
    pre_install : arg list list;
    wrap_install : arg list;
    post_install : arg list list;
    pre_remove : arg list list;
    wrap_remove : arg list;
    post_remove : arg list list;
  }

  let empty = {
    pre_build = [];
    wrap_build = [];
    post_build = [];
    pre_install = [];
    wrap_install = [];
    post_install = [];
    pre_remove = [];
    wrap_remove = [];
    post_remove = [];
  }

  let pre_build t = t.pre_build
  let wrap_build t = t.wrap_build
  let post_build t = t.post_build
  let pre_install t = t.pre_install
  let wrap_install t = t.wrap_install
  let post_install t = t.post_install
  let pre_remove t = t.pre_remove
  let wrap_remove t = t.wrap_remove
  let post_remove t = t.post_remove

  let with_pre_build pre_build t = { t with pre_build }
  let with_wrap_build wrap_build t = { t with wrap_build }
  let with_post_build post_build t = { t with post_build }
  let with_pre_install pre_install t = { t with pre_install }
  let with_wrap_install wrap_install t = { t with wrap_install }
  let with_post_install post_install t = { t with post_install }
  let with_pre_remove pre_remove t = { t with pre_remove }
  let with_wrap_remove wrap_remove t = { t with wrap_remove }
  let with_post_remove post_remove t = { t with post_remove }

  let fields = [
    "pre-build-commands", Pp.ppacc
      with_pre_build pre_build
      (Pp.V.map_list ~depth:2 (Pp.V.map_list Pp.V.arg));
    "pre-install-commands", Pp.ppacc
      with_pre_install pre_install
      (Pp.V.map_list ~depth:2 (Pp.V.map_list Pp.V.arg));
    "pre-remove-commands", Pp.ppacc
      with_pre_remove pre_remove
      (Pp.V.map_list ~depth:2 (Pp.V.map_list Pp.V.arg));
    "wrap-build-commands", Pp.ppacc
      with_wrap_build wrap_build
      (Pp.V.map_list ~depth:1 Pp.V.arg);
    "wrap-install-commands", Pp.ppacc
      with_wrap_install wrap_install
      (Pp.V.map_list ~depth:1 Pp.V.arg);
    "wrap-remove-commands", Pp.ppacc
      with_wrap_remove wrap_remove
      (Pp.V.map_list ~depth:1 Pp.V.arg);
    "post-build-commands", Pp.ppacc
      with_post_build post_build
      (Pp.V.map_list ~depth:2 (Pp.V.map_list Pp.V.arg));
    "post-install-commands", Pp.ppacc
      with_post_install post_install
      (Pp.V.map_list ~depth:2 (Pp.V.map_list Pp.V.arg));
    "post-remove-commands", Pp.ppacc
      with_post_remove post_remove
      (Pp.V.map_list ~depth:2 (Pp.V.map_list Pp.V.arg));
  ]

  let with_default ~default t =
    let f = function [] -> fun l -> l | l -> fun _ -> l in
    {
      pre_build = f t.pre_build default.pre_build;
      wrap_build = f t.wrap_build default.wrap_build;
      post_build = f t.post_build default.post_build;
      pre_install = f t.pre_install default.pre_install;
      wrap_install = f t.wrap_install default.wrap_install;
      post_install = f t.post_install default.post_install;
      pre_remove = f t.pre_remove default.pre_remove;
      wrap_remove = f t.wrap_remove default.wrap_remove;
      post_remove = f t.post_remove default.post_remove;
    }

  let add ~outer ~inner = {
    pre_build = outer.pre_build @ inner.pre_build;
    wrap_build = outer.wrap_build @ inner.wrap_build;
    post_build = inner.post_build @ outer.post_build;
    pre_install = outer.pre_install @ inner.pre_install;
    wrap_install = outer.wrap_install @ inner.wrap_install;
    post_install = inner.post_install @ outer.post_install;
    pre_remove = outer.pre_remove @ inner.pre_remove;
    wrap_remove = outer.wrap_remove @ inner.wrap_remove;
    post_remove = inner.post_remove @ outer.post_remove;
  }

end

(** General opam configuration (config) *)

module ConfigSyntax = struct

  let internal = "config"

  type t = {
    opam_version : opam_version;
    repositories : repository_name list;
    installed_switches : switch list;
    switch : switch option;
    jobs : int;
    dl_tool : arg list option;
    dl_jobs : int;
    dl_cache : url list option;
    wrappers : Wrappers.t;
    solver_criteria : (solver_criteria * string) list;
    best_effort_prefix : string option;
    solver : arg list option;
    global_variables : (variable * variable_contents * string) list;
    eval_variables : (variable * string list * string) list;
    validation_hook : arg list option;
    default_compiler : formula;
  }

  let opam_version t = t.opam_version
  let repositories t = t.repositories
  let installed_switches t = t.installed_switches
  let switch t = t.switch
  let jobs t = t.jobs
  let dl_tool t = t.dl_tool
  let dl_jobs t = t.dl_jobs
  let dl_cache t = OpamStd.Option.default [] t.dl_cache
  let criteria t = t.solver_criteria
  let best_effort_prefix t = t.best_effort_prefix
  let criterion kind t =
    try Some (List.assoc kind t.solver_criteria)
    with Not_found -> None
  let solver t = t.solver
  let wrappers t = t.wrappers

  let global_variables t = t.global_variables
  let eval_variables t = t.eval_variables

  let validation_hook t = t.validation_hook
  let default_compiler t = t.default_compiler

  let with_opam_version opam_version t = { t with opam_version }
  let with_repositories repositories t = { t with repositories }
  let with_installed_switches installed_switches t =
    { t with installed_switches }
  let with_switch_opt switch t = { t with switch }
  let with_switch switch t = { t with switch = Some switch }
  let with_jobs jobs t = { t with jobs }
  let with_dl_tool dl_tool t = { t with dl_tool = Some dl_tool }
  let with_dl_tool_opt dl_tool t = { t with dl_tool }
  let with_dl_jobs dl_jobs t = { t with dl_jobs }
  let with_dl_cache dl_cache t = { t with dl_cache = Some dl_cache }
  let with_criteria solver_criteria t = { t with solver_criteria }
  let with_criterion kind criterion t =
    { t with solver_criteria =
               (kind,criterion)::List.remove_assoc kind t.solver_criteria }
  let with_best_effort_prefix s t = { t with best_effort_prefix = Some s }
  let with_solver solver t = { t with solver = Some solver }
  let with_solver_opt solver t = { t with solver = solver }
  let with_wrappers wrappers t = { t with wrappers }
  let with_global_variables global_variables t = { t with global_variables }
  let with_eval_variables eval_variables t = { t with eval_variables }
  let with_validation_hook validation_hook t =
    { t with validation_hook = Some validation_hook}
  let with_validation_hook_opt validation_hook t = { t with validation_hook }
  let with_default_compiler default_compiler t = { t with default_compiler }

  let empty = {
    opam_version = OpamVersion.current_nopatch;
    repositories = [];
    installed_switches = [];
    switch = None;
    jobs = 1;
    dl_tool = None;
    dl_jobs = 1;
    dl_cache = None;
    solver_criteria = [];
    best_effort_prefix = None;
    solver = None;
    wrappers = Wrappers.empty;
    global_variables = [];
    eval_variables = [];
    validation_hook = None;
    default_compiler = OpamFormula.Empty;
  }

  let fields =
    let with_switch sw t =
      if t.switch = None then with_switch sw t
      else Pp.bad_format "Multiple switch specifications"
    in
    [
      "opam-version", Pp.ppacc
        with_opam_version opam_version
        (Pp.V.string -| Pp.of_module "opam-version" (module OpamVersion));
      "repositories", Pp.ppacc
        with_repositories repositories
        (Pp.V.map_list ~depth:1
           (Pp.V.string -|
            Pp.of_module "repository" (module OpamRepositoryName)));
      "installed-switches", Pp.ppacc
        with_installed_switches installed_switches
        (Pp.V.map_list ~depth:1
           (Pp.V.string -|
            Pp.of_module "switch" (module OpamSwitch)));
      "switch", Pp.ppacc_opt
        with_switch switch
        (Pp.V.string -| Pp.of_module "switch" (module OpamSwitch));
      "jobs", Pp.ppacc
        with_jobs jobs
        Pp.V.pos_int;
      "download-command", Pp.ppacc_opt
        with_dl_tool dl_tool
        (Pp.V.map_list ~depth:1 Pp.V.arg);
      "download-jobs", Pp.ppacc
        with_dl_jobs dl_jobs
        Pp.V.pos_int;
      "archive-mirrors", Pp.ppacc_opt
        with_dl_cache (fun t -> t.dl_cache)
        (Pp.V.map_list ~depth:1 Pp.V.url);
      "solver-criteria", Pp.ppacc_opt
        (with_criterion `Default) (criterion `Default)
        Pp.V.string;
      "solver-upgrade-criteria", Pp.ppacc_opt
        (with_criterion `Upgrade) (criterion `Upgrade)
        Pp.V.string;
      "solver-fixup-criteria", Pp.ppacc_opt
        (with_criterion `Fixup) (criterion `Fixup)
        Pp.V.string;
      "best-effort-prefix-criteria", Pp.ppacc_opt
        with_best_effort_prefix best_effort_prefix
        Pp.V.string;
      "solver", Pp.ppacc_opt
        with_solver solver
        (Pp.V.map_list ~depth:1 Pp.V.arg);
      "global-variables", Pp.ppacc
        with_global_variables global_variables
        (Pp.V.map_list ~depth:2
           (Pp.V.map_triple
              (Pp.V.ident -| Pp.of_module "variable" (module OpamVariable))
              Pp.V.variable_contents
              Pp.V.string));
      "eval-variables", Pp.ppacc
        with_eval_variables eval_variables
        (Pp.V.map_list ~depth:2
           (Pp.V.map_triple
              (Pp.V.ident -| Pp.of_module "variable" (module OpamVariable))
              (Pp.V.map_list Pp.V.string)
              Pp.V.string));
      "repository-validation-command", Pp.ppacc_opt
        with_validation_hook validation_hook
        (Pp.V.map_list ~depth:1 Pp.V.arg);
      "default-compiler", Pp.ppacc
        with_default_compiler default_compiler
        (Pp.V.package_formula `Disj Pp.V.(constraints Pp.V.version));

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
    ] @
    List.map
      (fun (fld, ppacc) -> fld, Pp.embed with_wrappers wrappers ppacc)
      Wrappers.fields

  let pp =
    let name = internal in
    Pp.I.map_file @@
    Pp.I.fields ~name ~empty fields -|
    Pp.I.show_errors ~name ~strict:OpamCoreConfig.(not !r.safe_mode) ()

end
module Config = struct
  include ConfigSyntax
  include SyntaxFile(ConfigSyntax)
end

module InitConfigSyntax = struct
  let internal = "init-config"

  type t = {
    opam_version : opam_version;
    repositories : (repository_name * (url * trust_anchors option)) list;
    default_compiler : formula;
    jobs : int option;
    dl_tool : arg list option;
    dl_jobs : int option;
    dl_cache : url list option;
    solver_criteria : (solver_criteria * string) list;
    solver : arg list option;
    wrappers : Wrappers.t;
    global_variables : (variable * variable_contents * string) list;
    eval_variables : (variable * string list * string) list;
  }

  let opam_version t = t.opam_version
  let repositories t = t.repositories
  let default_compiler t = t.default_compiler
  let jobs t = t.jobs
  let dl_tool t = t.dl_tool
  let dl_jobs t = t.dl_jobs
  let dl_cache t = OpamStd.Option.default [] t.dl_cache
  let solver_criteria t = t.solver_criteria
  let solver t = t.solver
  let wrappers t = t.wrappers
  let global_variables t = t.global_variables
  let eval_variables t = t.eval_variables

  let with_opam_version opam_version t = {t with opam_version}
  let with_repositories repositories t = {t with repositories}
  let with_default_compiler default_compiler t = {t with default_compiler}
  let with_jobs jobs t = {t with jobs}
  let with_dl_tool dl_tool t = {t with dl_tool}
  let with_dl_jobs dl_jobs t = {t with dl_jobs}
  let with_dl_cache dl_cache t = {t with dl_cache = Some dl_cache}
  let with_solver_criteria solver_criteria t = {t with solver_criteria}
  let with_solver solver t = {t with solver}
  let with_wrappers wrappers t = {t with wrappers}
  let with_global_variables global_variables t = {t with global_variables}
  let with_eval_variables eval_variables t = {t with eval_variables}

  let criterion kind t =
    try Some (List.assoc kind t.solver_criteria)
    with Not_found -> None

  let with_criterion kind criterion t =
    { t with solver_criteria =
               (kind,criterion)::List.remove_assoc kind t.solver_criteria }

  let empty = {
    opam_version = OpamVersion.current_nopatch;
    repositories = [];
    default_compiler = OpamFormula.Empty;
    jobs = None;
    dl_tool = None;
    dl_jobs = None;
    dl_cache = None;
    solver_criteria = [];
    solver = None;
    wrappers = Wrappers.empty;
    global_variables = [];
    eval_variables = [];
  }

  let pp_repository_def =
    Pp.V.map_options_3
      (Pp.V.string -|
       Pp.of_module "repository" (module OpamRepositoryName))
      (Pp.opt @@ Pp.singleton -| Pp.V.url)
      (Pp.map_list Pp.V.string)
      (Pp.opt @@
       Pp.singleton -| Pp.V.int -|
       OpamPp.check ~name:"quorum" ~errmsg:"quorum must be >= 0" ((<=) 0)) -|
    Pp.pp
      (fun ~pos:_ (name, url, fingerprints, quorum) ->
         name, url,
         match fingerprints with [] -> None | fingerprints ->
           Some {fingerprints; quorum = OpamStd.Option.default 1 quorum})
      (fun (name, url, ta) -> match ta with
         | Some ta ->  name, url, ta.fingerprints, Some ta.quorum
         | None -> name, url, [], None)

  let fields =
    [
      "opam-version", Pp.ppacc
        with_opam_version opam_version
        (Pp.V.string -| Pp.of_module "opam-version" (module OpamVersion));
      "repositories", Pp.ppacc
        with_repositories repositories
        (Pp.V.map_list ~depth:1 @@
         pp_repository_def -|
         Pp.pp (fun ~pos -> function
             | (name, Some url, ta) -> (name, (url, ta))
             | (_, None, _) -> Pp.bad_format ~pos "Missing repository URL")
           (fun (name, (url, ta)) -> (name, Some url, ta)));
      "default-compiler", Pp.ppacc
        with_default_compiler default_compiler
        (Pp.V.package_formula `Disj Pp.V.(constraints Pp.V.version));
      "jobs", Pp.ppacc_opt
        (with_jobs @* OpamStd.Option.some) jobs
        Pp.V.pos_int;
      "download-command", Pp.ppacc_opt
        (with_dl_tool @* OpamStd.Option.some) dl_tool
        (Pp.V.map_list ~depth:1 Pp.V.arg);
      "download-jobs", Pp.ppacc_opt
        (with_dl_jobs @* OpamStd.Option.some) dl_jobs
        Pp.V.pos_int;
      "archive-mirrors", Pp.ppacc
        with_dl_cache dl_cache
        (Pp.V.map_list ~depth:1 Pp.V.url);
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
        (with_solver @* OpamStd.Option.some) solver
        (Pp.V.map_list ~depth:1 Pp.V.arg);
      "global-variables", Pp.ppacc
        with_global_variables global_variables
        (Pp.V.map_list ~depth:2
           (Pp.V.map_triple
              (Pp.V.ident -| Pp.of_module "variable" (module OpamVariable))
              Pp.V.variable_contents
              Pp.V.string));
      "eval-variables", Pp.ppacc
        with_eval_variables eval_variables
        (Pp.V.map_list ~depth:2
           (Pp.V.map_triple
              (Pp.V.ident -| Pp.of_module "variable" (module OpamVariable))
              (Pp.V.map_list Pp.V.string)
              Pp.V.string));
    ] @
    List.map
      (fun (fld, ppacc) -> fld, Pp.embed with_wrappers wrappers ppacc)
      Wrappers.fields


  let pp =
    let name = internal in
    Pp.I.map_file @@
    Pp.I.fields ~name ~empty fields -|
    Pp.I.show_errors ~name ~strict:true ()

  let add t1 t2 =
    let opt = function None -> fun o -> o | some -> fun _ -> some in
    let list = function [] -> fun l -> l | l -> fun _ -> l in
    {
      opam_version = t2.opam_version;
      repositories = list t2.repositories t1.repositories;
      default_compiler =
        if t2.default_compiler <> Empty
        then t2.default_compiler else t1.default_compiler;
      jobs = opt t2.jobs t1.jobs;
      dl_tool = opt t2.dl_tool t1.dl_tool;
      dl_jobs = opt t2.dl_jobs t1.dl_jobs;
      dl_cache = opt t2.dl_cache t1.dl_cache;
      solver_criteria =
        List.fold_left (fun acc c ->
            try (c, List.assoc c t2.solver_criteria) :: acc with Not_found ->
            try (c, List.assoc c t1.solver_criteria) :: acc with Not_found ->
              acc)
          [] [`Fixup; `Upgrade; `Default];
      solver = opt t2.solver t1.solver;
      wrappers = Wrappers.with_default ~default:t1.wrappers t2.wrappers;
      global_variables = list t2.global_variables t1.global_variables;
      eval_variables = list t2.eval_variables t1.eval_variables;
    }

end
module InitConfig = struct
  include InitConfigSyntax
  include SyntaxFile(InitConfigSyntax)
end

module Repos_configSyntax = struct

  let internal = "repos-config"

  type t = ((url * trust_anchors option) option) OpamRepositoryName.Map.t

  let empty = OpamRepositoryName.Map.empty

  let fields = [
    "repositories",
    Pp.ppacc (fun x _ -> x) (fun x -> x)
      ((Pp.V.map_list ~depth:1 @@
        InitConfigSyntax.pp_repository_def -|
        Pp.pp
          (fun ~pos:_ -> function
             | (name, Some url, ta) -> name, Some (url, ta)
             | (name, None, _) -> name, None)
          (fun (name, def) -> match def with
             | Some (url, ta) -> name, Some url, ta
             | None -> name, None, None)) -|
       Pp.of_pair "repository-url-list"
         OpamRepositoryName.Map.(of_list, bindings));
  ]

  let pp =
    let name = internal in
    Pp.I.map_file @@
    Pp.I.fields ~name ~empty fields -|
    Pp.I.show_errors ~name ()

end
module Repos_config = struct
  include Repos_configSyntax
  include SyntaxFile(Repos_configSyntax)
end

module Switch_configSyntax = struct

  let internal = "switch-config"

  type t = {
    opam_version: OpamVersion.t;
    synopsis: string;
    repos: repository_name list option;
    paths: (std_path * string) list;
    variables: (variable * variable_contents) list;
    opam_root: dirname option;
    wrappers: Wrappers.t;
    env: env_update list;
  }

  let empty = {
    opam_version = OpamVersion.current_nopatch;
    synopsis = "";
    repos = None;
    paths = [];
    variables = [];
    opam_root = None;
    wrappers = Wrappers.empty;
    env = [];
  }

  let sections = [
    "paths", Pp.ppacc
      (fun paths t -> {t with paths}) (fun t -> t.paths)
      (Pp.I.anonymous_section Pp.I.items -|
       Pp.map_list
         (Pp.map_pair
            (Pp.of_pair "std-path" (std_path_of_string, string_of_std_path))
            Pp.V.string));
    "variables", Pp.ppacc
      (fun variables t -> {t with variables}) (fun t -> t.variables)
      (Pp.I.anonymous_section Pp.I.items -|
       Pp.map_list
         (Pp.map_pair
            (Pp.of_module "variable" (module OpamVariable))
            Pp.V.variable_contents));
  ]

  let fields = [
    "opam-version", Pp.ppacc
      (fun opam_version t -> {t with opam_version}) (fun t -> t.opam_version)
      (Pp.V.string -| Pp.of_module "opam-version" (module OpamVersion));
    "synopsis", Pp.ppacc
      (fun synopsis t -> {t with synopsis}) (fun t -> t.synopsis)
      Pp.V.string;
    "repositories",
    Pp.ppacc_opt (fun r t -> {t with repos = Some r}) (fun t -> t.repos)
      (Pp.V.map_list ~depth:1 @@
       Pp.V.string -| Pp.of_module "repo" (module OpamRepositoryName));
    "opam-root", Pp.ppacc_opt
      (fun r t -> {t with opam_root = Some r}) (fun t -> t.opam_root)
      (Pp.V.string -| Pp.of_module "dirname" (module OpamFilename.Dir));
    "setenv", Pp.ppacc
      (fun env t -> {t with env}) (fun t -> t.env)
      (Pp.V.map_list ~depth:2 Pp.V.env_binding);
  ] @
    List.map
      (fun (fld, ppacc) ->
         fld, Pp.embed (fun wrappers t -> {t with wrappers}) (fun t -> t.wrappers) ppacc)
      Wrappers.fields

  let pp =
    let name = internal in
    Pp.I.map_file @@
    Pp.I.fields ~name ~empty ~sections fields -|
    Pp.I.show_errors ~name ()

  let variable t s =
    try Some (List.assoc s t.variables)
    with Not_found -> None

  let path t p =
    try Some (List.assoc p t.paths)
    with Not_found -> None

  let wrappers t = t.wrappers

end
module Switch_config = struct
  include Switch_configSyntax
  include SyntaxFile(Switch_configSyntax)
end

module SwitchSelectionsSyntax = struct

  let internal = "switch-state"

  type t = switch_selections

  let empty = {
    sel_installed = OpamPackage.Set.empty;
    sel_roots = OpamPackage.Set.empty;
    sel_compiler = OpamPackage.Set.empty;
    sel_pinned = OpamPackage.Set.empty;
  }

  let pp_package =
    Pp.of_module "package" (module OpamPackage)

  let pp_pkglist =
    Pp.V.map_list (Pp.V.string -| pp_package) -|
    Pp.pp (fun ~pos:_ -> OpamPackage.Set.of_list) OpamPackage.Set.elements

  let fields = [
    "opam-version", Pp.ppacc
      (fun _ t -> t) (fun _ -> OpamVersion.current_nopatch)
      (Pp.V.string -| Pp.of_module "opam-version" (module OpamVersion));
    "compiler", Pp.ppacc
      (fun sel_compiler t -> {t with sel_compiler}) (fun t -> t.sel_compiler)
      pp_pkglist;
    "roots", Pp.ppacc
      (fun sel_roots t -> {t with sel_roots})
      (fun t -> t.sel_roots)
      pp_pkglist;
    "installed", Pp.ppacc
      (fun installed t ->
         {t with sel_installed = installed})
      (fun t -> t.sel_installed)
      pp_pkglist;
    "pinned", Pp.ppacc
      (fun sel_pinned t -> {t with sel_pinned}) (fun t -> t.sel_pinned)
      (Pp.V.map_list ~depth:1
         (Pp.V.option -|
          (* The contents of the option is obsolete, the information is now
             contained in the overlay only *)
          Pp.pp (fun ~pos:_ (nv,_) -> nv) (fun nv -> nv, []) -|
          Pp.V.string -| pp_package) -|
       Pp.of_pair "Package set" OpamPackage.Set.(of_list, elements))
  ]

  let pp =
    let name = "switch-state" in
    Pp.I.map_file @@
    Pp.I.check_opam_version () -|
    Pp.I.fields ~name ~empty fields -|
    Pp.I.show_errors ~name ()

end

module SwitchSelections = struct
  type t = switch_selections
  include SyntaxFile(SwitchSelectionsSyntax)
end

(** Local repository config file (repo/<repo>/config) *)

module Repo_config_legacySyntax = struct

  let internal = "repo-file"

  type t = {
    repo_name : repository_name;
    repo_root : dirname;
    repo_url : url;
    repo_priority : int;
  }

  let empty = {
    repo_name = OpamRepositoryName.of_string "<none>";
    repo_url = OpamUrl.empty;
    repo_root = OpamFilename.raw_dir "<none>";
    repo_priority = 0;
  }

  let fields = [
    "name", Pp.ppacc
      (fun repo_name (r:t) -> {r with repo_name})
      (fun r -> r.repo_name)
      (Pp.V.string -|
       Pp.of_module "repository-name" (module OpamRepositoryName));
    "address", Pp.ppacc
      (fun repo_url (r:t) -> {r with repo_url})
      (fun r -> r.repo_url)
      Pp.V.url;
    "kind", Pp.ppacc_opt (* deprecated *)
      (fun backend (r:t) ->
         {r with repo_url = {r.repo_url with OpamUrl.backend}})
      OpamStd.Option.none
      (Pp.V.string -|
       Pp.of_pair "repository-kind"
         OpamUrl.(backend_of_string, string_of_backend));
    "priority", Pp.ppacc
      (fun repo_priority (r:t) -> {r with repo_priority})
      (fun r -> r.repo_priority)
      Pp.V.int;
    "root", Pp.ppacc
      (fun repo_root (r:t) -> {r with repo_root})
      (fun r -> r.repo_root)
      (Pp.V.string -|
       Pp.of_module "directory" (module OpamFilename.Dir));
  ]

  let pp =
    let name = internal in
    Pp.I.map_file @@
    Pp.I.fields ~name ~empty ~mandatory_fields:["root";"address";"name"]
      fields -|
    Pp.I.show_errors ~name ~strict:true ()

end
module Repo_config_legacy = struct
  include Repo_config_legacySyntax
  include SyntaxFile(Repo_config_legacySyntax)
end

(** Global or package switch-local configuration variables.
    (<switch>/config/global-config.config,
    <switch>/lib/<pkgname>/opam.config) *)

module Dot_configSyntax = struct

  let internal = ".config"

  type t = {
    vars: (variable * variable_contents) list;
    file_depends: (filename * OpamHash.t) list;
  }

  let empty = {
    vars = [];
    file_depends = [];
  }

  let create vars = { empty with vars }

  let vars t = t.vars
  let with_vars vars t = { t with vars }

  let file_depends t = t.file_depends
  let with_file_depends file_depends t = { t with file_depends }

  let pp_variables =
    Pp.I.items -|
    Pp.map_list
      (Pp.map_pair
         (Pp.of_module "variable" (module OpamVariable))
         Pp.V.variable_contents)

  let pp_contents =
    Pp.I.fields ~name:"config-file" ~empty
      ~sections:[
        "variables", Pp.ppacc with_vars vars
          (Pp.I.anonymous_section pp_variables)
      ]
      [
        "opam-version", Pp.ppacc (fun _ t -> t) (fun _ -> OpamVersion.current)
          (Pp.V.string -| Pp.of_module "opam-version" (module OpamVersion));
        "file-depends", Pp.ppacc with_file_depends file_depends
          (Pp.V.map_list ~depth:2 @@ Pp.V.map_pair
             (Pp.V.string -| Pp.of_module "path" (module OpamFilename))
             (Pp.V.string -| Pp.of_module "checksum" (module OpamHash)))
      ]
    -| Pp.I.show_errors ~name:internal ()

  (* Files with the variables at toplevel and no other fields are allowed for
     backwards-compat, when opam-version is unset or too old *)
  let pp =
    Pp.I.map_file @@
    Pp.I.field "opam-version"
      (Pp.parse
         (Pp.V.string -| Pp.of_module "opam-version" (module OpamVersion)))
    -| Pp.pp
      (fun ~pos (opam_version_opt, s) ->
         match opam_version_opt with
         | Some v when
             OpamVersion.compare v (OpamVersion.of_string "1.3~dev3") > 0 ->
           Pp.parse ~pos pp_contents s
         | _ -> {empty with vars = Pp.parse ~pos pp_variables s})
      (fun t -> None, Pp.print pp_contents t)


  let variables t = List.rev_map fst t.vars

  let bindings t = t.vars

  let variable t s =
    try Some (List.assoc s t.vars)
    with Not_found -> None

  let set k v t =
    let vars = List.remove_assoc k t.vars in
    let vars =
      match v with
      | Some v -> (k,v) :: vars
      | None -> vars
    in
    { t with vars }

end
module Dot_config = struct
  include Dot_configSyntax
  include SyntaxFile(Dot_configSyntax)
end

(** (2) General, public repository format *)

(** Public repository definition file (<repo>/repo) *)

module RepoSyntax = struct

  let internal = "repo"

  type t = {
    opam_version : OpamVersion.t option;
    browse       : string option;
    upstream     : string option;
    redirect     : (string * filter option) list;
    root_url     : url option;
    dl_cache     : string list option;
    announce     : (string * filter option) list;
    stamp        : string option;
  }

  let create
      ?browse ?upstream ?opam_version
      ?(redirect=[]) ?root_url ?dl_cache ?(announce=[]) ?stamp () =
    { opam_version; browse; upstream; redirect; root_url; dl_cache; announce;
      stamp; }

  let empty = create ()

  let opam_version t = t.opam_version
  let browse t = t.browse
  let upstream t = t.upstream
  let redirect t = t.redirect
  let root_url t = t.root_url
  let dl_cache t = OpamStd.Option.default [] t.dl_cache
  let announce t = t.announce
  let stamp t = t.stamp

  let with_opam_version opam_version t =
    { t with opam_version = Some opam_version }
  let with_browse browse t = { t with browse = Some browse }
  let with_upstream upstream t = { t with upstream = Some upstream }
  let with_redirect redirect t = { t with redirect }
  let with_root_url root_url t = { t with root_url = Some root_url }
  let with_dl_cache dl_cache t = { t with dl_cache = Some dl_cache }
  let with_announce announce t = { t with announce }
  let with_stamp id t = { t with stamp = Some id }
  let with_stamp_opt stamp t = { t with stamp }

  let fields = [
    "opam-version", Pp.ppacc_opt
      with_opam_version opam_version
      (Pp.V.string -| Pp.of_module "opam-version" (module OpamVersion));
    "browse", Pp.ppacc_opt with_browse browse Pp.V.string;
    "upstream", Pp.ppacc_opt with_upstream upstream Pp.V.string;
    "redirect", Pp.ppacc
      with_redirect redirect
      (Pp.V.map_list ~depth:1
         (Pp.V.map_option Pp.V.string (Pp.opt Pp.V.filter)));
    "archive-mirrors", Pp.ppacc
      with_dl_cache dl_cache
      (Pp.V.map_list ~depth:1 Pp.V.string);
    "announce", Pp.ppacc
      with_announce announce
      (Pp.V.map_list ~depth:1
         (Pp.V.map_option Pp.V.string (Pp.opt Pp.V.filter)));
    "stamp", Pp.ppacc_opt
      with_stamp stamp
      Pp.V.string
  ]

  let pp =
    let name = internal in
    Pp.I.map_file @@
    Pp.I.fields ~name ~empty fields -|
    Pp.I.show_errors ~name
      ~condition:(function
          | {opam_version = Some v; _} -> OpamVersion.(compare current v) >= 0
          | _ -> true)
      ()

end
module Repo = struct
  include RepoSyntax
  include SyntaxFile(RepoSyntax)
end


(** Package url files (<repo>/packages/.../url) *)

module URLSyntax = struct

  let internal = "url-file"

  type t = {
    url     : url;
    mirrors : url list;
    checksum: OpamHash.t list;
    errors  : (string * Pp.bad_format) list;
  }

  let create ?(mirrors=[]) ?(checksum=[]) url =
    {
      url; mirrors; checksum; errors = [];
    }

  let empty = {
    url     = OpamUrl.empty;
    mirrors = [];
    checksum= [];
    errors  = [];
  }

  let url t = t.url
  let mirrors t = t.mirrors
  let checksum t = t.checksum

  let with_url url t = { t with url }
  let with_mirrors mirrors t = { t with mirrors }
  let with_checksum checksum t = { t with checksum = checksum }

  let fields =
    let with_url url t =
      if t.url <> OpamUrl.empty then Pp.bad_format "Too many URLS"
      else with_url url t
    in
    [
      "src", Pp.ppacc with_url url
        Pp.V.url;
      "archive", Pp.ppacc_opt with_url OpamStd.Option.none
        (Pp.V.url_with_backend `http);
      "http", Pp.ppacc_opt with_url OpamStd.Option.none
        (Pp.V.url_with_backend `http);
      "git",  Pp.ppacc_opt with_url OpamStd.Option.none
        (Pp.V.url_with_backend `git);
      "darcs",  Pp.ppacc_opt with_url OpamStd.Option.none
        (Pp.V.url_with_backend `darcs);
      "hg",  Pp.ppacc_opt with_url OpamStd.Option.none
        (Pp.V.url_with_backend `hg);
      "local",  Pp.ppacc_opt with_url OpamStd.Option.none
        (Pp.V.url_with_backend `rsync);
      "checksum", Pp.ppacc with_checksum checksum
        (Pp.V.map_list ~depth:1
           (Pp.V.string -| Pp.of_module "checksum" (module OpamHash)));
      "mirrors", Pp.ppacc with_mirrors mirrors
        (Pp.V.map_list ~depth:1 Pp.V.url);
    ]

  let pp_contents =
    let name = internal in
    Pp.I.fields ~name ~empty fields -|
    Pp.I.on_errors ~name (fun t e -> {t with errors = e::t.errors}) -|
    Pp.check ~name (fun t -> t.url <> OpamUrl.empty) ~errmsg:"missing URL"

  let pp = Pp.I.map_file pp_contents

end
module URL = struct
  include URLSyntax
  include SyntaxFile(URLSyntax)
end



(** (3) Opam package format *)

module OPAMSyntax = struct

  let internal = "opam"

  type t = {
    opam_version: opam_version;

    (* Package ident *)
    name       : OpamPackage.Name.t option;
    version    : OpamPackage.Version.t option;

    (* Relationships; solver and availability info *)
    depends    : filtered_formula;
    depopts    : filtered_formula;
    conflicts  : filtered_formula;
    conflict_class : name list;
    available  : filter;
    flags      : package_flag list;
    env        : env_update list;

    (* Build instructions *)
    build      : command list;
    run_test   : command list;
    install    : command list;
    remove     : command list;

    (* Auxiliary data affecting the build *)
    substs     : basename list;
    patches    : (basename * filter option) list;
    build_env  : env_update list;
    features   : (OpamVariable.t * filtered_formula * string) list;
    extra_sources: (basename * URL.t) list;

    (* User-facing data used by opam *)
    messages   : (string * filter option) list;
    post_messages: (string * filter option) list;
    depexts    : (string list * filter) list;
    libraries  : (string * filter option) list;
    syntax     : (string * filter option) list;
    dev_repo   : url option;
    pin_depends: (package * url) list;

    (* Package database details *)
    maintainer : string list;
    author     : string list;
    license    : string list;
    tags       : string list;
    homepage   : string list;
    doc        : string list;
    bug_reports: string list;

    (* Extension fields (x-foo: "bar") *)
    extensions : (pos * value) OpamStd.String.Map.t;

    (* Extra sections *)
    url        : URL.t option;
    descr      : Descr.t option;

    (* Extra data, not actually file fields *)

    (* Related metadata directory (not an actual field of the file)
       This can be used to locate e.g. the files/ overlays *)
    metadata_dir: dirname option;

    (* Names and hashes of the files below files/ *)
    extra_files: (OpamFilename.Base.t * OpamHash.t) list option;

    (* Stores any file errors for printing them later *)
    format_errors: (string * Pp.bad_format) list;

    (* Deprecated, for compat and proper linting *)
    ocaml_version: (OpamFormula.relop * string) OpamFormula.formula option;
    os         : (bool * string) generic_formula;

    deprecated_build_test : command list;
    deprecated_build_doc  : command list;
  }

  let empty = {
    opam_version = OpamVersion.current_nopatch;

    name       = None;
    version    = None;

    depends    = OpamFormula.Empty;
    depopts    = OpamFormula.Empty;
    conflicts  = OpamFormula.Empty;
    available  = FBool true;
    flags      = [];
    conflict_class = [];
    env        = [];

    build      = [];
    run_test   = [];
    install    = [];
    remove     = [];

    substs     = [];
    patches    = [];
    build_env  = [];
    features   = [];
    extra_sources = [];

    messages   = [];
    post_messages = [];
    depexts    = [];
    libraries  = [];
    syntax     = [];
    dev_repo   = None;
    pin_depends= [];

    maintainer = [];
    author     = [];
    license    = [];
    tags       = [];
    homepage   = [];
    doc        = [];
    bug_reports = [];

    extensions  = OpamStd.String.Map.empty;
    url         = None;
    descr       = None;

    metadata_dir = None;
    extra_files = None;

    format_errors = [];

    ocaml_version = None;
    os         = Empty;
    deprecated_build_test = [];
    deprecated_build_doc  = [];
  }

  let create nv =
    let name = Some (nv.OpamPackage.name) in
    let version = Some (nv.OpamPackage.version) in
    { empty with name; version }

  let check t name = function
    | None ->
      let pos =
        OpamStd.Option.Op.(OpamFilename.Op.(
            t.metadata_dir >>| fun d -> pos_file (d // "opam")))
      in
      Pp.bad_format ?pos "Field '%s:' is required" name
    | Some n -> n

  let ext_field_prefix = "x-"
  let is_ext_field = OpamStd.String.starts_with ~prefix:ext_field_prefix

  (* Getters *)

  let opam_version t = t.opam_version
  let name (t:t) = check t "name" t.name
  let name_opt (t:t) = t.name
  let version (t:t) = check t "version" t.version
  let version_opt (t:t) = t.version
  let package t = OpamPackage.create (name t) (version t)

  let depends t = t.depends
  let depopts t = t.depopts
  let conflicts t = t.conflicts
  let conflict_class t = t.conflict_class
  let available t = t.available
  let flags t = t.flags
  let has_flag f t = List.mem f t.flags
  let env (t:t) =
    List.map
      (fun env -> match t.name, env with
        | Some name, (var,op,value,None) ->
          var, op, value,
          Some ("Updated by package " ^ OpamPackage.Name.to_string name)
        | _, b -> b)
      t.env

  let build t = t.build
  let run_test t = t.deprecated_build_test @ t.run_test
  let deprecated_build_test t = t.deprecated_build_test
  let deprecated_build_doc t = t.deprecated_build_doc
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
  let pin_depends t = t.pin_depends

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
       | Pp.Bad_format _ as e -> raise (Pp.add_pos pos e))
    with Not_found -> None

  let url t = t.url
  let descr t = t.descr
  let synopsis t = OpamStd.Option.map Descr.synopsis t.descr
  let descr_body t = match t.descr with
    | None | Some (_, "") -> None
    | Some (_, text) -> Some text
  let get_url t = match url t with Some u -> Some (URL.url u) | None -> None

  let format_errors t = t.format_errors

  let metadata_dir t = t.metadata_dir
  let extra_files t = t.extra_files

  (* Setters *)

  let with_opam_version opam_version t = { t with opam_version }

  let with_name name (t:t) = { t with name = Some name }
  let with_name_opt name (t:t) = { t with name }
  let with_version version (t:t) = { t with version = Some version }
  let with_version_opt version (t:t) = { t with version }
  let with_nv nv (t:t) =
    { t with name = Some (nv.OpamPackage.name);
             version = Some (nv.OpamPackage.version) }

  let with_depends depends t = { t with depends }
  let with_depopts depopts t = { t with depopts }
  let with_conflicts conflicts t = {t with conflicts }
  let with_conflict_class conflict_class t = { t with conflict_class }
  let with_available available t = { t with available }
  let with_flags flags t = { t with flags }
  let add_flags flags t =
    { t with flags = OpamStd.List.sort_nodup compare (flags @ t.flags) }
  let with_env env t = { t with env }

  let with_build build t = { t with build }
  let with_run_test run_test t = { t with run_test }
  let with_deprecated_build_test deprecated_build_test t =
    { t with deprecated_build_test }
  let with_deprecated_build_doc deprecated_build_doc t =
    { t with deprecated_build_doc }
  let with_install install t = { t with install }
  let with_remove remove t = { t with remove }

  let with_substs substs t = { t with substs }
  let with_patches patches t = { t with patches }
  let with_build_env build_env t = { t with build_env }
  let with_features features t = {t with features }
  let with_extra_sources extra_sources t = { t with extra_sources }

  let with_messages messages t = { t with messages }
  let with_post_messages post_messages t = { t with post_messages }
  let with_depexts depexts t = { t with depexts = depexts }
  let with_libraries libraries t = { t with libraries }
  let with_syntax syntax t = { t with syntax }
  let with_dev_repo dev_repo t = { t with dev_repo = Some dev_repo }
  let with_dev_repo_opt dev_repo t = { t with dev_repo }
  let with_pin_depends pin_depends t = { t with pin_depends }

  let with_maintainer maintainer t = { t with maintainer }
  let with_author author t = { t with author }
  let with_license license t = { t with license }
  let with_tags tags t = { t with tags }
  let with_homepage homepage t = { t with homepage }
  let with_doc doc t = { t with doc }
  let with_bug_reports bug_reports t = { t with bug_reports }

  let with_extensions extensions t =
    if not (OpamStd.String.Map.for_all (fun k _ -> is_ext_field k) extensions)
    then invalid_arg "OpamFile.OPAM.with_extensions";
    {t with
     extensions = OpamStd.String.Map.map (fun s -> pos_null, s) extensions }
  let add_extension t fld syn =
    if not (is_ext_field fld) then invalid_arg "OpamFile.OPAM.add_extension";
    {t with
     extensions = OpamStd.String.Map.add fld (pos_null,syn) t.extensions }

  let with_url url t =
    let format_errors =
      List.map (fun (name,bf) -> "url."^name, bf) url.URL.errors
    in
    { t with url = Some url;
             format_errors = format_errors @ t.format_errors }
  let with_url_opt url t =
    let format_errors = match url with
      | None -> []
      | Some u -> List.map (fun (name,bf) -> "url."^name, bf) u.URL.errors
    in
    { t with url;
             format_errors = format_errors @ t.format_errors }

  let with_descr descr t = { t with descr = Some descr }
  let with_descr_opt descr t = { t with descr }
  let with_synopsis synopsis t =
    { t with descr =
               Some (synopsis, OpamStd.Option.default "" (descr_body t)) }
  let with_descr_body text t =
    { t with descr =
               Some (OpamStd.Option.default "" (synopsis t), text) }

  let with_metadata_dir metadata_dir t = { t with metadata_dir }
  let with_extra_files extra_files t = { t with extra_files = Some extra_files }
  let with_extra_files_opt extra_files t = { t with extra_files }

  let with_format_errors format_errors t = { t with format_errors }

  let with_ocaml_version ocaml_version t =
    { t with ocaml_version = Some ocaml_version }
  let with_os os t = { t with os }

  (* Post-processing functions used for some fields (optional, because we
     don't want them when linting). It's better to do them in the same pass
     as parsing, because it allows to get file positions, which we lose
     afterwards *)

  (* Allow 'flag:xxx' tags as flags, for compat *)
  let flag_of_tag tag =
    let prefix = "flags:" in
    if OpamStd.String.starts_with ~prefix tag then
      Some (pkg_flag_of_string (OpamStd.String.remove_prefix ~prefix tag))
    else None

  let cleanup_name _opam_version ~pos:(file,_,_ as pos) name =
    match OpamPackage.of_filename (OpamFilename.of_string file) with
    | Some nv when nv.OpamPackage.name <> name ->
      Pp.warn ~pos "This file is for package '%s' but its 'name:' field \
                    advertises '%s'."
        (OpamPackage.name_to_string nv) (OpamPackage.Name.to_string name);
      nv.OpamPackage.name
    | _ -> name

  let cleanup_version _opam_version ~pos:(file,_,_ as pos) version =
    match OpamPackage.of_filename (OpamFilename.of_string file) with
    | Some nv when nv.OpamPackage.version <> version ->
      Pp.warn ~pos "This file is for version '%s' but its 'version:' field \
                    advertises '%s'."
        (OpamPackage.version_to_string nv) (OpamPackage.Version.to_string version);
      nv.OpamPackage.version
    | _ -> version

  let cleanup_depopts opam_version ~pos depopts =
    if OpamFormatConfig.(!r.skip_version_checks) ||
       OpamVersion.compare opam_version (OpamVersion.of_string "1.2") < 0
    then depopts
    else
    (* Make sure depopts are a pure disjunction *)
    let rec aux acc disjunction =
      List.fold_left (fun acc -> function
          | OpamFormula.Atom _ as atom -> atom :: acc
          | f ->
            Pp.warn ~pos
              "Optional dependencies must be a disjunction. \
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
        OpamFormula.(ors_to_list f)
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
    force_disjunction conflicts

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

  let cleanup_tags opam_version ~pos tags =
    let flags = OpamStd.List.filter_map flag_of_tag tags in
    ignore (cleanup_flags opam_version ~pos flags);
    tags

  let cleanup_dev_repo opam_version ~pos:_ dev_repo =
    if OpamVersion.(compare opam_version (of_string "1.3") >= 0) then
      dev_repo
    else
      OpamUrl.parse ~handle_suffix:true (OpamUrl.to_string dev_repo)

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
        Pp.V.pkgname;
      "version", with_cleanup cleanup_version
        Pp.ppacc_opt with_version version_opt
        (Pp.V.string_tr -| Pp.of_module "version" (module OpamPackage.Version));

      "synopsis", no_cleanup Pp.ppacc_opt with_synopsis synopsis
        Pp.V.string_tr;
      "description", no_cleanup Pp.ppacc_opt with_descr_body descr_body
        Pp.V.string_tr;

      "maintainer", no_cleanup Pp.ppacc with_maintainer maintainer
        (Pp.V.map_list ~depth:1 Pp.V.string);
      "authors", no_cleanup Pp.ppacc
        with_author author
        (Pp.V.map_list ~depth:1 Pp.V.string);
      "author", no_cleanup Pp.ppacc
        (fun a t -> if t.author = [] then with_author a t else
            Pp.bad_format "multiple \"authors:\" fields" author)
        (fun _ -> [])
        (Pp.V.map_list ~depth:1 Pp.V.string);
      "license", no_cleanup Pp.ppacc with_license license
        (Pp.V.map_list ~depth:1 Pp.V.string);
      "tags", with_cleanup cleanup_tags Pp.ppacc with_tags tags
        (Pp.V.map_list ~depth:1 Pp.V.string);
      "homepage", no_cleanup Pp.ppacc with_homepage homepage
        (Pp.V.map_list ~depth:1 Pp.V.string);
      "doc", no_cleanup Pp.ppacc with_doc doc
        (Pp.V.map_list ~depth:1 Pp.V.string);
      "bug-reports", no_cleanup Pp.ppacc with_bug_reports bug_reports
        (Pp.V.map_list ~depth:1 Pp.V.string);

      "depends", no_cleanup Pp.ppacc with_depends depends
        (Pp.V.package_formula `Conj Pp.V.(filtered_constraints ext_version));
      "depopts", with_cleanup cleanup_depopts Pp.ppacc with_depopts depopts
        (Pp.V.package_formula `Disj Pp.V.(filtered_constraints ext_version));
      "conflicts", with_cleanup cleanup_conflicts
        Pp.ppacc with_conflicts conflicts
        (Pp.V.package_formula `Disj Pp.V.(filtered_constraints ext_version));
      "conflict-class", no_cleanup Pp.ppacc with_conflict_class conflict_class
        (Pp.V.map_list ~depth:1 Pp.V.pkgname);
      "available", no_cleanup Pp.ppacc with_available available
        (Pp.V.list_depth 1 -| Pp.V.list -| Pp.V.filter);
      "flags", with_cleanup cleanup_flags Pp.ppacc add_flags flags
        (Pp.V.map_list ~depth:1 @@
         Pp.V.ident -|
         Pp.of_pair "package-flag" (pkg_flag_of_string, string_of_pkg_flag));
      "setenv", no_cleanup Pp.ppacc with_env env
        (Pp.V.map_list ~depth:2 Pp.V.env_binding);

      "build", no_cleanup Pp.ppacc with_build build
        (Pp.V.map_list ~depth:2 Pp.V.command);
      "run-test", no_cleanup Pp.ppacc with_run_test run_test
        (Pp.V.map_list ~depth:2 Pp.V.command);
      "install", no_cleanup Pp.ppacc with_install install
        (Pp.V.map_list ~depth:2 Pp.V.command);
      "remove", no_cleanup Pp.ppacc with_remove remove
        (Pp.V.map_list ~depth:2 Pp.V.command);

      "substs", no_cleanup Pp.ppacc with_substs substs
        (Pp.V.map_list ~depth:1 pp_basename);
      "patches", no_cleanup Pp.ppacc with_patches patches
        (Pp.V.map_list ~depth:1 @@
         Pp.V.map_option pp_basename (Pp.opt Pp.V.filter));
      "build-env", no_cleanup Pp.ppacc with_build_env build_env
        (Pp.V.map_list ~depth:2 Pp.V.env_binding);
      "features", no_cleanup Pp.ppacc with_features features
        (Pp.V.map_list ~depth:2 @@
         Pp.V.map_options_2
           (Pp.V.ident -| Pp.of_module "variable" (module OpamVariable))
           (Pp.V.package_formula_items `Conj Pp.V.(filtered_constraints ext_version))
           (Pp.singleton -| Pp.V.string));

      "messages", no_cleanup Pp.ppacc with_messages messages
        (Pp.V.map_list ~depth:1 @@
         Pp.V.map_option Pp.V.string_tr (Pp.opt Pp.V.filter));
      "post-messages", no_cleanup Pp.ppacc with_post_messages post_messages
        (Pp.V.map_list ~depth:1 @@
         Pp.V.map_option Pp.V.string_tr (Pp.opt Pp.V.filter));
      "depexts", no_cleanup Pp.ppacc with_depexts depexts
        (Pp.fallback
           (Pp.V.map_list ~depth:2 @@
            Pp.V.map_option (Pp.V.map_list Pp.V.string) (Pp.V.filter))
           (Pp.V.map_list ~depth:3
              (let rec filter_of_taglist = function
                 | [] -> FBool true
                 | [v] -> FString v
                 | v :: r -> FAnd (FString v, filter_of_taglist r)
               in
               Pp.V.map_pair
                 (Pp.V.map_list Pp.V.string -|
                  Pp.of_pair "tag-list"
                    (filter_of_taglist, fun _ -> assert false))
                 (Pp.V.map_list Pp.V.string) -|
               Pp.pp (fun ~pos:_ (a,b) -> b,a) (fun (b,a) -> a,b))));
      "libraries", no_cleanup Pp.ppacc with_libraries libraries
        (Pp.V.map_list ~depth:1 @@
         Pp.V.map_option Pp.V.string (Pp.opt Pp.V.filter));
      "syntax", no_cleanup Pp.ppacc with_syntax syntax
        (Pp.V.map_list ~depth:1 @@
         Pp.V.map_option Pp.V.string (Pp.opt Pp.V.filter));
      "dev-repo", with_cleanup cleanup_dev_repo Pp.ppacc_opt with_dev_repo dev_repo
        (Pp.V.string -|
         Pp.of_pair "vc-url"
           OpamUrl.(parse ?backend:None ~handle_suffix:false, to_string));
      "pin-depends", no_cleanup Pp.ppacc with_pin_depends pin_depends
        (OpamFormat.V.map_list ~depth:2
           (OpamFormat.V.map_pair
              (OpamFormat.V.string -|
               OpamPp.of_module "package" (module OpamPackage))
              (OpamFormat.V.string -|
               OpamPp.of_module "URL" (module OpamUrl))));

      "extra-files", no_cleanup Pp.ppacc_opt with_extra_files extra_files
        (Pp.V.map_list ~depth:2 @@
         Pp.V.map_pair
           pp_basename
           (Pp.V.string -| Pp.of_module "checksum" (module OpamHash)));

      (* deprecated fields, here for compat *)
      "configure-style", (Pp.ppacc_ignore, Pp.ppacc_ignore);

      "ocaml-version", no_cleanup
        Pp.ppacc_opt with_ocaml_version OpamStd.Option.none
        (Pp.V.list_depth 1 -| Pp.V.list -|
         Pp.V.constraints Pp.V.compiler_version);
      "os", no_cleanup Pp.ppacc_opt with_os OpamStd.Option.none
        Pp.V.os_constraint;
      "descr", no_cleanup Pp.ppacc_opt with_descr OpamStd.Option.none
        (Pp.V.string_tr -|
         Pp.of_pair "descr" Descr.(of_string (), to_string ()));
      "extra-sources", no_cleanup Pp.ppacc_opt
        with_extra_sources OpamStd.Option.none
        (Pp.V.map_list ~depth:2 @@
         Pp.V.map_pair
           (Pp.V.map_option
              Pp.V.url
              (Pp.opt @@ Pp.singleton -| pp_basename))
           (Pp.V.string -| Pp.of_module "checksum" (module OpamHash))
         -| Pp.pp
           (fun ~pos:_ ((u,b),md5) ->
              OpamStd.Option.default
                (OpamFilename.Base.of_string (OpamUrl.basename u)) b,
              URL.create ~checksum:[md5] u)
           (fun (f, urlf) ->
              URL.((url urlf, Some f), List.hd (checksum urlf))));
      "build-test", no_cleanup Pp.ppacc_opt
        with_deprecated_build_test OpamStd.Option.none
        (Pp.V.map_list ~depth:2 Pp.V.command);
      "build-doc", no_cleanup Pp.ppacc_opt
        with_deprecated_build_doc (fun x -> Some (deprecated_build_doc x))
        (Pp.V.map_list ~depth:2 Pp.V.command);
    ]

  (* These don't have a printer and their info is stored in new fields *)
  let alias_fields = [
    "author", "authors";
    "descr", "description";
  ]

  (* These don't have a printer and their info can't be retrieved in the same
     format anymore *)
  let deprecated_fields = [
    "ocaml-version";
    "os";
    "configure-style";
    "extra-sources";
    "build-test";
    "build-doc";
  ]

  let fields =
    List.map (fun (name, (_, cleaned_up_pp)) -> name, cleaned_up_pp)
      fields_gen

  let sections = [
    "url", Pp.ppacc_opt with_url url (Pp.I.anonymous_section URL.pp_contents);
    "extra-source", Pp.ppacc with_extra_sources extra_sources
      (Pp.map_list
         (Pp.map_pair
            (Pp.pp
               (fun ~pos -> function
                  | Some o -> OpamFilename.Base.of_string o
                  | None -> Pp.bad_format ~pos "missing extra-source name")
               (fun b -> Some (OpamFilename.Base.to_string b)))
            URL.pp_contents))
  ]

  let raw_fields =
    List.map (fun (name, (raw_pp, _)) -> name, raw_pp)
      fields_gen

  let handle_flags_in_tags =
    let parse ~pos:_ t =
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

  let handle_deprecated_available =
    let add_available available filter =
      match available with
      | FBool true -> filter
      | f -> FAnd (filter, f)
    in
    let parse ~pos:_ t =
      let available = t.available in
      let available =
        match t.ocaml_version with
        | None -> available
        | Some ocaml_version ->
          let var = OpamVariable.of_string "ocaml-version" in
          let mk_atom (op,v) =
            FOp (FIdent ([], var, None), op, FString v)
          in
          let filter = OpamFilter.of_formula mk_atom ocaml_version in
          add_available available filter
      in
      let available =
        match t.os with
        | Empty -> available
        | os ->
          let var = OpamVariable.of_string "os" in
          let mk_atom (eq,name) =
            FOp (FIdent ([], var, None), (if eq then `Eq else `Neq), FString name)
          in
          let filter = OpamFilter.of_formula mk_atom os in
          add_available available filter
      in
      { t with available }
    in
    Pp.pp parse (fun x -> x)

  (* Doesn't handle package name encoded in directory name *)
  let pp_raw_fields =
    Pp.I.check_opam_version () -|
    Pp.I.partition_fields is_ext_field -| Pp.map_pair
      (Pp.I.items -|
       OpamStd.String.Map.(Pp.pp (fun ~pos:_ -> of_list) bindings))
      (Pp.I.fields ~name:"opam-file" ~empty ~sections fields -|
       Pp.I.on_errors (fun t e -> {t with format_errors=e::t.format_errors}) -|
       handle_flags_in_tags -|
       handle_deprecated_available) -|
    Pp.pp
      (fun ~pos:_ (extensions, t) -> with_extensions extensions t)
      (fun t -> extensions t, t)

  let pp_raw = Pp.I.map_file @@ pp_raw_fields

  let pp =
    pp_raw -|
    Pp.pp
      (fun ~pos:_ (filename, t) ->
         filename,
         let metadata_dir =
           if filename <> dummy_file then Some (OpamFilename.dirname filename)
           else None
         in
         let t = { t with metadata_dir } in
         match OpamPackage.of_filename filename with
         | Some nv -> with_nv nv t
         | None -> t)
      (fun (filename, t) ->
         filename,
         match OpamPackage.of_filename filename, t.name, t.version with
         | Some _, None, None -> t
         | None, Some _, Some _ -> t
         | None, _, _ ->
           OpamConsole.log "FILE(opam)"
             "Outputting opam file %s with unspecified name or version"
             (OpamFilename.to_string filename);
           t
         | Some nv, _, _ ->
           if t.name <> None && t.name <> Some (nv.OpamPackage.name) ||
              t.version <> None && t.version <> Some (nv.OpamPackage.version)
           then
             OpamConsole.warning
               "Skipping inconsistent 'name:' or 'version:' fields (%s) \
                while saving %s"
               (OpamPackage.to_string @@
                OpamPackage.create
                  (OpamStd.Option.default (nv.OpamPackage.name) t.name)
                  (OpamStd.Option.default (nv.OpamPackage.version) t.version))
               (OpamFilename.prettify filename);
           {t with name = None; version = None})

  let to_string_with_preserved_format
      ?format_from ?format_from_string filename t =
    Syntax.to_string_with_preserved_format
      ?format_from ?format_from_string filename ~empty
      ~sections ~fields:raw_fields pp t

  let write_with_preserved_format
      ?format_from ?format_from_string filename t =
    let s = to_string_with_preserved_format ?format_from ?format_from_string filename t in
    OpamFilename.write filename s

  let contents ?(filename=dummy_file) t =
    Pp.print pp (filename, t)

  let to_list ?filename t =
    let rec aux acc pfx = function
      | Section (_, {section_kind; section_name=None; section_items}) :: r ->
        aux (aux acc (section_kind :: pfx) section_items) pfx r
      | Section (_, {section_kind; section_name=Some n; section_items}) :: r ->
        aux
          (aux acc (Printf.sprintf "%s(%s)" section_kind n :: pfx)
             section_items)
          pfx r
      | Variable (_, name, value) :: r ->
        aux (((name :: pfx), value) :: acc) pfx r
      | [] -> acc
    in
    List.rev_map
      (fun (pfx, value) -> String.concat "." (List.rev pfx), value)
      (aux [] [] (contents ?filename t).file_contents)

  let print_field_as_syntax field t =
    let field = try List.assoc field alias_fields with Not_found -> field in
    if List.mem field deprecated_fields then raise Not_found;
    match OpamStd.String.cut_at field '.' with
    | None ->
      if is_ext_field field
      then
        OpamStd.Option.map snd
          (OpamStd.String.Map.find_opt field t.extensions)
      else snd (Pp.print (List.assoc field fields) t)
    | Some (sec, field) ->
      match snd (Pp.print (List.assoc sec sections) t) with
      | None -> None
      | Some items ->
        (* /!\ returns only the first result for multiple named sections *)
        Some (OpamStd.List.find_map (function
            | Variable (_, f, contents) when f = field -> Some contents
            | _ -> None)
            (List.flatten (List.map snd items)))

end
module OPAM = struct
  include OPAMSyntax
  include SyntaxFile(OPAMSyntax)

  (** Extra stuff for opam files *)

  let effective_part (t:t) =
    {
      opam_version = empty.opam_version;

      name       = t.name;
      version    = t.version;

      depends    = t.depends;
      depopts    = t.depopts;
      conflicts  = t.conflicts;
      conflict_class = t.conflict_class;
      available  = t.available;
      flags      = t.flags;
      env        = t.env;

      build      = t.build;
      run_test   = t.deprecated_build_test @ t.run_test;
      install    = t.install;
      remove     = t.remove;

      substs     = t.substs;
      patches    = t.patches;
      build_env  = t.build_env;
      features   = t.features;
      extra_sources = t.extra_sources;

      messages   = empty.messages;
      post_messages = empty.post_messages;
      depexts    = empty.depexts;
      libraries  = empty.libraries;
      syntax     = empty.syntax;
      dev_repo   = empty.dev_repo;
      pin_depends = empty.pin_depends;

      maintainer = empty.maintainer;
      author     = empty.author;
      license    = empty.license;
      tags       = empty.tags;
      homepage   = empty.homepage;
      doc        = empty.doc;
      bug_reports = empty.bug_reports;

      extensions  = empty.extensions;
      url         =
        (match t.url with
         | None -> None
         | Some u -> match URL.checksum u with
           | [] -> Some (URL.create (URL.url u)) (* ignore mirrors *)
           | cksum::_ ->
             Some (URL.with_checksum [cksum] URL.empty));
             (* ignore actual url and extra checksums *)
      descr       = empty.descr;

      metadata_dir = empty.metadata_dir;
      extra_files = OpamStd.Option.Op.(t.extra_files ++ Some []);

      format_errors = empty.format_errors;

      ocaml_version = empty.ocaml_version;
      os         = empty.os;

      deprecated_build_test = []; (* merged into run_test *)
      deprecated_build_doc = t.deprecated_build_doc;
    }

  let effectively_equal o1 o2 =
    effective_part o1 = effective_part o2

  let equal o1 o2 =
    with_metadata_dir None o1 = with_metadata_dir None o2

  let get_extra_files o =
    OpamStd.Option.Op.(
      (metadata_dir o >>= fun mdir ->
       let files_dir = OpamFilename.Op.(mdir / "files") in
       extra_files o >>| List.map @@ fun (basename, hash) ->
       OpamFilename.create files_dir basename,
       basename, hash)
      +! []
    )

  let print_errors ?file o =
    if o.format_errors <> [] then
      OpamConsole.error "In the opam file%s:\n%s\
                         %s %s been %s."
        (match o.name, o.version, file, o.metadata_dir with
         | Some n, Some v, _, _ ->
           Printf.sprintf " for %s"
             (OpamPackage.to_string (OpamPackage.create n v))
         | _, _, Some f, _ ->
           Printf.sprintf " at %s" (to_string f)
         | _, _, _, Some dir ->
           Printf.sprintf " in %s" (OpamFilename.Dir.to_string dir)
         | _ -> "")
        (OpamStd.Format.itemize
           (fun (_, bf) -> Pp.string_of_bad_format (OpamPp.Bad_format bf))
           o.format_errors)
        (OpamStd.List.concat_map ", " (fun (f,_) -> Printf.sprintf "'%s'" f)
           o.format_errors)
        (match o.format_errors with [_] -> "has" | _ -> "have")
        (OpamConsole.colorise `bold "ignored")
end


(** Optional package.install files (<source>/<pkgname>.install,
    <repo>/packages/.../files/<pkgname>.install) *)

module Dot_installSyntax = struct

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
    lib_root: (basename optional * basename option) list;
    libexec_root: (basename optional * basename option) list;
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
    lib_root = [];
    libexec_root = [];
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
  let raw_man t = t.man
  let doc t = t.doc
  let libexec t = t.libexec
  let lib_root t = t.lib_root
  let libexec_root t = t.libexec_root

  let with_bin bin t = { t with bin }
  let with_sbin sbin t = { t with sbin }
  let with_lib lib t = { t with lib }
  let with_toplevel toplevel t = { t with toplevel }
  let with_stublibs stublibs t = { t with stublibs }
  let with_misc misc t = { t with misc }
  let with_share share t = { t with share }
  let with_share_root share_root t = { t with share_root }
  let with_etc etc t = { t with etc }
  let with_man man t = { t with man }
  let with_doc doc t = { t with doc }
  let with_libexec libexec t = { t with libexec }
  let with_lib_root lib_root t = { t with lib_root }
  let with_libexec_root libexec_root t = { t with libexec_root }

  let add_man_section_dir src =
    let file = Filename.basename (OpamFilename.Base.to_string src.c) in
    let section =
      let base =
        if Filename.check_suffix file ".gz"
        then Filename.chop_suffix file ".gz" else file
      in
      let dot = String.rindex base '.' in
      if dot < String.length base - 1 then
        match base.[dot+1] with
        | '1'..'8' as c -> Some (Printf.sprintf "man%c" c)
        | _ -> None
      else None
    in
    OpamStd.Option.Op.(
      section >>|
      (fun s -> Filename.concat s file) >>|
      OpamFilename.Base.of_string
    )

  let man t =
    List.map (fun (src, dst) ->
        src,
        match dst with
        | Some _ -> dst
        | None -> add_man_section_dir src
      ) t.man

  (* Filenames starting by ? are not always present. *)
  let pp_optional =
    Pp.pp ~name:"file-name"
      (fun ~pos:_ str ->
         let mk = OpamFilename.Base.of_string in
         if String.length str > 0 && str.[0] = '?' then
           { optional = true;
             c        = mk (String.sub str 1 (String.length str - 1)) }
         else
           { optional = false;
             c        = mk str })
      (fun op ->
         if op.optional then "?" ^ OpamFilename.Base.to_string op.c
         else OpamFilename.Base.to_string op.c)

  let fields =
    let pp_field =
      Pp.V.map_list ~depth:1 @@ Pp.V.map_option
        (Pp.V.string -| pp_optional)
        (Pp.opt @@
         Pp.singleton -| Pp.V.string -|
         Pp.of_module "rel-filename" (module OpamFilename.Base))
    in
    let pp_misc =
      Pp.V.map_list ~depth:1 @@ Pp.V.map_option
        (Pp.V.string -| pp_optional)
        (Pp.singleton -| Pp.V.string -| Pp.pp ~name:"abs-filename"
           (fun ~pos s ->
              if not (Filename.is_relative s) then OpamFilename.of_string s
              else Pp.bad_format ~pos
                  "%s is not an absolute filename." s)
           OpamFilename.to_string)
    in
    [
      "lib", Pp.ppacc with_lib lib pp_field;
      "bin", Pp.ppacc with_bin bin pp_field;
      "sbin", Pp.ppacc with_sbin sbin pp_field;
      "misc", Pp.ppacc with_misc misc pp_misc;
      "toplevel", Pp.ppacc with_toplevel toplevel pp_field;
      "stublibs", Pp.ppacc with_stublibs stublibs pp_field;
      "share", Pp.ppacc with_share share pp_field;
      "share_root", Pp.ppacc with_share_root share_root pp_field;
      "etc", Pp.ppacc with_etc etc pp_field;
      "doc", Pp.ppacc with_doc doc pp_field;
      "man", Pp.ppacc with_man raw_man pp_field;
      "libexec", Pp.ppacc with_libexec libexec pp_field;
      "lib_root", Pp.ppacc with_lib_root lib_root pp_field;
      "libexec_root", Pp.ppacc with_libexec_root libexec_root pp_field;
    ]

  let pp =
    let name = internal in
    Pp.I.map_file @@
    Pp.I.check_opam_version ~optional:true () -|
    Pp.I.fields ~name ~empty fields -|
    Pp.I.show_errors ~name () -|
    Pp.check ~errmsg:"man file without destination or recognised suffix"
      (fun t ->
         List.for_all (function
             | m, None -> add_man_section_dir m <> None
             | _, Some _ -> true)
           t.man)

end
module Dot_install = struct
  include Dot_installSyntax
  include SyntaxFile(Dot_installSyntax)
end


module ChangesSyntax = struct
  let internal = "changes"

  open OpamDirTrack

  type t = OpamDirTrack.t

  module SM = OpamStd.String.Map

  let empty = SM.empty

  let field kind get_kind =
    Pp.ppacc
      (fun files t ->
         List.fold_left (fun t (f,digest) -> SM.add f (kind digest) t) t files)
      (fun t ->
         SM.fold (fun f op acc ->
             match get_kind op with Some dg -> (f, dg) :: acc | None -> acc)
           t []
         |> List.rev)
      (Pp.V.map_list ~depth:1 @@
       Pp.V.map_option
         Pp.V.string
         (Pp.opt (Pp.singleton -| Pp.V.string -|
                  Pp.of_pair "digest" (digest_of_string, string_of_digest))))

  let fields = [
    "added", field
      (function Some dg -> Added dg
              | None -> Pp.bad_format "Missing digest")
      (function Added dg -> Some (Some dg) | _ -> None);
    "removed", field
      (function Some _ -> Pp.bad_format "Extra digest"
              | None -> Removed)
      (function Removed -> Some None | _ -> None);
    "contents-changed", field
      (function Some dg -> Contents_changed dg
              | None -> Pp.bad_format "Missing digest")
      (function Contents_changed dg -> Some (Some dg) | _ -> None);
    "perm-changed", field
      (function Some dg -> Perm_changed dg
              | None -> Pp.bad_format "Missing digest")
      (function Perm_changed dg -> Some (Some dg) | _ -> None);
    "kind-changed", field
      (function Some dg -> Kind_changed dg
              | None -> Pp.bad_format "Missing digest")
      (function Kind_changed dg -> Some (Some dg) | _ -> None);
  ]

  let pp_contents =
    Pp.I.fields ~name:internal ~empty fields -|
    Pp.I.show_errors ~name:internal ()

  let pp = Pp.I.map_file pp_contents
end

module Changes = struct
  type t = OpamDirTrack.t
  include SyntaxFile(ChangesSyntax)
end

module SwitchExportSyntax = struct

  let internal = "switch-export"

  type t = {
    selections: switch_selections;
    overlays: OPAM.t OpamPackage.Name.Map.t;
  }

  let empty = {
    selections = SwitchSelectionsSyntax.empty;
    overlays = OpamPackage.Name.Map.empty;
  }

  let fields = SwitchSelectionsSyntax.fields

  let pp =
    let name = "export-file" in
    Pp.I.map_file @@
    Pp.I.check_opam_version () -|
    Pp.I.partition (function
        | Section (_, { section_kind="package"; section_name=Some _; _ }) ->
          false
        | _ -> true) -|
    Pp.map_pair
      (Pp.I.fields ~name
         ~empty:SwitchSelectionsSyntax.empty fields -|
       Pp.I.show_errors ~name ())
      (Pp.map_list
         (Pp.I.section "package" -|
          Pp.map_pair
            (Pp.map_option
               (Pp.of_module "package-name" (module OpamPackage.Name)))
            OPAMSyntax.pp_raw_fields -|
          Pp.pp
            (fun ~pos:_ (name, opam) ->
               match name with
               | Some name -> name, OPAM.with_name name opam
               | None -> OPAM.name opam, opam)
            (fun (name, opam) ->
               Some name, OPAM.with_name_opt None opam)) -|
       Pp.of_pair "package-metadata-map"
         OpamPackage.Name.Map.(of_list,bindings)) -|
    Pp.pp
      (fun ~pos:_ (selections, overlays) -> {selections; overlays})
      (fun {selections; overlays} -> (selections, overlays))

end

module SwitchExport = struct
  type t = SwitchExportSyntax.t = {
    selections: switch_selections;
    overlays: OPAM.t OpamPackage.Name.Map.t;
  }

  include SyntaxFile(SwitchExportSyntax)
end


module CompSyntax = struct

  let internal = "comp"

  type compiler = string
  type compiler_version = string

  type t = {
    opam_version : opam_version ;
    name         : compiler ;
    version      : compiler_version ;
    preinstalled : bool;
    src          : url option ;
    patches      : url list ;
    configure    : string list ;
    make         : string list ;
    build        : command list ;
    packages     : formula ;
    env          : env_update list;
    tags         : string list;
  }

  let empty = {
    opam_version = OpamVersion.current;
    name         = "<none>";
    version      = "<none>";
    src          = None;
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

  let name (t:t) = t.name
  let version (t:t) = t.version
  let patches t = t.patches
  let configure t = t.configure
  let make t = t.make
  let build t = t.build
  let src t = t.src
  let opam_version t = t.opam_version

  let packages t = t.packages
  let preinstalled t = t.preinstalled
  let env (t:t) =
    List.map (function
        | var,op,value,None ->
          var, op, value,
          Some ("Updated by compiler " ^ t.name)
        | b -> b)
      t.env

  let tags t = t.tags

  let with_opam_version opam_version t = {t with opam_version}
  let with_name name (t:t) = {t with name}
  let with_version version (t:t) = {t with version}
  let with_src src t = { t with src }
  let with_patches patches t = {t with patches}
  let with_configure configure t = {t with configure}
  let with_make make t = {t with make}
  let with_build build t = {t with build}
  let with_packages packages t = {t with packages}
  let with_preinstalled preinstalled t = {t with preinstalled}
  let with_env env t = {t with env}
  let with_tags tags t = {t with tags}

  let fields =
    let with_src url t =
      if t.src <> None then Pp.bad_format "Too many URLS"
      else with_src (Some url) t
    in
    [
      "opam-version", Pp.ppacc with_opam_version opam_version
        (Pp.V.string -| Pp.of_module "opam-version" (module OpamVersion));
      "name", Pp.ppacc_opt with_name
        (fun t -> if t.name = empty.name then None else Some t.name)
        Pp.V.string;
      "version", Pp.ppacc_opt with_version
        (fun t -> if t.version = empty.version then None else Some t.version)
        Pp.V.string;

      "src", Pp.ppacc_opt with_src src
        Pp.V.url;
      "http", Pp.ppacc_opt with_src OpamStd.Option.none
        (Pp.V.url_with_backend `http);
      "archive", Pp.ppacc_opt with_src OpamStd.Option.none
        (Pp.V.url_with_backend `http);
      "git", Pp.ppacc_opt with_src OpamStd.Option.none
        (Pp.V.url_with_backend `git);
      "darcs", Pp.ppacc_opt with_src OpamStd.Option.none
        (Pp.V.url_with_backend `darcs);
      "hg", Pp.ppacc_opt with_src OpamStd.Option.none
        (Pp.V.url_with_backend `hg);
      "local", Pp.ppacc_opt with_src OpamStd.Option.none
        (Pp.V.url_with_backend `rsync);


      "patches", Pp.ppacc with_patches patches
        (Pp.V.map_list ~depth:1 @@ Pp.V.url);

      "configure", Pp.ppacc with_configure configure
        (Pp.V.map_list ~depth:1 Pp.V.string);
      "make", Pp.ppacc with_make make
        (Pp.V.map_list ~depth:1 Pp.V.string);
      "build", Pp.ppacc with_build build
        (Pp.V.map_list ~depth:1 Pp.V.command);

      "packages", Pp.ppacc with_packages packages
        (Pp.V.package_formula `Conj (Pp.V.constraints Pp.V.version));
      "env", Pp.ppacc with_env env
        (Pp.V.map_list ~depth:2 Pp.V.env_binding);
      "preinstalled", Pp.ppacc_opt with_preinstalled
        (fun t -> if t.preinstalled then Some true else None)
        Pp.V.bool;
      "tags", Pp.ppacc with_tags tags
        (Pp.V.map_list ~depth:1 Pp.V.string);
    ]

  let system_compiler = "system"

  let version_of_name name =
    match OpamStd.String.cut_at name '+' with
    | Some (v,_) -> v
    | None -> name

  let pp_raw =
    let name = internal in
    Pp.I.map_file @@
    Pp.I.check_opam_version () -|
    Pp.I.fields ~name ~empty fields -|
    Pp.I.show_errors ~name () -|
    Pp.check ~errmsg:"fields 'build:' and 'configure:'+'make:' are mutually \
                      exclusive "
      (fun t -> t.build = [] || t.configure = [] && t.make = [])

  let of_filename f =
    if OpamFilename.check_suffix f ".comp" then
      f
      |> OpamFilename.chop_extension
      |> OpamFilename.basename
      |> OpamFilename.Base.to_string
      |> fun x -> Some x
    else
      None

  let pp =
    pp_raw -|
    Pp.pp
      (fun ~pos (filename, (t:t)) ->
         filename, match of_filename filename with
         | None ->
           if t.name = empty.name ||
              t.name <> "system" && t.version = empty.version
           then
             Pp.bad_format ~pos
               "File name not in the form <name>.<version>, and missing 'name:' \
                or 'version:' fields"
           else
             Pp.warn ~pos
               ".comp file name not in the form <name>.<version>";
           t
         | Some name ->
           let version =
             if name = "system" then t.version
             else version_of_name name
           in
           if t.name <> empty.name && t.name <> name then
             Pp.warn ~pos "Mismatching file name and 'name:' field";
           if name <> system_compiler &&
              t.version <> empty.version && t.version <> version then
             Pp.warn ~pos "Mismatching file name and 'version:' field";
           {t with name; version})
      (fun (filename, t) ->
         filename, match of_filename filename with
         | None ->
           if t.name = empty.name ||
              t.name <> system_compiler && t.version = empty.version
           then
             OpamConsole.warning
               "Outputting .comp file %s with unspecified name or version"
               (OpamFilename.to_string filename);
           t
         | Some name ->
           let version =
             if name = system_compiler then t.version
             else version_of_name name
           in
           if t.name <> empty.name && t.name <> name ||
              name <> system_compiler &&
              t.version <> empty.version && t.version <> version
           then
             OpamConsole.warning
               "Skipping inconsistent 'name:' or 'version:' fields (%s.%s) \
                while saving %s"
               t.name version (OpamFilename.to_string filename);
           { t with name = empty.name })

  let to_package ?package comp descr_opt =
    let package = match package with
      | Some p -> p
      | None ->
        OpamPackage.create
          (OpamPackage.Name.of_string "ocaml")
          (OpamPackage.Version.of_string (name comp))
    in
    let nofilter x = x, (None: filter option) in
    let depends =
      OpamFormula.map (fun (n, formula) ->
          let cstr (op, v) =
            OpamFormula.ands [
              Atom (Constraint (op, FString (OpamPackage.Version.to_string v)));
            ]
          in
          let post_flag =
            Filter (FIdent ([], OpamVariable.of_string "post", None))
          in
          Atom (n, OpamFormula.ands
                  [OpamFormula.map cstr formula; Atom post_flag]))
        (OpamFormula.ands [
            Atom (OpamPackage.Name.of_string "ocaml",
                  Atom (`Eq, OpamPackage.Version.of_string comp.version));
            comp.packages
          ])
    in
    let url =
      OpamStd.Option.map
        (fun url -> URL.with_url url URL.empty)
        comp.src
    in
    let build, install =
      match comp.build with
      | [] ->
        List.map (fun l -> nofilter (List.map nofilter l)) [
          (List.map (fun s -> CString s) ("./configure" :: configure comp ))
          @ [ CString "-prefix"; CIdent "prefix" ];
          CIdent "make" :: List.map (fun s -> CString s) (make comp);
        ],
        List.map (fun l -> nofilter (List.map nofilter l)) [
          [ CIdent "make"; CString "install" ];
        ]
      | cl ->
        match List.rev cl with
        | install::cl -> List.rev cl, [install]
        | [] -> assert false
    in
    let extra_sources =
      List.map (fun url ->
          OpamFilename.Base.of_string (OpamUrl.basename url),
          URL.create url)
        comp.patches
    in
    let patches =
      List.map
        (fun u -> nofilter (OpamFilename.Base.of_string (OpamUrl.basename u)))
        comp.patches
    in
    let pkg = OPAM.create package in
    { pkg with
      OPAM.
      depends;
      build;
      install;
      maintainer = [ "platform@lists.ocaml.org" ];
      extra_sources;
      patches;
      env = comp.env;
      flags = [Pkgflag_Compiler];
      url;
      descr = descr_opt;
    }

end
module Comp = struct
  include CompSyntax
  include SyntaxFile(CompSyntax)
end
