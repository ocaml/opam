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

module Git = struct

  let exec repo command =
    OpamFilename.in_dir repo (fun () ->
        OpamSystem.command command
      )

  let return_one_line repo command =
    OpamFilename.in_dir repo (fun () ->
        List.hd (OpamSystem.read_command_output command)
      )

  let return repo command =
    OpamFilename.in_dir repo (fun () ->
        (OpamSystem.read_command_output command)
      )

  let commit repo fmt =
    Printf.kprintf (fun msg ->
        exec repo [ "git"; "commit"; "-a"; "-m"; msg; "--allow-empty" ]
      ) fmt

  let commit_file repo file fmt =
    Printf.kprintf (fun msg ->
        if OpamFilename.exists file then
          let file = OpamFilename.remove_prefix repo file in
          exec repo [ "git"; "add"; file ];
          exec repo [ "git"; "commit"; "-m"; msg; file; "--allow-empty" ];
        else
          OpamConsole.error_and_exit "Cannot commit %s" (OpamFilename.to_string file);
      ) fmt

  let revision repo =
    return_one_line repo [ "git"; "rev-parse"; "HEAD" ]

  let commits repo =
    return repo ["git"; "log"; "master"; "--pretty=format:%H"]

  let init repo =
    exec repo ["git"; "init"]

  let test_tag = "test"

  let branch repo =
    exec repo ["git"; "checkout"; "-B"; test_tag]

  let add repo file =
    if OpamFilename.exists file then
      let file = OpamFilename.remove_prefix repo file in
      exec repo ["git"; "add"; file]

  let checkout repo hash =
    exec repo ["git"; "checkout"; hash];
    exec repo ["git"; "clean"; "-fdx"]

  let msg repo commit package fmt =
    Printf.kprintf (fun str ->
        OpamConsole.msg "%-25s %s     %-10s %-30s\n"
          (OpamFilename.Dir.to_string repo)
          commit
          (OpamPackage.to_string package)
          str
      ) fmt

  let date repo commit =
    let r = return_one_line repo [ "git"; "show"; "-s"; "--format=\"%ct\""; commit ] in
    let r = OpamStd.String.strip r in
    let r = String.sub r 1 (String.length r - 2) in
    float_of_string r

  let files repo commit dir =
    return repo [ "git"; "ls-tree"; commit; dir ^ "/"; "--name-only"; "-r" ]

  let authors repo commit =
    return repo ["git"; "shortlog"; "-sne"; "--no-merges"; commit ]

end

type stats = {
  commit  : string;
  date    : float;
  authors : string list;
  packages: package_set;
  names   : name_set;
}

let compare_stats s1 s2 =
  int_of_float (s1.date -. s2.date)

let rec filter_monotone_loop s0 ge = function
  | [] -> []
  | s :: tl -> if ge s0 s then s :: filter_monotone_loop s ge tl
              else filter_monotone_loop s0 ge tl

let filter_monotone ge = function
  | [] -> []
  | s0 :: tl -> s0 :: filter_monotone_loop s0 ge tl

let stats repo =
  let commits = Git.commits repo in
  let n = List.length commits in
  Printf.printf "Commits: %d\n%!" n;
  let c = ref 0 in
  let stats = List.map (fun commit ->
      Printf.printf "\r%d / %d%!" !c n; incr c;
      let date = Git.date repo commit in
      let files = Git.files repo commit "packages" in
      let authors = Git.authors repo commit in
      let packages = List.fold_left (fun packages f ->
          if Filename.basename f <> "opam" then packages else
          match OpamPackage.of_string_opt (Filename.basename (Filename.dirname f))
          with
          | None    -> packages
          | Some nv -> OpamPackage.Set.add nv packages
        ) OpamPackage.Set.empty files in
      let names = OpamPackage.Set.fold (fun nv names ->
          OpamPackage.Name.Set.add (OpamPackage.name nv) names
        ) packages OpamPackage.Name.Set.empty in
      { commit; date; authors; packages; names }
    ) commits in
  Printf.printf "\n";
  let stats = List.sort compare_stats stats in
  (* Drop the initial zero values. *)
  let stats =
    List.filter (fun s -> not(OpamPackage.Set.is_empty s.packages)) stats in
  (* Because of merges, the number of contributors or packages is not
     monotone.  Filter the list so it is. *)
  let ge s1 s2 = List.length s1.authors <= List.length s2.authors
                 && OpamPackage.Set.cardinal s1.packages
                   <= OpamPackage.Set.cardinal s2.packages in
  filter_monotone ge stats

let display stats fn ylabel output =
  let dotfile = output ^ ".dot" in
  let oc = open_out dotfile in
  Printf.fprintf oc
    "#!/usr/bin/gnuplot\n\
     set xdata time\n\
     set timefmt \"%%s\"\n\
     set format x \"%%m/%%Y\"\n\
     set term png size 800,400 font \"Arial,10\"\n\
     set output \"%s.png\"\n\
     unset key\n\
     set style data lines\n\
     set style fill transparent solid 0.4\n\
     set grid\n\
     set xlabel 'Time'\n\
     set ylabel '%s'\n\
     set datafile separator \";\"\n\
     plot '-' using 1:($2) with filledcurves below x1 lt rgb 'dark-blue' lw 3\n"
    output ylabel;
  List.iter (fun stats ->
      Printf.fprintf oc "%.0f;%d\n" stats.date (fn stats)
    ) stats;
  Printf.fprintf oc "e";
  close_out oc;
  Printf.printf "Generating %s.png ...\n" output;
  OpamSystem.command ["gnuplot"; dotfile ]

let process () =
  let stats = stats (OpamFilename.cwd ()) in
  List.iter (fun (fn, ylabel, output) ->
      display stats fn ylabel output
    ) [
    ((fun s -> List.length s.authors),
     "Contributors", "contributors");

    ((fun s -> OpamPackage.Set.cardinal s.packages),
     "Packages", "packages");

    ((fun s -> OpamPackage.Name.Set.cardinal s.names),
     "Unique Packages", "unique-packages");
  ]
