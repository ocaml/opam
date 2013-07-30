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

open OpamArg

let commands = [
  init;
  list; search; info;
  install; remove; reinstall;
  update; upgrade;
  config;
  remote; repository;
  switch;
  pin;
  help;
]

let () =
  let at_exit () =
    if !OpamGlobals.print_stats then (
      OpamFile.print_stats ();
      OpamState.print_stats ();
    );
    OpamJson.output () in
  run ~at_exit default commands
