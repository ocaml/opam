(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Functions handling the "opam list" subcommand *)

open OpamParserTypes.FullPos
open OpamTypes
open OpamStateTypes

(** Switches to determine what to include when querying (reverse)
    dependencies *)
type dependency_toggles = {
  recursive: bool;
  depopts: bool;
  build: bool;
  post: bool;
  test: bool;
  dev_setup: bool;
  doc: bool;
  dev: bool;
}

val default_dependency_toggles: dependency_toggles

type pattern_selector = {
  case_sensitive: bool;
  exact: bool;
  glob: bool;
  fields: string list;
  ext_fields: bool; (** Match on raw strings in [x-foo] fields *)
}

val default_pattern_selector: pattern_selector

(** Package selectors used to filter the set of packages *)
type selector =
  | Any
  | Installed
  | Root
  | Compiler
  | Available
  | Installable
  | Pinned
  | Depends_on of dependency_toggles * atom list
  | Required_by of dependency_toggles * atom list
  | Conflicts_with of package list
  | Coinstallable_with of dependency_toggles * package list
  | Solution of dependency_toggles * atom list
  | Pattern of pattern_selector * string
  | Atoms of atom list
  | Flag of package_flag
  | Tag of string
  | From_repository of repository_name list
  | Owns_file of filename

(** Applies a formula of selectors to filter the package from a given switch
    state *)
val filter:
  base:package_set -> 'a switch_state ->
  selector OpamFormula.formula -> package_set

(** Or-filter on package patterns (NAME or NAME.VERSION) *)
val pattern_selector: string list -> selector OpamFormula.formula

(** Get the aggregated active external dependencies of the given packages *)
val get_depexts: 'a switch_state -> package_set -> OpamSysPkg.Set.t

(** Lists the given aggregated active external dependencies of the given packages *)
val print_depexts: OpamSysPkg.Set.t -> unit

(** Element of package information to be printed. Fixme: should be part of the
    run-time man! *)
type output_format =
  | Name               (** Name without version *)
  | Version            (** Version of the currently looked-at package *)
  | Package            (** [name.version] *)
  | Synopsis           (** One-line package description *)
  | Synopsis_or_target (** Pinning target if pinned, synopsis otherwise *)
  | Description        (** The package description, excluding synopsis *)
  | Field of string    (** The value of the given opam-file field *)
  | Raw_field of string   (** The raw value of the given opam-file field *)
  | Installed_version  (** Installed version or "--" if none *)
  | Pinning_target     (** Empty string if not pinned *)
  | Source_hash        (** The VC-reported ident of current version, for dev
                           packages. Empty if not available *)
  | Raw                (** The full contents of the opam file (reformatted) *)
  | All_installed_versions (** List of the installed versions in all switches
                               with the corresponding switches in brackets *)
  | Available_versions (** List of the available versions (currently installed
                           one in bold if color enabled) *)
  | All_versions       (** List of the existing package versions (installed,
                           installed in current switch and unavailable colored
                           specifically if color enabled) *)
  | Repository         (** The repository the package was found in (may be empty
                           for pinned packages) *)
  | Installed_files    (** The list of files that the installed package added to
                           the system *)
  | VC_ref             (** The version-control branch or tag the package url is
                           bound to, if any *)
  | Depexts            (** The external dependencies *)

val default_list_format: output_format list

(** Gets either the current switch state, if a switch is selected, or a virtual
    state corresponding to the configured repos *)
val get_switch_state: 'a global_state -> 'a repos_state -> unlocked switch_state

(** For documentation, includes a dummy '<field>:' for the [Field] format.
    Used for the --columns argument. *)
val raw_field_names: (output_format * string) list

(** For documentation, includes a dummy '<field>:' and '<field>' for the
    [Field] format. Used for the --field argument. *)
val field_names: (output_format * string) list

val string_of_field: ?raw:bool -> output_format -> string

val field_of_string: raw:bool -> string -> output_format

type package_listing_format = {
  short: bool;
  header: bool;
  columns: output_format list;
  all_versions: bool;
  wrap: [`Wrap of string | `Truncate | `None] option;
  separator: string;
  value_printer: [`Normal | `Pretty | `Normalised];
  order: [`Standard | `Dependency | `Custom of package -> package -> int];
}

val default_package_listing_format: package_listing_format

(** Outputs a list of packages as a table according to the formatting options.
    [normalise] supersedes [prettify] and uses a canonical way of displaying
    package definition file fields. [prettify] uses a nicer to read format for the
    package definition file fields. *)
val display: 'a switch_state -> package_listing_format -> package_set -> unit

(** Display a general summary of a collection of packages. *)
val info:
  'a switch_state ->
  fields:string list -> raw:bool -> where:bool ->
  ?normalise:bool -> ?show_empty:bool -> ?all_versions:bool -> ?sort:bool ->
  atom list -> unit

(** Prints the value of an opam field in a shortened way (with [prettify] -- the
    default -- puts lists of strings in a format that is easier to read *)
val mini_field_printer: ?prettify:bool -> ?normalise:bool -> value -> string

val string_of_formula: selector OpamFormula.formula -> string
