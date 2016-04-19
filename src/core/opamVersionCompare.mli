(******************************************************************************)
(*  This file is part of the Dose library http://www.irill.org/software/dose  *)
(*                                                                            *)
(*  Copyright (C) 2009-2011 Pietro Abate <pietro.abate@pps.jussieu.fr>        *)
(*                                                                            *)
(*  This library is free software: you can redistribute it and/or modify      *)
(*  it under the terms of the GNU Lesser General Public License as            *)
(*  published by the Free Software Foundation, either version 3 of the        *)
(*  License, or (at your option) any later version.  A special linking        *)
(*  exception to the GNU Lesser General Public License applies to this        *)
(*  library, see the COPYING file for more information.                       *)
(*                                                                            *)
(*  Work developed with the support of the Mancoosi Project                   *)
(*  http://www.mancoosi.org                                                   *)
(*                                                                            *)
(******************************************************************************)

(** Version comparison function used throughout. From the Dose suite. *)

(** Functions for manipulating and comparing Debian version strings.
    Compliant with Debian policy version 3.9.2. and Debian developers
    reference version 3.4.6 *)

(** {2 Comparing debian version strings} *)

(** The following functions compare any two strings, that is these
    functions do not check whether the arguments are really legal
    debian versions. If the arguments are debian version strings, then
    the result is as required by debian policy. Note that two strings
    may be equivalent, that is denote the same debian version, even
    when they differ in syntax, as for instance "0:1.2.00" and
    "1.02-0".
*)

(** @return [true] iff the two strings define the same version. Hence,
    the result may be true even when the two string differ
    syntactically. *)
val equal : string -> string -> bool

(** [compare x y] returns 0 if x is eqivalent to y, -1 if x is smaller
    than y, and 1 if x is greater than y. This is consistent with
    [Pervasives.compare]. *)
val compare : string -> string -> int
