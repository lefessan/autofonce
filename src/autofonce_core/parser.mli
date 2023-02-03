(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU General Public    *)
(*  License version 3.0, as described in the LICENSE.md file in the root  *)
(*  directory of this source tree.                                        *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

exception TestsuiteNotFound of string

val name_of_loc : Types.location -> string
val m4_escape : string -> string

(* Read filename to get the corresponding testsuite *)
val read : string -> Types.suite

(* Try to find filename in the upper directories, and read the testsuite *)
val find : string -> Types.suite
