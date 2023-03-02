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

val args :
  (string list * Ezcmd.V2.EZCMD.TYPES.Arg.spec * Ezcmd.V2.EZCMD.TYPES.info)
    list

val exec :
  Types.project_config ->
  Types.testsuite_config -> Types.suite -> int

val find :
  unit -> Types.project_config * Types.testsuite_config * Types.suite

val print : Types.suite -> unit

val read :
  Types.project_config -> Types.testsuite_config ->
  Types.project_config * Types.testsuite_config * Types.suite
