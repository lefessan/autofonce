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

val select_tests :
  ?state:Types.state -> (Types.test -> unit) -> Types.suite -> unit

val only_failed : bool ref

val args :
   (string list * Ezcmd.V2.EZCMD.TYPES.Arg.spec *
    Ezcmd.V2.EZCMD.TYPES.info) list
