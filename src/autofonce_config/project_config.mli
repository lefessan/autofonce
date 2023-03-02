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

val from_file : string -> Types.project_config
val from_string : ?computed:bool -> file:string -> string ->
  Types.project_config

val to_string : Types.project_config -> string
val to_file : Types.project_config -> unit
