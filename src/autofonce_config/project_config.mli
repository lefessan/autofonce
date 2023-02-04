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

type testsuite_config = {
  config_name : string ;
  config_file : string ;
  config_path : string list ;
}

type project_config = {
  project_topdir : string ;
  project_file : string ;
  project_testsuites : testsuite_config list ;
  project_env : string option ;
}

val read : string -> project_config
val write : project_config -> unit
