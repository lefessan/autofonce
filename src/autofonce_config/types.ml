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

type env_kind =
  | Env_file of string
  | Env_content

type testsuite_env = {
  env_name : string ;
  env_kind : env_kind ;
  env_content : string ;
}

type testsuite_config = {
  config_name : string ;
  config_file : string ;
  config_path : string list ;
  config_env : testsuite_env ; (* name of env in file, actually *)
}

type project_config = {
  project_name : string option ;
  project_source_anchors : string list ;
  project_build_anchors : string list ;
  project_testsuites : testsuite_config list ;
  project_envs : testsuite_env EzCompat.StringMap.t ;

  (* computed *)
  project_file : string ;
  project_source_dir : string ;
  project_build_dir : string ;
}
