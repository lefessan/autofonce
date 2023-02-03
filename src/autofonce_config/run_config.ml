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

open Ez_file.V1
open EzFile.OP

module Misc = Autofonce_misc.Misc

(* read "autofonce.toml" colocated with "_autofonce/" *)

type run_config = {
  run_topdir : string ;
  run_file : string ;
  run_build_dir : string option ;
  run_env : string option ;
}

let read filename =
  assert (not @@ Filename.is_relative filename);
  let run_file = Filename.basename filename in
  let run_topdir = Filename.dirname filename in
  let table =
    match EzToml.from_file filename with
    | `Ok table -> table
    | `Error (s, loc) ->
        Misc.error
          "Could not parse run config %S: %s at %s" filename s
          (EzToml.string_of_location loc)
  in
  let run_env = EzToml.get_string_option table
      [ "run" ; "env" ] in
  let run_build_dir = EzToml.get_string_option table
      [ "build" ; "dir" ] in
  {
    run_topdir ;
    run_file ;
    run_build_dir ;
    run_env ;
  }

let to_string p =
  let b = Buffer.create 10000 in


  Printf.bprintf b "[build]\n";
  Buffer.add_string b "# set AUTOFONCE_BUILD_DIR to this value in scripts\n";
  begin
    match p.run_build_dir with
    | None ->
        Printf.bprintf b "# dir = %S\n" "..."
    | Some path ->
        Printf.bprintf b "dir = %S\n" path
  end;
  Buffer.add_char b '\n';

  Printf.bprintf b "[run]\n";
  Buffer.add_string b "# add this content to every test header\n";
  begin
    match p.run_build_dir with
    | None ->
        Buffer.add_string b {|# env = """..."""|}
    | Some path ->
        Printf.bprintf b {|env = """%s"""|} path
  end;
  Buffer.add_char b '\n';

  Buffer.contents b

let write p =
  let filename = p.run_topdir // p.run_file in
  EzFile.write_file filename ( to_string p )
