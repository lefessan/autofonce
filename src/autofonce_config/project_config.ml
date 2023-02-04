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

(* read .autofonce *)

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



let read filename =
  assert (not @@ Filename.is_relative filename);
  let project_file = Filename.basename filename in
  let project_topdir = Filename.dirname filename in
  let table =
    match EzToml.from_file filename with
    | `Ok table -> table
    | `Error (s, loc) ->
        Misc.error
          "Could not parse project config %S: %s at %s" filename s
          (EzToml.string_of_location loc)
  in

  let project_testsuites =
    let table = EzToml.get_table table [ "testsuites" ] in
    let testsuites = ref [] in
    EzToml.iter
      (fun config_name value ->
         let t =
           match value with
           | EzToml.TYPES.TString file ->
               {
                 config_name ;
                 config_file = file ;
                 config_path = [ Filename.dirname file // "testsuite.src" ];
               }
           | EzToml.TYPES.TTable table ->
               let config_file = EzToml.get_string table [ "file" ] in
               let config_path = EzToml.get_string_list_default
                   table [ "path" ] [] in
               {
                 config_name ;
                 config_file ;
                 config_path ;
               }
           | _ -> assert false
         in
         testsuites := t :: !testsuites )
      table;
    !testsuites
  in
  let project_env = EzToml.get_string_option table
      [ "project" ; "env" ] in
  {
    project_topdir ;
    project_file ;
    project_testsuites ;
    project_env ;
  }

let to_string p =
  let b = Buffer.create 10000 in

  Printf.bprintf b "[project]\n";
  Buffer.add_char b '\n';

  Printf.bprintf b "[testsuites]\n" ;
  Buffer.add_string b {|# alias = "path-from-topdir"|} ;
  Buffer.add_char b '\n' ;
  List.iter (fun t ->
      Printf.bprintf b "[testsuites.%s]\n" t.config_name ;
      Printf.bprintf b "file = %S\n" t.config_file ;
      Printf.bprintf b "path = [%s]\n"
        ( String.concat ", "
            ( List.map (Printf.sprintf " %S") t.config_path) )
    ) p.project_testsuites ;
  Buffer.add_char b '\n';

  Printf.bprintf b "[project]\n";
  Buffer.add_string b "# add this content to every test header\n";
  begin
    match p.project_env with
    | None ->
        Buffer.add_string b {|# env = """..."""|}
    | Some env ->
        Printf.bprintf b {|env = """%s"""|} env
  end;
  Buffer.add_char b '\n';

  Buffer.contents b

let write p =
  let filename = p.project_topdir // p.project_file in
  EzFile.write_file filename ( to_string p )
