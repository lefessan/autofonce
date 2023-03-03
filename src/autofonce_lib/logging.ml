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


open EzCompat (* for IntMap *)
(*
open Ezcmd.V2
open EZCMD.TYPES
*)
open Ez_file.V1
open EzFile.OP

(* module MISC = Autofonce_misc.Misc *)
module PARSER = Autofonce_core.Parser
(* module CONFIG = Autofonce_config.Project_config *)

open Types

let output = ref None (* full path to results.log *)

let indents =  Array.init 10 (fun i -> String.make i ' ')

let log_header ?(indent=0) state fmt =
  let b = state.state_buffer in
  let indent = indents.( indent ) in
  Printf.kprintf (fun s ->
      Printf.bprintf b "\n%s#######################################\n" indent;
      Printf.bprintf b    "#\n%s#          %50s\n%s#\n" indent s indent;
      Printf.bprintf b "%s#######################################\n\n" indent;
    ) fmt

let log_captured_files ?indent ?dir state msg files =
  let b = state.state_buffer in
  let p = state.state_project in
  let dir = match dir with
    | None -> p.project_source_dir
    | Some dir -> dir in
  List.iter (fun file ->
      log_header ?indent state "%s: captured file %S" msg file ;
      let filename = dir // file in
      match EzFile.read_file filename with
      | exception exn ->
          Printf.bprintf b "Exception while reading %S:\n  %s\n"
            filename ( Printexc.to_string exn )
      | file ->
          Printf.bprintf b "\n```\n%s```\n" file
    ) files

let log_failed_tests state msg tests =
  List.iter (fun ter ->
      let t = ter.tester_test in
      let test_dir = Runner_common.tester_dir ter in
      log_header state "%s %04d %s (%s %s)"
        msg t.test_id t.test_name
        (PARSER.name_of_loc t.test_loc)
        (match ter.tester_fail_reason with
         | None -> assert false
         | Some (_loc, reason) -> reason);

      (* TODO : show internal information on failed tests *)

      log_captured_files
        ~indent:3
        ~dir:test_dir
        state
        (Printf.sprintf "Test %04d" t.test_id)
        (StringSet.to_list ter.tester_captured_files);
      ()
    ) tests ;
  () (* TODO *)

let log_state_buffer state =
  let p = state.state_project in
  log_failed_tests state "Failure"
    ( List.rev state.state_tests_failed ) ;
  log_failed_tests state "Expected Failure"
    ( List.rev state.state_tests_failed ) ;
  log_captured_files state "Project" p.project_captured_files;

  let buffer_file = match !output with
    | None ->
        let tests_dir = Autofonce_config.Globals.tests_dir in
        Sys.getcwd () // tests_dir // "results.log"
    | Some output -> output
  in
  let buffer = Buffer.contents state.state_buffer in
  EzFile.write_file buffer_file buffer;
  Printf.eprintf "File %S created with failure results\n%!" buffer_file;
