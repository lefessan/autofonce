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
open Ez_file.V1
open EzFile.OP

open Types
open Globals (* toplevel references *)

module Parser = Autofonce_core.Parser
module Misc = Autofonce_misc.Misc
module Project_config = Autofonce_config.Project_config

let buffer_test ter fmt =
  let t = ter.tester_test in
  let state = ter.tester_state in
  Printf.kprintf (fun s ->
      Printf.bprintf state.state_buffer "%04d %-45s %s\n"
        t.test_id t.test_name s) fmt

let commented s =
  "# " ^ String.concat "\n# " (EzString.split s '\n')

let output fmt =
  Printf.kprintf (fun s ->
      Terminal.move_bol ();
      Terminal.erase Eol;
      Printf.printf "%s\n%!" s) fmt

let output_test ter fmt =
  let t = ter.tester_test in
  Printf.kprintf (fun s ->
      output "%04d %-45s %s"  t.test_id t.test_name s) fmt

let print_ntests n list =
  List.iteri (fun i ter ->
      if i < n then Printf.printf " %04d" ter.tester_test.test_id
      else
      if i = n then Printf.printf ".."
    ) list

let test_dir t =
  Autofonce_config.Globals.tests_dir // Printf.sprintf "%04d" t.test_id
let tester_dir ter = test_dir ter.tester_test

let test_is_ok ter =
  if not !keep_all then Misc.remove_rec ( tester_dir ter ) ;
  let state = ter.tester_state in
  state.state_ntests_ok <- state.state_ntests_ok + 1

let test_is_skipped_fail cer s =
  let ter = cer.checker_tester in
  let state = ter.tester_state in
  let check = cer.checker_check in
  if not !keep_skipped then Misc.remove_rec ( tester_dir ter ) ;
  state.state_tests_failexpected <- ter :: state.state_tests_failexpected ;
  buffer_test ter "SKIPPED FAIL (%s %s)"
    ( Parser.name_of_loc check.check_loc ) s

let test_is_failed cer s =
  let ter = cer.checker_tester in
  let state = ter.tester_state in
  let check = cer.checker_check in
  if ter.tester_fail_expected then begin
    state.state_tests_failexpected <- ter :: state.state_tests_failexpected ;
    if not !keep_skipped then Misc.remove_rec ( tester_dir ter ) ;
    buffer_test ter "EXPECTED FAIL (%s %s)"
      ( Parser.name_of_loc check.check_loc ) s;
  end else begin
    state.state_tests_failed <- ter :: state.state_tests_failed ;
    output_test ter "FAIL (%s %s)" ( Parser.name_of_loc check.check_loc ) s;
    buffer_test ter "FAIL (%s %s)" ( Parser.name_of_loc check.check_loc ) s;
    if !stop_on_first_failure then exit 2;
  end

let test_is_skip ter =
  let t = ter.tester_test in
  let state = ter.tester_state in
  state.state_tests_skipped <- ter :: state.state_tests_skipped ;
  if not !keep_skipped then Misc.remove_rec ( tester_dir ter ) ;
  buffer_test ter "SKIP (%s)" ( Parser.name_of_loc t.test_loc )

let exec_action_no_check ter action =
  match action with
  | AT_DATA { file ; content } ->
      EzFile.write_file ( tester_dir ter // file ) content
  | AT_CHECK _job -> assert false
  | AT_CLEANUP _ -> ()
  | AT_XFAIL_IF "true" -> ter.tester_fail_expected <- true
  | AT_CAPTURE_FILE file ->
      if (Sys.file_exists file) then ()
  (* TODO: save file in log in case of failure *)
  | _ ->
      Printf.kprintf failwith "exec_action: %s not implemented"
        ( string_of_action action )

let print_status state =
  Terminal.move_bol ();
  Terminal.erase Eol;
  Printf.printf " %d / %d%!" state.state_ntests_ran state.state_ntests;
  if state.state_tests_failed <> [] then begin
    Terminal.printf [ Terminal.red ]
      " %d failed:%!" (List.length state.state_tests_failed);
    print_ntests 3 state.state_tests_failed;
  end;
  Printf.printf " %s%!" state.state_status;
  ()

let start_test state t =
  let c = state.state_suite in
  state.state_ntests_ran <- state.state_ntests_ran + 1;
  print_status state;
  let ter = {
    tester_test = t ;
    tester_state = state ;
    tester_suite = c ;
    tester_fail_expected = false ;
  }
  in
  let test_dir = tester_dir ter in
  if Sys.file_exists test_dir then
    Misc.remove_all test_dir
  else
    Unix.mkdir test_dir 0o755;

  EzFile.write_file ( test_dir // Autofonce_config.Globals.env_autofonce_sh ) @@
  Printf.sprintf {|#!/bin/sh
# name of testsuite in 'autofonce.toml'
AUTOFONCE_TESTSUITE="%s"
# location of directory containing _autofonce/ dir
AUTOFONCE_RUN_DIR="%s"
# project source directory
AUTOFONCE_SOURCE_DIR="%s"
# build directory of project
AUTOFONCE_BUILD_DIR="%s"

%s
|}
    state.state_config.config_name
    state.state_run_dir
    state.state_project.project_source_dir
    state.state_project.project_build_dir
    state.state_config.config_env.env_content;
  Unix.chmod ( test_dir // Autofonce_config.Globals.env_autofonce_sh ) 0o755;
  ter

let check_dir check = test_dir check.check_test
let check_prefix check =
  Printf.sprintf "%s_%s" check.check_step check.check_kind

let start_check ter check =
  let state = ter.tester_state in
  let check_prefix = check_prefix check in
  let check_sh = Printf.sprintf "%s.sh" check_prefix in
  let check_stdout = Printf.sprintf "%s.out" check_prefix in
  let check_stderr = Printf.sprintf "%s.err" check_prefix in
  let check_content =
    Printf.sprintf {|#!/bin/sh

# create test env
. ./%s

# check to perform
%s

%s
|}
      Autofonce_config.Globals.env_autofonce_sh
      (commented (string_of_check check))
      check.check_command
  in
  let test_dir = tester_dir ter in
  EzFile.write_file ( test_dir // check_sh ) check_content ;
  Unix.chmod (test_dir // check_sh ) 0o755 ;
  Unix.chdir test_dir ;
  let checker_pid = Call.create_process
      [ "./" ^ check_sh ]
      ~stdout:check_stdout
      ~stderr:check_stderr
  in
  Unix.chdir state.state_run_dir ;
 {
    checker_check = check ;
    checker_tester = ter ;
    checker_pid ;
  }

let check_failures cer retcode =
  let check = cer.checker_check in
  let ter = cer.checker_tester in
  let state = ter.tester_state in
  let test_dir = tester_dir ter in
  let check_prefix = Printf.sprintf "%s_%s" check.check_step
      check.check_kind
  in
  if check.check_kind = "CHECK" then
    state.state_nchecks <- state.state_nchecks + 1;
  let check_stdout = Printf.sprintf "%s.out" check_prefix in
  let check_stderr = Printf.sprintf "%s.err" check_prefix in
  let compare to_check file kind =
    match to_check with
    | Ignore -> []
    | Content expected ->
        let stdout_file = test_dir // file in
        if EzFile.read_file stdout_file <> expected then begin
          let stdout_expected = stdout_file ^ ".expected" in
          EzFile.write_file stdout_expected expected ;
          Misc.command_ "diff -u %s %s > %s.diff"
            stdout_file stdout_expected stdout_file;
          [ kind ]
        end else []
  in
  begin match check.check_retcode with
    | None -> []
    | Some expected ->
        if retcode <> expected then begin
          let check_exit = Printf.sprintf "%s.exit" check_prefix in
          let check_exit = test_dir // check_exit in
          EzFile.write_file check_exit (string_of_int retcode);
          EzFile.write_file (check_exit ^ ".expected")
            (string_of_int expected);
          [ "exitcode" ]
        end else []
  end
  @
  compare check.check_stdout check_stdout "stdout"
  @
  compare check.check_stderr check_stderr "stderr"

let create_state state_run_dir p tc suite =
  Unix.chdir state_run_dir;
  { state_suite = suite ;
    state_config = tc ;
    state_project = p ;
    state_run_dir ;
    state_status = "";
    state_banner = "" ;
    state_ntests_ran = 0 ;
    state_ntests_ok = 0 ;
    state_tests_failed = [] ;
    state_tests_skipped = [] ;
    state_tests_failexpected = [] ;
    state_buffer = Buffer.create 10000;
    state_ntests = suite.suite_ntests ;
    state_nchecks = 0;
  }
