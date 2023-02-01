(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

open EzCompat (* for IntMap *)
open Ez_file.V1
open EzFile.OP

open Types
open Globals (* toplevel references *)

open Autofonce_core

let buffer_test ter fmt =
  let t = ter.tester_test in
  let state = ter.tester_state in
  Printf.kprintf (fun s ->
      Printf.bprintf state.state_buffer "%04d %-50s %s\n"
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
      output "%04d %-50s %s"  t.test_id t.test_name s) fmt

let print_ntests n list =
  List.iteri (fun i ter ->
      if i < n then Printf.printf " %04d" ter.tester_test.test_id
      else
      if i = n then Printf.printf " ..."
    ) list

let test_dir ter =
  tests_dir // Printf.sprintf "%04d" ter.tester_test.test_id

let test_is_ok ter =
  if not !keep_all then Misc.remove_rec ( test_dir ter ) ;
  let state = ter.tester_state in
  state.state_ntests_ok <- state.state_ntests_ok + 1

let test_is_skipped_fail cer s =
  let ter = cer.checker_tester in
  let state = ter.tester_state in
  let check = cer.checker_check in
  if not !keep_skipped then Misc.remove_rec ( test_dir ter ) ;
  state.state_tests_failexpected <- ter :: state.state_tests_failexpected ;
  buffer_test ter "SKIPPED FAIL (%s %s)" check.check_loc s

let test_is_failed cer s =
  let ter = cer.checker_tester in
  let state = ter.tester_state in
  let check = cer.checker_check in
  if ter.tester_fail_expected then begin
    state.state_tests_failexpected <- ter :: state.state_tests_failexpected ;
    if not !keep_skipped then Misc.remove_rec ( test_dir ter ) ;
    buffer_test ter "EXPECTED FAIL (%s %s)" check.check_loc s;
  end else begin
    state.state_tests_failed <- ter :: state.state_tests_failed ;
    output_test ter "FAIL (%s %s)" check.check_loc s;
    buffer_test ter "FAIL (%s %s)" check.check_loc s;
    if !stop_on_first_failure then exit 2;
  end

let test_is_skip ter =
  let t = ter.tester_test in
  let state = ter.tester_state in
  state.state_tests_skipped <- ter :: state.state_tests_skipped ;
  if not !keep_skipped then Misc.remove_rec ( test_dir ter ) ;
  buffer_test ter "SKIP (%s)" t.test_loc

let exec_action_no_check ter action =
  match action with
  | AT_DATA { file ; content } ->
      EzFile.write_file ( test_dir ter // file ) content
  | AT_CHECK _job -> assert false
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
      "  %d failed:%!" (List.length state.state_tests_failed);
    print_ntests 10 state.state_tests_failed;
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
  let test_dir = test_dir ter in
  if Sys.file_exists test_dir then
    Misc.remove_all test_dir
  else
    Unix.mkdir test_dir 0o755;

  EzFile.write_file ( test_dir // env_autotest_sh ) @@
  Printf.sprintf {|#!/bin/sh
AUTOTEST_TESTSUITE="%s"
%s
|} c.suite_dir state.state_env;
  Unix.chmod ( test_dir // env_autotest_sh ) 0o755;
  ter

let start_check ter check =
  let state = ter.tester_state in
  let check_prefix = Printf.sprintf "%s_%s" check.check_step check.check_kind
  in
  let check_sh = Printf.sprintf "%s.sh" check_prefix in
  let check_stdout = Printf.sprintf "%s.out" check_prefix in
  let check_stderr = Printf.sprintf "%s.err" check_prefix in
  let check_content =
    Printf.sprintf {|#!/bin/sh

# %s
. ./%s

%s

%s
|}
      autotest_env
      env_autotest_sh
      (commented (string_of_check check))
      check.check_command
  in
  let test_dir = test_dir ter in
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
  let test_dir = test_dir ter in
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
        if retcode <> expected then [
          "exitcode" ] else []
  end
  @
  compare check.check_stdout check_stdout "stdout"
  @
  compare check.check_stderr check_stderr "stderr"

let create_state c =

  let state_run_dir, state_env =
    try
      let autotest_file = Misc.find_file autotest_env in
      let autotest_dir = Filename.dirname autotest_file in
      Unix.chdir autotest_dir;
      let autotest_env = EzFile.read_file autotest_env in
      ( autotest_dir, autotest_env )
    with
    | Not_found ->
        Printf.eprintf "Error: file %S not found in top dirs\n%!" autotest_env;
        Printf.eprintf
          "This project-specific file is used to configure the tests, and\n";
        Printf.eprintf
          " tell autofonce where to create the _autotest/ directory to run\n";
        Printf.eprintf
          " tests.\n";
        Printf.eprintf
          "Use `autofonce init` to automatically create it if you think\n";
        Printf.eprintf
          " that this project is known by `autofonce`.\n";
        exit 2
  in

  { state_suite = c ;
    state_run_dir ;
    state_env ;
    state_status = "";
    state_banner = "" ;
    state_ntests_ran = 0 ;
    state_ntests_ok = 0 ;
    state_tests_failed = [] ;
    state_tests_skipped = [] ;
    state_tests_failexpected = [] ;
    state_buffer = Buffer.create 10000;
    state_ntests = c.suite_ntests ;
    state_nchecks = 0;
  }
