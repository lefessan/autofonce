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
open Types
open Ez_file.V1
open EzFile.OP
open Globals (* toplevel references *)

let buffer_test c t fmt =
  Printf.kprintf (fun s ->
      Printf.bprintf c.buffer "%04d %-50s %s\n"  t.id t.name s) fmt

let commented s =
  "# " ^ String.concat "\n# " (EzString.split s '\n')

let output fmt =
  Printf.kprintf (fun s ->
      Terminal.move_bol ();
      Terminal.erase Eol;
      Printf.printf "%s\n%!" s) fmt

let output_test t fmt =
  Printf.kprintf (fun s ->
      output "%04d %-50s %s"  t.id t.name s) fmt

let print_ntests n list =
  List.iteri (fun i t ->
      if i < n then Printf.printf " %04d" t.id
      else
      if i = n then Printf.printf " ..."
    ) list

let test_dir t =
  tests_dir // Printf.sprintf "%04d" t.id

let test_is_ok t =
  Misc.remove_rec ( test_dir t ) ;
  let c = t.suite in
  c.ntests_ok <- c.ntests_ok + 1

let test_is_skipped_fail check s =
  let t = check.test in
  let c = t.suite in
  if not !keep_skipped then Misc.remove_rec ( test_dir t ) ;
  c.tests_failexpected <- t :: c.tests_failexpected ;
  buffer_test c t "SKIPPED FAIL (%s %s)" check.check_loc s

let test_is_failed check s =
  let t = check.test in
  let c = t.suite in
  if check.test.fail_expected then begin
    c.tests_failexpected <- t :: c.tests_failexpected ;
    if not !keep_skipped then Misc.remove_rec ( test_dir t ) ;
    buffer_test c t "EXPECTED FAIL (%s %s)" check.check_loc s;
  end else begin
    c.tests_failed <- t :: c.tests_failed ;
    output_test t "FAIL (%s %s)" check.check_loc s;
    buffer_test c t "FAIL (%s %s)" check.check_loc s;
    if !stop_on_first_failure then exit 2;
  end

let test_is_skip t =
  let c = t.suite in
  c.tests_skipped <- t :: c.tests_skipped ;
  if not !keep_skipped then Misc.remove_rec ( test_dir t ) ;
  buffer_test c t "SKIP"

let exec_action_no_check t action =
  match action with
  | AT_DATA { file ; content } ->
      EzFile.write_file ( test_dir t // file ) content
  | AT_CHECK _job -> assert false
  | AT_XFAIL_IF "true" -> t.fail_expected <- true
  | AT_CAPTURE_FILE file ->
      if (Sys.file_exists file) then ()
  (* TODO: save file in log in case of failure *)
  | _ ->
      Printf.kprintf failwith "exec_action: %s not implemented"
        ( string_of_action action )


let start_test t =
  let c = t.suite in
  Terminal.move_bol ();
  Terminal.erase Eol;
  Printf.printf " %d / %d%!" c.ntests_ran c.ntests;
  if c.tests_failed <> [] then begin
    Terminal.printf [ Terminal.red ]
      "  %d failed:%!" (List.length c.tests_failed);
    print_ntests 10 c.tests_failed;
  end;
  c.ntests_ran <- c.ntests_ran + 1;
  let test_dir = tests_dir // Printf.sprintf "%04d" t.id in
  if Sys.file_exists test_dir then
    Misc.remove_all test_dir
  else
    Unix.mkdir test_dir 0o755;

  EzFile.write_file ( test_dir // env_autotest_sh ) @@
  Printf.sprintf {|#!/bin/sh
AUTOTEST_TESTSUITE="%s"
%s
|} c.testsuite_dir c.autotest_env;
  Unix.chmod ( test_dir // env_autotest_sh ) 0o755;
  ()

let start_check check =
  let t = check.test in
  let check_prefix = Printf.sprintf "%s_%s" check.step
      (if check.is_check then "CHECK" else "SHELL")
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
      (if check.is_check then commented (string_of_check check) else
         "# SHELL COMMAND")
      check.command
  in
  let test_dir = test_dir t in
  EzFile.write_file ( test_dir // check_sh ) check_content ;
  Unix.chmod (test_dir // check_sh ) 0o755 ;
  Unix.chdir test_dir ;
  let pid = Call.create_process
      [ "./" ^ check_sh ]
      ~stdout:check_stdout
      ~stderr:check_stderr
  in
  let c = t.suite in
  Unix.chdir c.autotest_dir ;
  pid

let check_failures check retcode =
  let t = check.test in
  let test_dir = test_dir t in
  let check_prefix = Printf.sprintf "%s_%s" check.step
      (if check.is_check then "CHECK" else "SHELL")
  in
  let check_stdout = Printf.sprintf "%s.out" check_prefix in
  let check_stderr = Printf.sprintf "%s.err" check_prefix in
  begin match check.retcode with
    | None -> []
    | Some expected ->
        if retcode <> expected then [
          "exitcode" ] else []
  end
  @
  begin match check.stdout with
    | Ignore -> []
    | Content expected ->
        if EzFile.read_file ( test_dir // check_stdout )
           <> expected then begin
          EzFile.write_file ( test_dir // check_stdout ^ ".expected")
            expected ;
          [ "stdout" ]
        end else []
  end
  @
  begin match check.stderr with
    | Ignore -> []
    | Content expected ->
        if EzFile.read_file ( test_dir // check_stderr ) <> expected then
          begin
            EzFile.write_file ( test_dir // check_stderr ^ ".expected")
              expected ;
            [ "stderr" ]
          end
        else []
  end
