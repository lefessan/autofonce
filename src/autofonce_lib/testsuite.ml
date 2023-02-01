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
open Ezcmd.V2
open EZCMD.TYPES
open Ez_file.V1
open EzFile.OP

open Autofonce_core
open Types
open Globals (* toplevel references *)

let exec c =
  Misc.set_signal_handle Sys.sigint (fun _ -> exit 2);
  Misc.set_signal_handle Sys.sigterm (fun _ -> exit 2);
  if !clean_tests_dir && Sys.file_exists tests_dir then
    Misc.remove_rec tests_dir;
  if not ( Sys.file_exists tests_dir ) then Unix.mkdir tests_dir 0o755;
  Unix.putenv "AUTOTEST_TESTSUITE" c.suite_dir;
  let state = Runner_common.create_state c in
  Runner_common.output "Executing testsuite in %s"
    (state.state_run_dir // tests_dir);

  if !max_jobs = 1 then
    Runner_seq.exec_testsuite state
  else
    Runner_par.exec_testsuite state;

  Terminal.move_bol ();
  Printf.printf "Results:\n%!"; Terminal.erase Eol;
  Terminal.printf [] "* %d checks performed\n%!" state.state_nchecks ;
  let style =
    if state.state_tests_failed <> [] then [ Terminal.red ]
    else [ Terminal.green ]
  in
  Terminal.printf style
    "* %d / %d tests executed successfully\n%!"
    state.state_ntests_ok state.state_ntests_ran ;
  begin match state.state_tests_failed with
    | [] -> ()
    | list ->
        let nb = List.length list in
        Terminal.printf [ Terminal.red ] "* %d tests failed:" nb;
        Runner_common.print_ntests 10 (List.rev list);
        Printf.printf "\n%!";
  end;
  begin match state.state_tests_skipped with
    | [] -> ()
    | list ->
        let nb = List.length list in
        Terminal.printf [ Terminal.magenta ]
          "* %d tests were skipped\n%!" nb;
  end;
  begin match state.state_tests_failexpected with
    | [] -> ()
    | list ->
        let nb = List.length list in
        Terminal.printf [ Terminal.magenta ]
          "* %d tests were expected to fail:%!" nb;
        Runner_common.print_ntests 5 (List.rev list);
        Printf.printf "\n%!";
  end;
  let buffer = Buffer.contents state.state_buffer in
  let buffer_file = tests_dir // "results.log" in
  EzFile.write_file buffer_file buffer;
  Printf.eprintf "File %S created with failure results\n%!" buffer_file;
  if !print_all then
    Terminal.printf [ Terminal.magenta ] "%s\n%!" buffer;
  ()

let print_test _c t =
  Printf.printf "%04d %-50s %s\n%!" t.test_id t.test_name t.test_loc;
  ()

let print c =
  let current_banner = ref "" in
  Filter.select_tests
    (fun t ->
       if t.test_banner <> !current_banner then begin
         Printf.eprintf "\n%s\n\n%!" t.test_banner;
         current_banner := t.test_banner
       end;
       print_test c t;
    ) c

let args = [

  [ "t" ; "testsuite" ], Arg.String (fun s -> testsuite := s),
  EZCMD.info
    ~env:(EZCMD.env "AUTOFONCE_TESTSUITE")
    ~docv:"FILE" "File to lookup (default to 'tests/testsuite.at')";

]
