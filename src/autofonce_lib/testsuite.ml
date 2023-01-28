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

open Autofonce_m4 (* for M4Types, M4Parser *)
open Types
open Globals (* toplevel references *)

let set_verbosity n =
  Globals.verbose := n;
  Call.debug := !Globals.verbose > 1

let get_verbosity () = !Globals.verbose

let read filename =
  let testsuite_dir = Filename.dirname filename in
  let autotest_dir, autotest_env =
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
  let c = {
    testsuite = "";
    ntests  = 0;
    test_by_id = Hashtbl.create 1000 ;
    tests = [] ;
    tested_programs = [] ;
    copyright = "" ;
    status = "";

    testsuite_dir ;
    autotest_dir ;
    autotest_env ;

    current_banner = "" ;
    ntests_ran = 0 ;
    ntests_ok = 0 ;
    tests_failed = [] ;
    tests_skipped = [] ;
    tests_failexpected = [] ;
    buffer = Buffer.create 10000;
  }
  in
  Parser.load_file c filename;
  c.tests <- List.rev c.tests;
  c

let find () =
  try
    let filename = Misc.find_file !testsuite in
    Printf.eprintf "Found testsuite in %s\n%!" filename;
    let c = read filename in
    Printf.eprintf "Found %d tests in testsuite\n%!" c.ntests;
    c
  with
  | M4Types.Error (msg, loc) ->
      Printf.eprintf "Error at %s: %s\n%!"
        (M4Printer.string_of_location loc) msg;
      exit 2
  | exn ->
      Printf.eprintf "Fatal error: %s\n%!"
        ( Printexc.to_string exn );
      exit 2

let exec c =
  Runner_common.output "Executing testsuite in %s"
    (c.autotest_dir // tests_dir);
  Misc.set_signal_handle Sys.sigint (fun _ -> exit 2);
  Misc.set_signal_handle Sys.sigterm (fun _ -> exit 2);
  if !clean_tests_dir && Sys.file_exists tests_dir then
    Misc.remove_rec tests_dir;
  if not ( Sys.file_exists tests_dir ) then Unix.mkdir tests_dir 0o755;
  Unix.putenv "AUTOTEST_TESTSUITE" c.testsuite_dir;
  c.current_banner <- "";
  c.ntests_ran <- 0 ;
  c.ntests_ok <- 0 ;
  c.tests_failed <- [];
  c.tests_skipped <- [];
  c.tests_failexpected <- [];
  Buffer.clear c.buffer;

  if !max_jobs = 1 then
    Runner_seq.exec_testsuite c
  else
    Runner_par.exec_testsuite c;

  Terminal.move_bol ();
  Printf.printf "Results:\n%!"; Terminal.erase Eol;
  let style =
    if c.tests_failed <> [] then [ Terminal.red ]
    else [ Terminal.green ]
  in
  Terminal.printf style
    "* %d / %d tests executed successfully\n%!"
    c.ntests_ok c.ntests_ran ;
  begin match c.tests_failed with
    | [] -> ()
    | list ->
        let nb = List.length list in
        Terminal.printf [ Terminal.red ] "* %d tests failed:" nb;
        Runner_common.print_ntests 10 (List.rev list);
        Printf.printf "\n%!";
  end;
  begin match c.tests_skipped with
    | [] -> ()
    | list ->
        let nb = List.length list in
        Terminal.printf [ Terminal.magenta ]
          "* %d tests were skipped\n%!" nb;
  end;
  begin match c.tests_failexpected with
    | [] -> ()
    | list ->
        let nb = List.length list in
        Terminal.printf [ Terminal.magenta ]
          "* %d tests were expected to fail:%!" nb;
        Runner_common.print_ntests 5 (List.rev list);
        Printf.printf "\n%!";
  end;
  let buffer = Buffer.contents c.buffer in
  let buffer_file = tests_dir // "results.log" in
  EzFile.write_file buffer_file buffer;
  Printf.eprintf "File %S created with failure results\n%!" buffer_file;
  if !print_all then
    Terminal.printf [ Terminal.magenta ] "%s\n%!" buffer;
  ()

let print_test _c t =
  Printf.printf "%04d %-50s %s\n%!" t.id t.name t.test_loc;
  ()

let print c =
  c.current_banner <- "";
  Filter.select_tests
    (fun t ->
       if t.banner <> c.current_banner then begin
         Printf.eprintf "\n%s\n\n%!" t.banner;
         c.current_banner <- t.banner
       end;
       print_test c t;
    ) c

let args = [

  [ "t" ; "testsuite" ], Arg.String (fun s -> testsuite := s),
  EZCMD.info
    ~env:(EZCMD.env "AUTOFONCE_TESTSUITE")
    ~docv:"FILE" "File to lookup (default to 'tests/testsuite.at')";

]
