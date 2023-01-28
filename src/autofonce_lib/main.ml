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

let read_testsuite filename =
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

let find_testsuite () =
  try
    let filename = Misc.find_file !testsuite in
    Printf.eprintf "Found testsuite in %s\n%!" filename;
    let c = read_testsuite filename in
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

let exec_testsuite c =
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

let list_testsuite c =
  c.current_banner <- "";
  Filter.select_tests
    (fun t ->
       if t.banner <> c.current_banner then begin
         Printf.eprintf "\n%s\n\n%!" t.banner;
         c.current_banner <- t.banner
       end;
       print_test c t;
    ) c

let common_args = [

  [ "t" ; "testsuite" ], Arg.String (fun s -> testsuite := s),
  EZCMD.info ~docv:"SUFFIX" "File to lookup (default to 'tests/testsuite.at')";

]

let filter_args = [

  [ "k"; "keyword" ], Arg.String (fun s ->
      tests_keywords := !tests_keywords @ [ s ];
      clean_tests_dir := false
    ),
  EZCMD.info ~docv:"ID" "Run only test ID";

  [ "id" ], Arg.Int (fun id ->
      tests_ids := !tests_ids @ [ id ];
      clean_tests_dir := false
    ),
  EZCMD.info ~docv:"ID" "Run only test ID";

  [ "after" ], Arg.Int (fun x ->
      exec_after := x;
      clean_tests_dir := false;
    ),
  EZCMD.info ~docv:"ID" "Exec starting at test $(docv)";

  [ "before" ], Arg.Int (fun x ->
      exec_before := x;
      clean_tests_dir := false;
    ),
  EZCMD.info ~docv:"ID" "Exec ending at test $(docv)";

]

let run_cmd =
  let args =
    common_args @
    filter_args @
    [

      [ "print-all" ], Arg.Set print_all,
      EZCMD.info "Print also expected failures";

      [ "e" ; "stop-on-failure" ], Arg.Set stop_on_first_failure,
      EZCMD.info "Stop on first failure";

      [ "K" ; "keep-more" ], Arg.Set keep_skipped,
      EZCMD.info "Keep directories of skipped and expected failed";

      [ "no-clean" ], Arg.Clear clean_tests_dir,
      EZCMD.info "Do not clean _autotest/ dir on startup";

      [ "j1" ], Arg.Unit (fun () -> max_jobs := 1),
      EZCMD.info "Use Sequential scheduling of tests";

      [ "j" ], Arg.Int (fun n -> max_jobs := max 1 n),
      EZCMD.info ~docv:"NJOBS" "Set maximal parallelism";

    ]
  in
  EZCMD.sub
    "run"
    (fun () ->
       let c = find_testsuite () in
       exec_testsuite c
    )
    ~args
    ~doc: "Run testsuite of the current project"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P ""
      ];
    ]

let list_cmd =
  let args =
    common_args @
    filter_args @
    [
    ]
  in
  EZCMD.sub
    "list"
    (fun () ->
       let c = find_testsuite () in
       list_testsuite c
    )
    ~args
    ~doc: "Print testsuite of the current project"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P ""
      ];
    ]

let list_known_project () =
  Printf.printf "Known projects with environment files:\n";
  List.iter (fun file ->
      if Filename.check_suffix file ".env" then
        Printf.printf "* %S\n%!" (Filename.chop_suffix file ".env")
    ) Autofonce_share.Tree.file_list;
  Printf.printf "%!";
  exit 0

let init_cmd =
  let force_update = ref false in
  let project = ref None in
  let args =
    [
      [ "f"; "force-update" ], Arg.Set force_update,
      EZCMD.info "Force update of the environment file";

      [ "p"; "project" ], Arg.String (fun s -> project := Some s),
      EZCMD.info ~docv:"PROJECT" "Use environment file from known project";

      [ "l"; "list-known" ], Arg.Unit list_known_project,
      EZCMD.info "List known projects with environment files";

    ]
  in
  EZCMD.sub
    "init"
    (fun () ->
       if Sys.file_exists autotest_env && not !force_update then
         Misc.error
           "File %s already present (use -f to update)" autotest_env;

       begin
         match Misc.find_file autotest_env with
         | exception Not_found -> ()
         | file ->
             Printf.eprintf
               "Warning: %S already present in top dirs at\n %s\n%!"
               autotest_env file;
       end;

       let rec autodetect dirname =
         let basename = Filename.basename dirname in
         match Autofonce_share.Files.content ( basename ^ ".env" ) with
         | content -> (basename, content)
         | exception Not_found ->
             let parent_dirname = Filename.dirname dirname in
             if parent_dirname = dirname then raise Not_found;
             autodetect parent_dirname
       in
       let project, content =
         match
           match !project with
           | None
           | Some "auto" ->
               autodetect (Sys.getcwd ())
           | Some project ->
               match Autofonce_share.Files.content ( project ^ ".env" ) with
               | content -> (project, content)
               | exception Not_found ->
                   Misc.error "Project %S is not known" project
         with
         | (project, content) ->
             (Printf.sprintf "for project %S" project, content)
         | exception Not_found ->
             Printf.eprintf "Warning: autodetection of project failed\n%!";
             let content = {|
# set env variables for tests here
# (you can gather inspiration from existing atlocal/atconfig files)
|}
             in
             let project = "with EMPTY CONTENT." in
             (project, content)
       in
       EzFile.write_file autotest_env content;
       Printf.eprintf "File %S created %s.\n%!"
         autotest_env project;

    )
    ~args
    ~doc: "Print testsuite of the current project"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P ""
      ];
    ]
