(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

open Ezcmd.V2
open EZCMD.TYPES

open Globals (* toplevel references *)

let cmd =
  let args =
    Testsuite.args @
    Filter.args @
    [

      [ "print-all" ], Arg.Set print_all,
      EZCMD.info "Print also expected failures";

      [ "e" ; "stop-on-failure" ], Arg.Set stop_on_first_failure,
      EZCMD.info "Stop on first failure";

      [ "s" ; "keep-more" ], Arg.Set keep_skipped,
      EZCMD.info "Keep directories of skipped and expected failed";

      [ "S" ; "keep-all" ], Arg.Set keep_all,
      EZCMD.info "Keep all directories of tests";

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
       let c = Autofonce_core.Parser.find !Globals.testsuite in
       Testsuite.exec c
    )
    ~args
    ~doc: "Run testsuite of the current project"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P {|Run the testsuite.|} ;
        `P {|$(b,autofonce) expects the existence of two files: $(b,autofonce.env) and $(b,tests/testsuite.at).|} ;
        `P {|$(b,autofonce.env) is required to configure the tests that will be run. Check the following command for more information:|} ;
        `Pre {|\$ autofonce init --help|} ;
        `P {|$(b,tests/testsuite.at) contains the descriptions of the tests, as expected by the GNU Autoconf testsuite. Check the following command for more information:|} ;
        `Pre {|\$ autofonce list --help|} ;
        `P {|To run tests, $(b,autofonce) will create a directory $(b,_autotest/) in the directory containing the file $(b,autofonce.env).|} ;
        `P {|Every test is run independantly in a test directory with its number in the $(b,_autotest/) directory. The test directory is removed if the test does not fail, or if it was expected to fail. Use the $(b,--keep-more) argument to keep directories of tests that have been skipped or were expected to fail. Use the $(b,--keep-all) argument to keep all directories.|} ;
        `P {|You can select which tests to run, by selecting a range of tests using $(b,--after TEST) or $(b,--before TEST), by selecting individual tests identifiers using $(b,--id NUM) or by selecting keywords using $(b,--keyword KEYWORD).|} ;
        `P {|$(b,autofonce) will only display failed tests on its output. You can use the argument $(b,--print-all) to display all tests that were not OK, or just read the generated file $(b,_autotests/results.log).|};
      ];
    ]
