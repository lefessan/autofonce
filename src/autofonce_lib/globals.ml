(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

let verbose = ref 1

let stop_on_first_failure = ref false
let clean_tests_dir = ref true
let print_all = ref false
let max_jobs = ref 16
let testsuite = ref "tests/testsuite.at"
let keep_skipped = ref false
let keep_all = ref false

(* selection of tests *)
let exec_after = ref 0
let exec_before = ref max_int
let tests_ids = ref ( [] : int list )
let tests_keywords = ref ( [] : string list )
let tests_nokeywords = ref ( [] : string list )

(* file looked-up for project specific testsuite environment *)
let autotest_env = "autofonce.env"

(* toplevel dir created to run tests *)
let tests_dir = "_autotest"

(* name of env script created in every test dir *)
let env_autotest_sh = "env_autofonce.sh"
