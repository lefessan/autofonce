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

let verbose = ref 1

let stop_on_first_failure = ref false
let clean_tests_dir = ref true
let print_all = ref false
let max_jobs = ref 16
let testsuite = ref ( None : string option )
let keep_skipped = ref false
let keep_all = ref false
let auto_promote = ref 0

let fake = ref false

(* selection of tests *)
let exec_after = ref 0
let exec_before = ref max_int
let tests_ids = ref ( [] : ( int * int ) list )
let tests_keywords = ref ( [] : string list )
let tests_nokeywords = ref ( [] : string list )
let only_failed = ref false
