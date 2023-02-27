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
let keep_skipped = ref false
let keep_all = ref false
let auto_promote = ref 0

let fake = ref false
