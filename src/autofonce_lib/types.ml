(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

open Autofonce_core
include Types

(* imperative context, these values are meaningless at the end of
   functions *)
type state = { (* variable name is `state` *)
  state_suite : suite ;
  state_run_dir : string ;
  state_env : string ;
  mutable state_banner : string ;
  mutable state_status : string ;
  mutable state_ntests_ran : int ;
  mutable state_ntests_ok : int ;
  mutable state_tests_failed : tester list ;
  mutable state_tests_skipped : tester list ;
  mutable state_tests_failexpected : tester list ;
  mutable state_buffer : Buffer.t ;
  mutable state_ntests : int ;
  mutable state_nchecks : int ;
}

and tester = { (* variable name is `ter` *)
  tester_state : state ;
  tester_suite : suite ;
  tester_test : test ;
  (* can only be determined during execution *)
  mutable tester_fail_expected : bool ;
}

and checker = { (* variable name is `cer` *)
  checker_check : check ;
  checker_tester : tester ;
  checker_pid : int ;
}
