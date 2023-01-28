(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

type check_output =
  | Ignore
  | Content of string

type check = {
  check_loc : string ;
  step : string ;
  command : string ;
  retcode : int option ;
  stdout : check_output ;
  stderr : check_output ;
  run_if_fail : action list ;
  run_if_pass : action list ;
  is_check : bool ;

  test : test ;
}

and action =
  | AT_DATA of { file:string ; content: string }
  | AT_CAPTURE_FILE of string
  | AT_XFAIL_IF of string
  | AT_SKIP_IF of string
  | AT_CHECK of check

and test = {
  suite : testsuite ;
  test_loc : string ;
  name : string ;
  id : int ;
  banner : string ;
  mutable keywords : string list ;
  mutable actions : action list ;

  (* can only be determined during execution *)
  mutable fail_expected : bool ;

  (* imperative part *)
  mutable steps : int list ;
}

and testsuite = {
  testsuite_dir : string ; (* dir of "testsuite.at" *)
  autotest_dir : string ;  (* dir of "autotest.sh" *)
  autotest_env : string ;   (* content of "autotest.sh" *)
  mutable ntests : int ;
  test_by_id : ( int, test ) Hashtbl.t ;
  mutable tests : test list ;
  mutable tested_programs : string list ;
  mutable copyright : string ;
  mutable testsuite : string ;

  (* imperative context, these values are meaningless at the end of
     functions *)
  mutable current_banner : string ;
  mutable status : string ;
  mutable ntests_ran : int ;
  mutable ntests_ok : int ;
  mutable tests_failed : test list ;
  mutable tests_skipped : test list ;
  mutable tests_failexpected : test list ;
  mutable buffer : Buffer.t ;
}


let rec string_of_action = function
  | AT_DATA { file ; content } ->
      Printf.sprintf "AT_DATA ( file=%S, content=%S )" file content
  | AT_CAPTURE_FILE string ->
      Printf.sprintf "AT_CAPTURE_FILE %s" string
  | AT_XFAIL_IF string ->
      Printf.sprintf "AT_XFAIL_IF %s" string
  | AT_SKIP_IF string ->
      Printf.sprintf "AT_SKIP_IF %s" string
  | AT_CHECK  check ->
      Printf.sprintf "AT_CHECK %s" ( string_of_check check )

and string_of_check_output = function
    | Ignore -> "IGNORE"
    | Content s -> Printf.sprintf "EXPECT [%s]" s

and string_of_runif = function
  | [] -> ""
  | _ -> "[ACTIONS]"

and string_of_check c =
  let b = Buffer.create 1000 in
  Printf.bprintf b "{\n";
  Printf.bprintf b "  check_loc = %s;\n" c.check_loc;
  Printf.bprintf b "  step = %S;\n" c.step;
  Printf.bprintf b "  command = %S;\n" c.command;
  Printf.bprintf b "  retcode = %s;\n" (match c.retcode with
      | None -> "" | Some n -> string_of_int n);
  Printf.bprintf b "  stdout = %s;\n" (string_of_check_output c.stdout);
  Printf.bprintf b "  stderr = %s;\n" (string_of_check_output c.stderr);
  Printf.bprintf b "  run_if_fail = %s;\n" (string_of_runif c.run_if_fail);
  Printf.bprintf b "  run_if_pass = %s;\n" (string_of_runif c.run_if_pass);
  Printf.bprintf b "  }";
  Buffer.contents b

exception Error of string
