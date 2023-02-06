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

open EzCompat

open Types
open Globals (* toplevel references *)
open Ez_file.V1
open EzFile.OP

module Misc = Autofonce_misc.Misc

let failures = ref None

let select_tests ?state select_test suite =
  ignore state;
  let all_tests =
    !tests_ids = [] && !tests_keywords = []
  in
  let id_set =
    match !tests_ids with
    | [] -> IntSet.empty
    | ids -> IntSet.of_list ids
  in
  let keyword_set =
    match !tests_keywords with
    | [] -> StringSet.empty
    | ids -> StringSet.of_list ids
  in
  let nokeyword_set =
    match !tests_nokeywords with
    | [] -> StringSet.empty
    | ids -> StringSet.of_list ids
  in
  List.iter (fun t ->
      if t.test_id >= !exec_after
      && t.test_id <= !exec_before
      && (all_tests
          || IntSet.mem t.test_id id_set
          || List.exists (fun k -> StringSet.mem k keyword_set)
            t.test_keywords
         )
      && not (
          List.exists (fun k -> StringSet.mem k nokeyword_set)
            t.test_keywords
        )
      then
        if !only_failed then begin
          (* only_failed option should only be available
                                   with state *)
          match state with
          | None ->
              Misc.error "Options --failed/--failure only works with 'run' or 'promote'"
          | Some state ->
              let test_dir = Runner_common.test_dir t in
              let test_dir = state.state_run_dir // test_dir in
              if Sys.file_exists test_dir then
                match !failures with
                | None ->
                    select_test t
                | Some failure ->
                    let reason =
                      let failure_exitcode = ref false in
                      let failure_stdout = ref false in
                      let failure_stderr = ref false in
                      let files = Sys.readdir test_dir in
                      Array.iter (fun file ->
                          if Filename.check_suffix file ".exit.expected" then
                            failure_exitcode := true
                          else
                          if Filename.check_suffix file ".out.expected" then
                            failure_stdout := true
                          else
                          if Filename.check_suffix file ".err.expected" then
                            failure_stderr := true
                        ) files ;
                      String.concat " " (
                        begin
                          if !failure_exitcode then
                            [ "exitcode" ]
                          else
                            []
                        end
                        @
                        begin
                          if !failure_stdout then
                            [ "stdout" ]
                          else
                            []
                        end
                        @
                        begin
                          if !failure_stderr then
                            [ "stderr" ]
                          else
                            []
                        end
                      )
                    in
                    if failure = reason then
                      select_test t
        end else
          select_test t
    )
    suite.suite_tests

open Ezcmd.V2
open EZCMD.TYPES

let args = [

  [ "k"; "keywords" ], Arg.String (fun s ->
      tests_keywords := !tests_keywords @
                        EzString.split_simplify s ' ';
      clean_tests_dir := false
    ),
  EZCMD.info ~docv:"KEYWORD" "Run only tests matching KEYWORD";

  [ "i"; "ids" ], Arg.String (fun ids ->
      tests_ids := !tests_ids @
                   (List.map int_of_string
                      (EzString.split_simplify ids ' '));
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

  [ "N"; "not" ], Arg.String (fun s ->
      tests_nokeywords := !tests_nokeywords @
                          EzString.split_simplify s ' ';
      clean_tests_dir := false
    ),
  EZCMD.info ~docv:"KEYWORD" "Skip tests matching KEYWORD";

  [ "failures" ], Arg.String (fun s ->
      only_failed := true ;
      failures := Some s;
      clean_tests_dir := false
    ),
  EZCMD.info ~docv:"REASON" "Run failed tests with given failure";

  [ "failed" ], Arg.Unit (fun () ->
      only_failed := true ;
      clean_tests_dir := false
    ),
  EZCMD.info "Run only previously failed tests (among selected tests)";

  [], Arg.Anons (fun list ->
      match list with
      | [] -> ()
      | _ ->
          List.iter (fun s ->
              match int_of_string s with
              | id -> tests_ids := !tests_ids @ [id]
              | exception _ ->
                  tests_keywords := !tests_keywords @ [s]
            ) list ;
          clean_tests_dir := false;
    ),
  EZCMD.info ~docv:"ID" "Exec ending at test $(docv)";

]
