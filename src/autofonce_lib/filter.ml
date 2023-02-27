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
let exec_after = ref 0
let exec_before = ref max_int
let tests_ids = ref ( [] : ( int * int ) list )
let tests_keywords = ref ( [] : string list )
let tests_nokeywords = ref ( [] : string list )
let only_failed = ref false

let select_tests ?state select_test suite =
  let ntests = suite.suite_ntests in
  let all_tests =
    !tests_ids = [] && !tests_keywords = []
  in
  let id_set =
    match !tests_ids with
    | [] -> Array.make (ntests+1) false
    | ids ->
        let t = Array.make (ntests+1) false in
        List.iter (fun (id1,id2) ->
            for i = (max id1 1) to (min id2 ntests) do
              t.(i) <- true
            done;
          ) ids;
        t
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
          || id_set. (t.test_id)
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

let set_id s =
  try
    let range =
      match EzString.split s '-' with
      | [id1 ; "" ] -> (int_of_string id1, max_int)
      | [id1 ; id2 ] -> (int_of_string id1, int_of_string id2)
      | [id] ->
          let id = int_of_string id in
          if id < 0 then (0,-id-1) else (id,id)
      | _ -> raise Exit
    in
    tests_ids := !tests_ids @ [ range ]
  with _ ->
    tests_keywords := !tests_keywords @ [s]

let args = [

  [ "k"; "keywords" ], Arg.String (fun s ->
      tests_keywords := !tests_keywords @
                        EzString.split_simplify s ' ';
      clean_tests_dir := false
    ),
  EZCMD.info ~docv:"KEYWORD" "Run only tests matching KEYWORD";

  [ "i"; "ids" ], Arg.String (fun ids ->
      List.iter set_id @@ EzString.split_simplify ids ' ' ;
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
              set_id s
            ) list ;
          clean_tests_dir := false;
    ),
  EZCMD.info ~docv:"ID" "Exec ending at test $(docv)";

]
