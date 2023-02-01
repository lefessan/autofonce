(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

open EzCompat

open Autofonce_core
open Types
open Globals (* toplevel references *)

let select_tests select_test c =
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
        select_test t)
    c.suite_tests

open Ezcmd.V2
open EZCMD.TYPES

let args = [

  [ "k"; "keywords" ], Arg.String (fun s ->
      tests_keywords := !tests_keywords @
                        EzString.split_simplify s ' ';
      clean_tests_dir := false
    ),
  EZCMD.info ~docv:"KEYWORD" "Run only tests matching KEYWORD";

  [ "ids" ], Arg.String (fun ids ->
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

  [], Arg.Anons (fun list ->
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
