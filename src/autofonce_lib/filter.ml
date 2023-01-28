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
  List.iter (fun t ->
      if t.id >= !exec_after
      && t.id <= !exec_before
      && (all_tests
          || IntSet.mem t.id id_set
          || List.exists (fun k -> StringSet.mem k keyword_set) t.keywords
         )
      then
        select_test t)
    c.tests
