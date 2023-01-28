(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)


open Autofonce_m4 (* for M4Types, M4Parser *)
open M4Types

open EzCompat (* for IntMap *)
open Types
open Ez_file.V1
open EzFile.OP

let name_of_loc loc =
  M4Printer.string_of_location { loc with file = Filename.basename loc.file }

let add_action t action =
  t.actions <- t.actions @ [ action ]

let rec iter_macros c macros =
  match macros with
  | [] -> ()
  | macro :: macros ->
      match macro.exp with
      | Shell cmd   ->
          Printf.eprintf "Shell: [ %s ]\n%!" cmd;
          iter_macros c macros (* TODO *)
      | Macro ("m4_include", [ Quoted filename ]) ->

          let dirname = Filename.dirname macro.loc.file in
          let new_macros = load_file
              ( dirname // "testsuite.src" // filename ) in
          iter_macros c ( new_macros @ macros )

      | Macro ("AT_COPYRIGHT", [ Quoted copyright ]) ->
          c.copyright <- copyright ;
          iter_macros c macros
      | Macro ("AT_INIT", [ Quoted name ]) ->
          c.testsuite <- name ;
          iter_macros c macros
      | Macro ("AT_COLOR_TESTS", [ ]) ->
          iter_macros c macros
      | Macro ("AT_TESTED", [ Quoted tested ]) ->
          c.tested_programs <- c.tested_programs @
                               EzString.split_simplify tested ' ';
          iter_macros c macros
      | Macro ("AT_BANNER", [ Quoted banner ]) ->
          c.current_banner <- banner ;
          iter_macros c macros

      | Macro ("AT_SETUP", [ Quoted name ]) ->
          c.ntests <- c.ntests + 1;
          let id = c.ntests in
          let t = {
            suite = c;
            test_loc = name_of_loc macro.loc ;
            name ;
            id;
            keywords = [];
            actions = [];
            banner = c.current_banner ;
            steps = [ 0 ];
            fail_expected = false ;
          }
          in
          Hashtbl.add c.test_by_id id t;
          c.tests <- t :: c.tests ;
          let macros = iter_macro t macros in
          iter_macros c macros

      | Macro (_, _) ->
          Printf.eprintf "At top, unexpected macro %S\n%!"
            (M4Printer.string_of_macro macro) ;
          assert false

and iter_macro t macros =
  match macros with
  | [] -> []
  | macro :: macros ->
      match macro.exp with

      | Macro ("AT_KEYWORDS", [ Quoted keywords]) ->
          t.keywords <- t.keywords @ EzString.split_simplify keywords ' ';
          iter_macro t macros

      | Macro ("AT_CLEANUP", [] ) -> macros

      | _ ->
          let action = parse_action t macro in
          add_action t action ;
          iter_macro t macros

and parse_action t macro =
  match macro.exp with
  | Macro ("AT_DATA", [ Quoted file ; Quoted content]) ->
      AT_DATA  { file ; content }

  | Macro ("AT_CAPTURE_FILE", [ Quoted file ] ) ->
      AT_CAPTURE_FILE  file

  | Macro ("AT_XFAIL_IF",
           [ Quoted test
           | Block [{exp=Macro (test,[]);loc=_}] ] ) ->
      AT_XFAIL_IF test

  | Macro ("AT_SKIP_IF", [ Quoted "true"
                         | Block [{exp=Macro ("true",[]);loc=_}] ] ) ->
      AT_SKIP

  | Macro ("AT_SKIP_IF", [ Quoted command
                         | Block [{exp=Macro (command,[]);loc=_}] ] ) ->
      AT_CHECK {
        step = next_step t ;
        is_check = false ;
        command ;
        check_loc = name_of_loc macro.loc ;
        retcode = Some 1 ;
        stdout = Ignore ;
        stderr = Ignore ;
        test = t;
        run_if_pass = [] ;
        run_if_fail = [ AT_SKIP ] ;
      }

  | Macro ("AT_CHECK", args ) ->
      let check_loc = name_of_loc macro.loc in
      let command, args =
        match args with
        | [] -> Misc.macro_error macro "Missing argument 1 to AT_CHECK"
        | (Quoted command) :: args -> command, args
        | _ -> Misc.macro_error macro "Wrong argument 1 to AT_CHECK"
      in
      let retcode, args =
        match args with
        | [] -> None, []
        | (Quoted num) :: args -> Some ( int_of_string num ), args
        | _ -> Misc.macro_error macro "Wrong argument 2 to AT_CHECK"
      in
      let stdout, args =
        match args with
        | [] -> Ignore, []
        | arg :: args ->
            let arg =
              match arg with
              | Block [ { exp = Macro ("ignore", []); loc=_ }] -> Ignore
              | Quoted "ignore" -> Ignore
              | Quoted s -> Content s
              | _ -> Misc.macro_error macro "Wrong argument 3 to AT_CHECK"
            in
            arg, args
      in
      let stderr, args =
        match args with
        | [] -> Ignore, []
        | arg :: args ->
            let arg =
              match arg with
              | Quoted "ignore" -> Ignore
              | Quoted s -> Content s
              | Block [ { exp = Macro ("ignore", []); loc=_ }] -> Ignore
              | _ -> Misc.macro_error macro "Wrong argument 4 to AT_CHECK"
            in
            arg, args
      in
      let run_if_fail, args =
        match args with
        | [] -> [], []
        | arg :: args ->
            let arg =
              match arg with
              | Block macros ->
                  parse_actions t macros
              | _ -> Misc.macro_error macro "Wrong argument 5 to AT_CHECK"
            in
            arg, args
      in
      let run_if_pass, args =
        match args with
        | [] -> [], []
        | arg :: args ->
            let arg =
              match arg with
              | Block macros -> parse_actions t macros
              | _ -> Misc.macro_error macro "Wrong argument 6 to AT_CHECK"
            in
            arg, args
      in
      begin
        match args with
        | [] -> ()
        | _ ->
            Misc.macro_error macro "extra arguments to AT_CHECK"
      end;
      let step = next_step t in
      AT_CHECK {
        test = t ;
        is_check = true ;
        check_loc ; step ;
        command ; retcode ; stdout ; stderr ;
        run_if_fail ; run_if_pass }

  | Macro ("MANUAL_CHECK", _ ) ->
      (* TODO *)
      assert false

  | Shell command ->
      (* Printf.eprintf "Shell: [ %s ]\n%!" cmd;*)
      AT_CHECK {
        step = next_step t ;
        is_check = false ;
        command ;
        check_loc = name_of_loc macro.loc ;
        retcode = None ;
        stdout = Ignore ;
        stderr = Ignore ;
        test = t;
        run_if_pass = [] ;
        run_if_fail = [] ;
      }

  | Macro (_, _) ->
      Printf.eprintf "%s: unexpected macro %S\n%!"
        (M4Printer.string_of_location macro.loc)
        (M4Printer.string_of_macro macro) ;
      assert false

and next_step t =
  let step = String.concat "_"
      ( List.map ( Printf.sprintf "%02d" ) ( List.rev t.steps ) )
  in
  t.steps <- ( match t.steps with
      | [] -> assert false
      | step :: tail -> (step+1) :: tail
    );
  step

and parse_actions t macros =
  let former_steps = t.steps in
  t.steps <- 0 :: former_steps ;
  let actions = List.map ( parse_action t ) macros in
  t.steps <- former_steps;
  actions

and load_file filename =
  (* Printf.eprintf "Loading tests from %s\n%!" filename; *)
  match M4Parser.parse_file filename with
  | exception ( M4Types.Error ( s, loc ) ) ->
      Printf.eprintf "%s:%d:%d: Error %s\n%!"
        loc.file loc.line loc.char s;
      exit 2
  | macros -> macros

let load_file c filename =
  let macros = load_file filename in
  iter_macros c macros;
