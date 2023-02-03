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

open EzCompat (* for IntMap *)
open Types
open Ez_file.V1
open EzFile.OP

open Autofonce_m4.M4Types
module M4Printer = Autofonce_m4.M4Printer
module M4Parser = Autofonce_m4.M4Parser
module M4Types = Autofonce_m4.M4Types
module Misc = Autofonce_misc.Misc

let name_of_loc loc =
  M4Printer.string_of_location { loc with file = Filename.basename loc.file }

let int_of_string macro n =
  try
    int_of_string n
  with _ ->
    M4Parser.macro_error macro "int_of_string (%S)" n

let rec iter_macros c macros =
  match macros with
  | [] -> ()
  | macro :: macros ->
      match macro.kind with
      | Shell cmd   ->
          Printf.eprintf "At %s:\n  Discarding toplevel shell line: %s\n%!"
            (M4Printer.string_of_location macro.loc) cmd;
          iter_macros c macros (* TODO *)
      | Macro ("m4_include", [ filename ]) ->
          let filename = M4Parser.to_string filename in
          let dirname = Filename.dirname macro.loc.file in
          let new_macros = load_file
              ( dirname // "testsuite.src" // filename ) in
          iter_macros c ( new_macros @ macros )

      | Macro ("AT_COPYRIGHT", [ copyright ]) ->
          let copyright = M4Parser.to_string copyright in
          c.suite_copyright <- copyright ;
          iter_macros c macros
      | Macro ("AT_INIT", [ name ]) ->
          let name = M4Parser.to_string name in
          c.suite_name <- name ;
          iter_macros c macros
      | Macro ("AT_COLOR_TESTS", [ ]) ->
          iter_macros c macros
      | Macro ("AT_TESTED", [ tested ]) ->
          let tested = M4Parser.to_string tested in
          c.suite_tested_programs <- c.suite_tested_programs @
                               EzString.split_simplify tested ' ';
          iter_macros c macros
      | Macro ("AT_BANNER", [ banner ]) ->
          let banner = M4Parser.to_string banner in
          c.suite_banners <- banner :: c.suite_banners ;
          iter_macros c macros

      | Macro ("AT_SETUP", [ name ]) ->
          c.suite_ntests <- c.suite_ntests + 1;
          let test_name = M4Parser.to_string name in
          let test_id = c.suite_ntests in
          let t = {
            test_suite = c;
            test_loc = macro.loc ;
            test_name ;
            test_id;
            test_keywords = [];
            test_actions = [];
            test_banner = (match c.suite_banners with
                | [] -> "" | hd :: _ -> hd);
          }
          in
          let steps = ref [0] in
          Hashtbl.add c.suite_test_by_id test_id t;
          c.suite_tests <- t :: c.suite_tests ;
          let macros, actions = iter_actions t steps [] macros in
          t.test_actions <- actions ;
          iter_macros c macros

      | Macro (_, _) ->
          M4Parser.macro_error macro "At top, unexpected macro %S"
            (M4Printer.string_of_macro macro)

and iter_actions t steps actions macros =
  match macros with
  | [] -> [], List.rev actions
  | macro :: macros ->
      match macro.kind with

      | Macro ("AT_KEYWORDS", [ keywords]) ->
          let keywords = M4Parser.to_string keywords in
          t.test_keywords <- t.test_keywords
                             @ EzString.split_simplify keywords ' ';
          iter_actions t steps actions macros

      | Macro ("AT_CLEANUP", [] ) ->
          macros,
          List.rev ( AT_CLEANUP { loc = macro.loc } :: actions )

      | Shell s when EzString.starts_with s ~prefix:"if "
            && EzString.ends_with s ~suffix:"then"
        ->
          let len = String.length s in
          let test = String.sub s 3 (len-3-4) in
          parse_if t steps actions macro.loc test macros

      | _ ->
          let action = parse_action t steps macro in
          iter_actions t steps ( action :: actions ) macros

and parse_if t steps actions check_loc check_command macros =
  let rec iter list macros =
    match macros with
    | [] -> failwith "unterminated if"
    | macro :: macros ->
        match macro.kind with
        | Shell "else" -> "else", List.rev list, macros
        | Shell "fi" -> "fi", List.rev list,  macros
        | _ -> iter (macro::list) macros
  in
  let (k, then_, macros) = iter [] macros in
  let (k, else_, macros) = if k = "fi" then k, [], macros else
      iter [] macros in
  assert (k <> "else");
  let action =
      (* Printf.eprintf "Shell: [ %s ]\n%!" cmd;*)
      AT_CHECK {
        check_step = next_step steps ;
        check_kind = "IF" ;
        check_command ;
        check_loc ;
        check_retcode = Some 0 ;
        check_stdout = Ignore ;
        check_stderr = Ignore ;
        check_test = t;
        check_run_if_pass = parse_actions t steps else_ ;
        check_run_if_fail = parse_actions t steps then_ ;
      }
  in
  iter_actions t steps (action :: actions) macros

and parse_action t steps macro =
  match macro.kind with
  | Macro ("AT_DATA", [ file ; content]) ->
      let file = M4Parser.to_string file in
      let content = M4Parser.to_string content in
      AT_DATA  { file ; content }

  | Macro ("AT_CAPTURE_FILE", [ file ] ) ->
      let file = M4Parser.to_string file in
      AT_CAPTURE_FILE  file

  | Macro ("AT_XFAIL_IF", [ test ]) ->
      let test = M4Parser.to_string test in
      AT_XFAIL_IF test

  | Macro ("AT_SKIP_IF", [ test ]) ->
      let test = M4Parser.to_string test in
      begin
        match test with
        | "true" -> AT_SKIP
        | check_command ->
            AT_CHECK {
              check_step = next_step steps ;
              check_kind = "SKIPIF" ;
              check_command ;
              check_loc = macro.loc ;
              check_retcode = Some 1 ;
              check_stdout = Ignore ;
              check_stderr = Ignore ;
              check_test = t;
              check_run_if_pass = [] ;
              check_run_if_fail = [ AT_SKIP ] ;
            }
      end

  | Macro ("AT_CHECK", args ) ->
      let check_loc = macro.loc in
      let check_command, args =
        match args with
        | [] -> M4Parser.macro_error macro "Missing argument 1 to AT_CHECK"
        | command :: args ->
            M4Parser.to_string command, args
      in
      let check_retcode, args =
        match args with
        | [] -> None, []
        | num :: args ->
            Some (
              int_of_string macro
                ( M4Parser.to_string num )), args
      in
      let check_stdout, args =
        match args with
        | [] -> Ignore, []
        | arg :: args ->
            let arg = M4Parser.to_string arg in
            let arg = match arg with
              | "ignore" -> Ignore
              | s -> Content s
            in
            arg, args
      in
      let check_stderr, args =
        match args with
        | [] -> Ignore, []
        | arg :: args ->
            let arg = M4Parser.to_string arg in
            let arg = match arg with
              | "ignore" -> Ignore
              | s -> Content s
            in
            arg, args
      in
      let check_run_if_fail, args =
        match args with
        | [] -> [], []
        | arg :: args ->
            let macros = M4Parser.parse_string
                           ~loc:arg.arg_loc arg.arg in
            parse_actions t steps macros, args
      in
      let check_run_if_pass, args =
        match args with
        | [] -> [], []
        | arg :: args ->
            let macros = M4Parser.parse_string
                           ~loc:arg.arg_loc arg.arg in
            parse_actions t steps macros, args
      in
      begin
        match args with
        | [] -> ()
        | _ ->
            M4Parser.macro_error macro "extra arguments to AT_CHECK"
      end;
      let check_step = next_step steps in
      AT_CHECK {
        check_test = t ;
        check_kind = "CHECK" ;
        check_loc ;
        check_step ;
        check_command ;
        check_retcode ;
        check_stdout ;
        check_stderr ;
        check_run_if_fail ;
        check_run_if_pass }

  | Macro ("MANUAL_CHECK", _ ) ->
      (* TODO *)
      assert false

  | Shell check_command ->
      (* Printf.eprintf "Shell: [ %s ]\n%!" cmd;*)
      AT_CHECK {
        check_step = next_step steps ;
        check_kind = "SHELL" ;
        check_command ;
        check_loc = macro.loc ;
        check_retcode = Some 0 ;
        check_stdout = Ignore ;
        check_stderr = Ignore ;
        check_test = t;
        check_run_if_pass = [] ;
        check_run_if_fail = [] ;
      }

  | Macro (_, _) ->
      Printf.eprintf "%s: unexpected macro %S\n%!"
        (M4Printer.string_of_location macro.loc)
        (M4Printer.string_of_macro macro) ;
      assert false

and next_step steps =
  let step = String.concat "_"
      ( List.map ( Printf.sprintf "%02d" ) ( List.rev !steps ) )
  in
  steps := ( match !steps with
      | [] -> assert false
      | step :: tail -> (step+1) :: tail
    );
  step

and parse_actions t steps macros =
  let former_steps = !steps in
  steps := 0 :: former_steps ;
  let macros, actions =
    iter_actions t steps [] macros in
  assert (macros = []);
  steps := former_steps;
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
  ()

let read filename =
  let suite_dir = Filename.dirname filename in
  let c = {
    suite_name = "";
    suite_ntests  = 0;
    suite_test_by_id = Hashtbl.create 1000 ;
    suite_tests = [] ;
    suite_tested_programs = [] ;
    suite_copyright = "" ;
    suite_banners = [];
    suite_file = Filename.basename filename;
    suite_dir ;
  }
  in
  load_file c filename;
  c.suite_banners <- List.rev c.suite_banners ;
  c.suite_tests <- List.rev c.suite_tests ;
  c

exception TestsuiteNotFound of string

let find testsuite_at =
  match Misc.find_file testsuite_at with
  | exception Not_found -> raise ( TestsuiteNotFound testsuite_at)
  | filename ->
      Printf.eprintf "Found testsuite in %s\n%!" filename;
      let c = read filename in
      Printf.eprintf "Found %d tests in testsuite\n%!" c.suite_ntests;
      c

let m4_escape s =
  let b = Buffer.create (String.length s) in
  Buffer.add_char b '[';
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '[' -> Buffer.add_string b "@<:@"
    | ']' -> Buffer.add_string b "@:>@"
    | c -> Buffer.add_char b c
  done;
  Buffer.add_char b ']';
  Buffer.contents b
