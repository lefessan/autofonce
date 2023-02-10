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
open Ezcmd.V2
open EZCMD.TYPES
open Ez_file.V1
open EzFile.OP

module Misc = Autofonce_misc.Misc
module Parser = Autofonce_core.Parser
module Project_config = Autofonce_config.Project_config
open Types
open Globals (* toplevel references *)

(* returns run_dir and project_config *)
let find_project_config () =
  match Misc.find_file Autofonce_config.Globals.project_config_build with
  | exception Not_found ->
      begin
        match Misc.find_file Autofonce_config.Globals.project_config_source with
        | exception Not_found ->
            Printf.eprintf "Error: files %S or %S not found in top dirs\n%!"
              Autofonce_config.Globals.project_config_build
              Autofonce_config.Globals.project_config_source ;
            Printf.eprintf
              "  Use `autofonce init` to create a file %S.\n"
              Autofonce_config.Globals.project_config_build ;
            exit 2
        | file ->
            Autofonce_config.Project_config.from_file file
      end
  | file ->
      Autofonce_config.Project_config.from_file file

let read run_dir p tc =
  let testsuite_file = p.project_source_dir // tc.config_file in
  if not (Sys.file_exists testsuite_file) then
    Misc.error "Could not find testsuite file %S in project" testsuite_file ;
  let path = List.map (fun path ->
      p.project_source_dir // path
    ) tc.config_path in
  Printf.eprintf "Loading tests from file %S\n%!" testsuite_file ;
  let suite = Parser.read ~path testsuite_file in
  run_dir, p, tc, suite

let find () =

  begin
    if not ( Sys.file_exists ( Sys.getcwd () )) then
      Misc.error "Current directory does not exist anymore. Move back up.\n%!";
  end ;

  begin
    try
      let file = Misc.find_file "autofonce.env" in
      Misc.error "File %S found. This file is deprecated, remove it and run `autofonce init`" file
    with Not_found -> ()
  end;

  let p = find_project_config () in
  let run_dir = p.project_run_dir in
  Printf.eprintf "Project description loaded from %s\n%!" p.project_file;
  let tc =
    match !Globals.testsuite with
    | None ->
        begin
          match p.project_testsuites with
          | [] -> Misc.error
                    "Project does not define any testsuite in %s !\n"
                    p.project_file
          | tc :: _ -> tc
        end
    | Some testsuite ->
        let rec iter testsuites =
          match testsuites with
          | [] ->
              Misc.error "Testsuite %S not found among testsuites in %s\n%!"
                testsuite p.project_file
          | tc :: testsuites ->
              if tc.config_name = testsuite then tc else
                iter testsuites
        in
        iter p.project_testsuites
  in
  read run_dir p tc



let exec rundir p tc suite =
  Misc.set_signal_handle Sys.sigint (fun _ -> exit 2);
  Misc.set_signal_handle Sys.sigterm (fun _ -> exit 2);

  let state = Runner_common.create_state rundir p tc suite in
  (* we are now in state_run_dir, i.e. before _autofonce/ *)

  let tests_dir = Autofonce_config.Globals.tests_dir in
  if !clean_tests_dir && Sys.file_exists tests_dir then
    Misc.remove_rec tests_dir ;

  if not ( Sys.file_exists tests_dir ) then begin
    Runner_common.output state "Creating testing directory %s\n%!"
      (Sys.getcwd () // tests_dir);
    Unix.mkdir tests_dir 0o755;
  end else begin
    Runner_common.output state "Using testing directory %s\n%!"
      (Sys.getcwd () // tests_dir);
  end;

  if !max_jobs = 1 then
    Runner_seq.exec_testsuite state
  else
    Runner_par.exec_testsuite state;

  Terminal.move_bol ();
  Printf.printf "Results:\n%!"; Terminal.erase Eol;
  Terminal.printf [] "* %d checks performed\n%!" state.state_nchecks ;
  let style =
    if state.state_tests_failed <> [] then [ Terminal.red ]
    else [ Terminal.green ]
  in
  Terminal.printf style
    "* %d / %d tests executed successfully\n%!"
    state.state_ntests_ok state.state_ntests_ran ;
  begin match state.state_tests_failed with
    | [] -> ()
    | list ->
        let nb = List.length list in
        Terminal.printf [ Terminal.red ] "* %d tests failed:" nb;
        Runner_common.print_ntests 10 (List.rev list);
        Printf.printf "\n%!";
  end;
  begin match state.state_tests_skipped with
    | [] -> ()
    | list ->
        let nb = List.length list in
        Terminal.printf [ Terminal.magenta ]
          "* %d tests were skipped\n%!" nb;
  end;
  begin match state.state_tests_failexpected with
    | [] -> ()
    | list ->
        let nb = List.length list in
        Terminal.printf [ Terminal.magenta ]
          "* %d tests were expected to fail:%!" nb;
        Runner_common.print_ntests 5 (List.rev list);
        Printf.printf "\n%!";
  end;
  let buffer = Buffer.contents state.state_buffer in
  let buffer_file = Sys.getcwd () // tests_dir // "results.log" in
  EzFile.write_file buffer_file buffer;
  Printf.eprintf "File %S created with failure results\n%!" buffer_file;
  if !print_all then
    Terminal.printf [ Terminal.magenta ] "%s\n%!" buffer;
  List.length state.state_tests_failed

let print_test _c t =
  Printf.printf "%04d %-50s %s\n%!" t.test_id t.test_name
    ( Parser.name_of_loc t.test_loc );
  ()

let print c =
  let current_banner = ref "" in
  Filter.select_tests
    (fun t ->
       if t.test_banner <> !current_banner then begin
         Printf.eprintf "\n%s\n\n%!" t.test_banner;
         current_banner := t.test_banner
       end;
       print_test c t;
    ) c

let args = [

  [ "t" ; "testsuite" ], Arg.String (fun s -> testsuite := Some s),
  EZCMD.info
    ~env:(EZCMD.env "AUTOFONCE_TESTSUITE")
    ~docv:"TESTSUITE" "Name of the testsuite to run (as specified in 'autofonce.toml')";

]
