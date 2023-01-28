(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Ezcmd.V2

module PROGRAM = struct
  let command = "autofonce"
  let about = "autofonce COMMAND COMMAND-OPTIONS"
  let set_verbosity = Autofonce_lib.Main.set_verbosity
  let get_verbosity = Autofonce_lib.Main.get_verbosity
  let backtrace_var = Some "AUTOFONCE_BACKTRACE"
  let usage = "Modern runner for GNU Autoconf testsuites"
  let version = Version.version
  exception Error = Autofonce_lib.Types.Error
end
module MAIN = EZCMD.MAKE( PROGRAM )
include PROGRAM

let () =
  Printexc.record_backtrace true;
  let commands = [
    Autofonce_lib.Main.run_cmd ;
    Autofonce_lib.Main.list_cmd ;
    Autofonce_lib.Main.init_cmd ;
  ] in

  let common_args = [
  ] in

  try
    MAIN.main
      ~on_error: (fun () -> () )
      ~on_exit: (fun () -> () )
      ~print_config: (fun () -> () )
      (* ~argv *)
      commands
      ~common_args;
  with
  | PROGRAM.Error s ->
      Printf.eprintf "Error: %s\n%!" s;
      exit 2
  | exn ->
      let bt = Printexc.get_backtrace () in
      let error = Printexc.to_string exn in
      Printf.eprintf "fatal exception %s\n%s\n%!" error bt;
      exit 2
