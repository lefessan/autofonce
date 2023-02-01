(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

open Ezcmd.V2

let cmd =
  let args =
    Testsuite.args @
    Filter.args @
    [
    ]
  in
  EZCMD.sub
    "list"
    (fun () ->
       let c = Autofonce_core.Parser.find !Globals.testsuite in
       Testsuite.print c
    )
    ~args
    ~doc: "Print testsuite of the current project"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P {|List the tests, with their numeric identifier, their name and their location in the testsuite files.|}
      ];
    ]
