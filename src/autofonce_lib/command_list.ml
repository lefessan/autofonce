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
       let suite = Testsuite.find () in
       Testsuite.print suite
    )
    ~args
    ~doc: "Print testsuite of the current project"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P {|List the tests, with their numeric identifier, their name and their location in the testsuite files.|}
      ];
    ]
