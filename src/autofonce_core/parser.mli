(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

(* Read filename to get the corresponding testsuite *)
val read : string -> Types.suite

(* Try to find filename in the upper directories, and read the testsuite *)
val find : string -> Types.suite
