(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

open M4Types

let string_of_token = function
  | QUOTED s -> Printf.sprintf "QUOTED %S" s
  | SHELL s -> Printf.sprintf "SHELL %S" s
  | IDENT s -> Printf.sprintf "IDENT %S" s
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | COMMA -> "COMMA"
  | EOF -> "EOF"

let string_of_location loc =
  Printf.sprintf "%s:%d:%d" loc.file loc.line loc.char

let rec string_of_value = function
  | Quoted s -> Printf.sprintf "%S" s
  | Block list ->
      String.concat "\n"
        ( List.map string_of_macro list )

and string_of_macro macro =
  match macro.exp with
  | Macro ( ident, args ) ->
    Printf.sprintf "Macro: %s ( %s )" ident
      ( String.concat ", "
          ( List.map string_of_value args ))
  | Shell shell ->
      Printf.sprintf "Shell: %s" shell
