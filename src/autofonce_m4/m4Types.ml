(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

type location = {
  file : string ;
  line : int ;
  char : int ;
}

exception Error of string * location

type token =
  | QUOTED of string
  | IDENT of string
  | LPAREN
  | RPAREN
  | COMMA
  | EOF
  | SHELL of string

type value =
  | Block of macro list
  | Quoted of string

and expression =
  | Macro of string * value list
  | Shell of string

and macro = {
  exp : expression ;
  loc : location ;
}
