(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

open Ez_file.V1
open M4Types

let error loc fmt =
  Printf.kprintf (fun s -> raise ( Error (s, loc))) fmt

let parse_file filename =
  let content = EzFile.read_file filename in
  let lexbuf = Lexing.from_string content in
  Lexing.set_filename lexbuf filename;
  M4Lexer.init ();

  let rec iter macros =
    expect_ident macros ( M4Lexer.token lexbuf )

  and expect_ident macros ( new_loc, token ) =
    match token with
    | EOF | COMMA | RPAREN -> ( new_loc, token ), List.rev macros
    | IDENT ident ->
        expect_lparen macros (new_loc,ident) ( M4Lexer.token lexbuf )
    | SHELL shell ->
        expect_ident ( { exp = Shell shell ; loc = new_loc } :: macros )
          ( M4Lexer.token lexbuf )
    | _ -> error new_loc "Unexpected token %S in expect_ident"
             (M4Printer.string_of_token token)

  and expect_lparen macros (loc,ident) ( new_loc, token ) =
    match token with
    | EOF | RPAREN | COMMA ->
        ( new_loc, token ),
        List.rev ( { exp = Macro (ident,[]) ; loc } :: macros )
    | IDENT new_ident ->
        expect_lparen ( { exp = Macro (ident,[]) ; loc } :: macros )
          (new_loc,new_ident) ( M4Lexer.token lexbuf )
    | SHELL shell ->
        expect_ident ( { exp = Shell shell ; loc = new_loc } ::
                       { exp = Macro (ident,[]) ; loc } :: macros )
          ( M4Lexer.token lexbuf )
    | LPAREN ->
        expect_arg_or_rparen macros (loc,ident) ( M4Lexer.token lexbuf )
    | _ -> error new_loc "Unexpected token %S in expect_lparen"
             (M4Printer.string_of_token token)

  and expect_arg_or_rparen macros (loc,ident) ( new_loc, token ) =
    match token with
    | RPAREN ->
        expect_ident (
          { exp = Macro (ident,[]) ; loc } ::macros ) ( M4Lexer.token lexbuf )
    | QUOTED arg ->
        expect_comma_or_rparen macros (loc,ident) [Quoted arg]
          ( M4Lexer.token lexbuf )
    | IDENT _ ->
        let ( new_loc, token ), arg = expect_ident [] ( new_loc, token ) in
        expect_comma_or_rparen macros (loc,ident) [Block arg]
          ( new_loc, token )
    | _ -> error new_loc "Unexpected token %S in expect_arg_or_rparen"
             (M4Printer.string_of_token token)

  and expect_comma_or_rparen macros (loc,ident) args ( new_loc, token ) =
    match token with
    | RPAREN ->
        expect_ident (
          { exp = Macro (ident,List.rev args) ; loc } ::macros )
          ( M4Lexer.token lexbuf )
    | COMMA ->
        expect_arg macros (loc,ident) args ( M4Lexer.token lexbuf )
    | _ -> error new_loc "Unexpected token %S in expect_comma_or_rparen"
             (M4Printer.string_of_token token)

  and expect_arg macros ident args ( new_loc, token ) =
    match token with
    | QUOTED arg ->
        expect_comma_or_rparen macros ident ( ( Quoted arg ) :: args)
          ( M4Lexer.token lexbuf )
    | IDENT _ ->
        let ( new_loc, token ), arg = expect_ident [] ( new_loc, token ) in
        expect_comma_or_rparen macros ident ( ( Block arg ) :: args)
          ( new_loc, token )
    | _ -> error new_loc "Unexpected token %S in expect_arg"
             (M4Printer.string_of_token token)
  in
  match iter [] with
  | exception ( Failure s ) ->
      let new_loc = M4Lexer.location lexbuf in
      raise ( M4Types.Error (s, new_loc) )
  | ( _, EOF), macros -> macros
  | ( loc, token ), _ ->
      error loc "Unexpected token %S"
        (M4Printer.string_of_token token)
