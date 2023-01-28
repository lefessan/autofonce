(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021 OCamlPro & Origin Labs                             *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

{
  (* open Lexing *)
  open M4Types

let debug_lexer = match Sys.getenv "DEBUG_LEXER" with
  | exception Not_found -> false
  | _ -> true

let quoted_buffer = Buffer.create 1000

let in_macro = ref 0
let init () =
  in_macro := 0 ;
  ()

let location lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  let file = pos.Lexing.pos_fname in
  let line = pos.Lexing.pos_lnum in
  let char = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  { file ; line ; char }

}

let upper =[ 'A'-'Z' ]
let lower =[ 'a'-'z' ]
let alpha =['a'-'z' 'A'-'Z' ]
let alphanum = [ 'a'-'z' 'A'-'Z' '0'-'9' '_' ]
let space = [ '\r' '\t' ' ' ]

rule shell = parse
  | eof { location lexbuf, EOF }
  | '#' [ ^ '\n' ]* { shell lexbuf }
  | space+ { shell lexbuf }
  | '\n' { Lexing.new_line lexbuf; shell lexbuf }
  | ( alpha alphanum* as ident ) {
      let loc = location lexbuf in
      let token =
        match ident.[0] with
        | 'a'..'z' when ident <> "m4_include" ->
            SHELL (ident ^ end_of_line lexbuf)
        | _ ->
            IDENT ( Lexing.lexeme lexbuf )
      in
      ( loc, token )
    }
  | '(' { incr in_macro;
          location lexbuf, LPAREN }
  | _
      { Printf.kprintf failwith "Unexpected char %S" (Lexing.lexeme lexbuf) }

and end_of_line = parse
  | [ ^ '\n' '#' ]* as line { line }
  | '\n' { Lexing.new_line lexbuf; "" }
  | '#' [ ^ '\n' ]* { "" }
  | eof { "" }

and token = parse
  | '#' [ ^ '\n' ]* { token lexbuf }
  | [ '\r' '\t' ' ' ]+ { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | alpha alphanum* { location lexbuf, IDENT ( Lexing.lexeme lexbuf ) }
  | '(' { incr in_macro; location lexbuf, LPAREN }
  | ')' { decr in_macro; location lexbuf, RPAREN }
  | ',' { location lexbuf, COMMA }
  | "[[" {
      let loc = location lexbuf in
      Buffer.clear quoted_buffer;
      double_quoted lexbuf;
      loc, QUOTED ( Buffer.contents quoted_buffer )
    }
  | '[' {
      let loc = location lexbuf in
      Buffer.clear quoted_buffer;
      quoted lexbuf;
      loc, QUOTED ( Buffer.contents quoted_buffer )
    }
  | eof { location lexbuf, EOF }
  | _
      { Printf.kprintf failwith "Unexpected char %S" (Lexing.lexeme lexbuf) }

and quoted = parse
  | eof { failwith "Unexpected end of file in [...]" }
  | '[' ( [^ '[' ']' '\n'] as c ) ']' {
      Buffer.add_char quoted_buffer c;
      quoted lexbuf }
  | "[[" { Buffer.add_char quoted_buffer '[';  quoted lexbuf }
  | "]]" { Buffer.add_char quoted_buffer ']';  quoted lexbuf }
  | "]" { () }
  | '\n' { Lexing.new_line lexbuf;
           Buffer.add_char quoted_buffer '\n';
           quoted lexbuf }
  | "@<:@" { Buffer.add_char quoted_buffer '['; quoted lexbuf }
  | "@:>@" { Buffer.add_char quoted_buffer ']'; quoted lexbuf }
  | _ { Buffer.add_string quoted_buffer ( Lexing.lexeme lexbuf );
                          quoted lexbuf; }

and double_quoted = parse
  | eof { failwith "Unexpected end of file in [[...]]" }
  | "]]" { () }
  | '\n' { Lexing.new_line lexbuf;
           Buffer.add_char quoted_buffer '\n';
           double_quoted lexbuf }
  | ']'  { Buffer.add_char quoted_buffer ']'; double_quoted lexbuf }
  | [ ^ '\n' ']' ]+ { Buffer.add_string quoted_buffer ( Lexing.lexeme lexbuf );
                      double_quoted lexbuf; }

      {
        let token lexbuf =
          let (loc, token) =
            if !in_macro > 0 then
              token lexbuf
            else
              shell lexbuf
          in
          if debug_lexer then
            Printf.eprintf "token [%s] at %s\n%!"
              (M4Printer.string_of_token token)
              (M4Printer.string_of_location loc);
          loc, token

      }
