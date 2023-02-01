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

val init : ?loc:M4Types.location -> Lexing.lexbuf -> unit

val unescape : ?last:bool -> Lexing.lexbuf -> string

val token : Lexing.lexbuf -> M4Types.location * M4Types.token

val location : Lexing.lexbuf -> M4Types.location
