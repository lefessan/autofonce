(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2022 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

val debug : bool ref

(* [BEGIN] The following part is similar to ocamlup:call.ml *)

val tmpfile : unit -> string
val command :
  ?on_error:(int -> unit) -> ('a, unit, string, unit) format4 -> 'a
val call :
  ?stdout:Unix.file_descr ->
  ?stderr:Unix.file_descr -> string list -> unit
val call_stdout_file : ?stderr:bool -> ?file:string -> string list -> string
val call_stdout_string : ?stderr:bool -> string list -> string
val call_stdout_lines : ?stderr:bool -> string list -> string list

(* [END] The preceeding part is similar to ocamlup:call.ml *)

val create_process :
  ?stdin:string -> ?stdout:string -> ?stderr:string -> string list -> int
val wait_pids : unit -> int * Unix.process_status
