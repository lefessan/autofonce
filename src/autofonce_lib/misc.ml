(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

open Autofonce_m4
open M4Types

open Ez_file.V1
open EzFile.OP

let _TODO_ loc s =
  Printf.kprintf failwith "Feature not implemented %s at %s" s loc

let set_signal_handle signal handle =
  ignore (Sys.set_signal signal (Sys.Signal_handle (fun _ -> handle ())))

let error fmt =
  Printf.kprintf (fun s -> raise (Types.Error s)) fmt

let macro_error macro fmt =
  Printf.kprintf (fun s ->
      error "Error %s at %s, in macro %s" s
        (M4Printer.string_of_location macro.loc)
        (M4Printer.string_of_macro macro)
    ) fmt

let command fmt =
  Printf.kprintf (fun cmd ->
      let retcode = Sys.command cmd in
      assert ( retcode = 0 )
    ) fmt

let command_ fmt =
  Printf.kprintf (fun cmd ->
      ignore ( Sys.command cmd )
    ) fmt

let remove_rec dir = command "rm -rf %s" dir

let remove_all dir = command "rm -rf %s/*" dir

let find_file file =
  let rec iter dirname =
    let filename = dirname // file in
    if Sys.file_exists filename then filename else
      let newdir = Filename.dirname dirname in
      if newdir = dirname then
        raise Not_found
      else
        iter newdir
  in
  iter ( Sys.getcwd () )
