(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

val set_verbosity : int -> unit
val get_verbosity : unit -> int

val run_cmd : Ezcmd.V2.EZCMD.TYPES.sub
val list_cmd : Ezcmd.V2.EZCMD.TYPES.sub
val init_cmd : Ezcmd.V2.EZCMD.TYPES.sub
