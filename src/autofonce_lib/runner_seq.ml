(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

open EzCompat (* for IntMap *)
open Types

exception FAILED of check * string
exception SKIPPED_FAIL of check * string
exception SKIP

let failed job s = raise (FAILED (job, s))

let rec exec_action_or_check t action =
  match action with
  | AT_SKIP -> raise SKIP
  | AT_CHECK job ->
      let pid = Runner_common.start_check job in
      let ret_pid, status = Call.wait_pids () in
      let retcode =
        assert (ret_pid = pid );
        let ret_code =
          match status with
          | WEXITED n -> n
          | WSIGNALED _ -> -1 (* TODO: what ? *)
          | WSTOPPED _ ->
              ( try Unix.kill Sys.sigkill pid with _ -> ());
              ( try Unix.kill Sys.sigcont pid with _ -> ());
              -1
        in
        ret_code
      in
      let failures = Runner_common.check_failures job retcode in
      begin
        match failures with
        | [] -> (* SUCCESS *)
            List.iter (exec_action_or_check t) job.run_if_pass
        | failures ->
            begin
              match job.run_if_fail with
              | [] ->
                  let failures = String.concat " " failures in
                  if retcode = 77 then
                    raise (SKIPPED_FAIL (job, failures))
                  else
                    failed job failures
              | actions ->
                  List.iter (exec_action_or_check t) actions
            end
      end

  | _ -> Runner_common.exec_action_no_check t action

let exec_test t =
  Runner_common.start_test t;
  begin try
      List.iter (exec_action_or_check t) t.actions;
      Runner_common.test_is_ok t
    with
    | FAILED (job,s) ->
        Runner_common.test_is_failed job s
    | SKIPPED_FAIL (job,s) ->
        Runner_common.test_is_skipped_fail job s
    | SKIP ->
        Runner_common.test_is_skip t
  end;
  ()

let exec_testsuite c =
  Filter.select_tests (fun t ->
      if t.banner <> c.current_banner then begin
        Runner_common.output "%s" t.banner;
        c.current_banner <- t.banner
      end;
      exec_test t;
    ) c
