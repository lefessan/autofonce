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

exception FAILED of checker * string
exception SKIPPED_FAIL of checker * string
exception SKIP

let failed job s = raise (FAILED (job, s))

let rec exec_action_or_check ter action =
  match action with
  | AT_SKIP -> raise SKIP
  | AT_CHECK check ->
      let cer = Runner_common.start_check ter check in
      let ret_pid, status = Call.wait_pids () in
      let retcode =
        assert (ret_pid = cer.checker_pid );
        let ret_code =
          match status with
          | WEXITED n -> n
          | WSIGNALED _ -> -1 (* TODO: what ? *)
          | WSTOPPED _ ->
              ( try Unix.kill Sys.sigkill cer.checker_pid with _ -> ());
              ( try Unix.kill Sys.sigcont cer.checker_pid with _ -> ());
              -1
        in
        ret_code
      in
      let failures = Runner_common.check_failures cer retcode in
      begin
        match failures with
        | [] -> (* SUCCESS *)
            List.iter (exec_action_or_check ter) check.check_run_if_pass
        | failures ->
            begin
              match check.check_run_if_fail with
              | [] ->
                  let failures = String.concat " " failures in
                  if retcode = 77 then
                    raise (SKIPPED_FAIL (cer, failures))
                  else
                    failed cer failures
              | actions ->
                  List.iter (exec_action_or_check ter) actions
            end
      end

  | _ -> Runner_common.exec_action_no_check ter action

let exec_test state t =
  let ter = Runner_common.start_test state t in
  begin try
      List.iter (exec_action_or_check ter) t.test_actions;
      Runner_common.test_is_ok ter
    with
    | FAILED (job,s) ->
        Runner_common.test_is_failed job s
    | SKIPPED_FAIL (job,s) ->
        Runner_common.test_is_skipped_fail job s
    | SKIP ->
        Runner_common.test_is_skip ter
  end;
  ()

let exec_testsuite state =
  let c = state.state_suite in
  Filter.select_tests (fun t ->
      if t.test_banner <> state.state_banner then begin
        Runner_common.output "%s" t.test_banner;
        state.state_banner <- t.test_banner
      end;
      exec_test state t;
    ) c
