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
open Globals (* toplevel references *)

type scheduler = {
  test_fifo : test Queue.t ;
  mutable running_tests : running_test IntMap.t ;
  mutable current_jobs : int ;
}

and running_test = {
  scheduler : scheduler ;
  running_test : test ;
  mutable waiting_actions : action list list ;
  mutable current_check : check option ;
}

let rec schedule_job r =
  match r.waiting_actions with
  | [] -> Runner_common.test_is_ok r.running_test
  | actions :: action_queue ->
      match actions with
      | [] ->
          r.waiting_actions <- action_queue ;
          schedule_job r
      | action :: actions ->
          r.waiting_actions <- actions :: action_queue ;
          schedule_action r action

and schedule_action r action =
  if !Globals.verbose > 1 then Printf.eprintf "schedule_action\n%!";
  match action with
  | AT_SKIP_IF "true" -> Runner_common.test_is_skip r.running_test
  | AT_SKIP_IF _ -> Runner_common.test_is_skip r.running_test
  (* TODO : run a shell to understand if useful *)
  | AT_CHECK check ->
      let pid = Runner_common.start_check check in
      if !Globals.verbose > 1 then Printf.eprintf "JOB %d STARTED\n%!" pid;
      r.current_check <- Some check ;

      let s = r.scheduler in
      s.current_jobs <- s.current_jobs + 1;
      s.running_tests <- IntMap.add pid r s.running_tests

  | _ ->
      Runner_common.exec_action_no_check r.running_test action;
      schedule_job r

  let schedule_test s t =
    if !Globals.verbose > 1 then Printf.eprintf "schedule_test\n%!";
    let r = {
      scheduler = s ;
      running_test = t ;
      waiting_actions = [ t.actions ] ;
      current_check = None ;
    } in
    Runner_common.start_test t;
    schedule_job r

  and job_terminated r retcode =
    if !Globals.verbose > 1 then Printf.eprintf "job_terminated\n%!";
    match r.current_check with
    | None -> assert false
    | Some check ->
        let failures = Runner_common.check_failures check retcode in
        match failures with
        | [] -> (* SUCCESS *)
            r.waiting_actions <- check.run_if_pass :: r.waiting_actions;
            schedule_job r
        | failures ->
            match check.run_if_fail with
            | [] ->
                let failures = String.concat " " failures in
                if retcode = 77 then
                  Runner_common.test_is_skipped_fail check failures
                else
                  Runner_common.test_is_failed check failures
            | actions ->
                r.waiting_actions <- actions :: r.waiting_actions;
                schedule_job r

let run s =
  let rec iter () =
    if !Globals.verbose > 1 then Printf.eprintf "iter %d\n%!" s.current_jobs;
    if s.current_jobs < !max_jobs && not (Queue.is_empty s.test_fifo) then
      let t = Queue.take s.test_fifo in
      schedule_test s t;
      iter ()
    else
    if s.current_jobs > 0 then
      let pid, status = Call.wait_pids () in
      if !Globals.verbose > 1 then Printf.eprintf "JOB %d finished\n%!" pid;
      let ret_code =
        match status with
        | WEXITED n -> n
        | WSIGNALED _ -> -1 (* TODO: what ? *)
        | WSTOPPED _ ->
            ( try Unix.kill Sys.sigkill pid with _ -> ());
            ( try Unix.kill Sys.sigcont pid with _ -> ());
            -1
      in
      let job = IntMap.find pid s.running_tests in
      s.running_tests <- IntMap.remove pid s.running_tests;
      s.current_jobs <- s.current_jobs - 1;
      job_terminated job ret_code;
      iter ()
  in
  iter ()

let exec_testsuite c =
  let s = {
    test_fifo = Queue.create () ;
    running_tests = IntMap.empty;
    current_jobs = 0;
  } in
  let select_test t =
    Queue.add t s.test_fifo;
  in
  Filter.select_tests select_test c;
  c.ntests <- Queue.length s.test_fifo ;
  run s
