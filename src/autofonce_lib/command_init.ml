(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro SAS                                       *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the                       *)
(*  OCAMLPRO-NON-COMMERCIAL license.                                      *)
(*                                                                        *)
(**************************************************************************)

open Ezcmd.V2
open EZCMD.TYPES
open Ez_file.V1

open Autofonce_core
open Globals (* toplevel references *)

let list_known_project () =
  Printf.printf "Known projects with environment files:\n";
  List.iter (fun file ->
      if Filename.check_suffix file ".env" then
        Printf.printf "* %S\n%!" (Filename.chop_suffix file ".env")
    ) Autofonce_share.Tree.file_list;
  Printf.printf "%!";
  exit 0

let cmd =
  let force_update = ref false in
  let project = ref None in
  let args =
    [
      [ "f"; "force-update" ], Arg.Set force_update,
      EZCMD.info "Force update of the environment file";

      [ "p"; "project" ], Arg.String (fun s -> project := Some s),
      EZCMD.info ~docv:"PROJECT" "Use environment file from known project";

      [ "l"; "list-known" ], Arg.Unit list_known_project,
      EZCMD.info "List known projects with environment files";

    ]
  in
  EZCMD.sub
    "init"
    (fun () ->
       if Sys.file_exists autotest_env && not !force_update then
         Misc.error
           "File %s already present (use -f to update)" autotest_env;

       begin
         match Misc.find_file autotest_env with
         | exception Not_found -> ()
         | file ->
             Printf.eprintf
               "Warning: %S already present in top dirs at\n %s\n%!"
               autotest_env file;
       end;

       let rec autodetect dirname =
         let basename = Filename.basename dirname in
         match Autofonce_share.Files.content ( basename ^ ".env" ) with
         | content -> (basename, content)
         | exception Not_found ->
             let parent_dirname = Filename.dirname dirname in
             if parent_dirname = dirname then raise Not_found;
             autodetect parent_dirname
       in
       let project, content =
         match
           match !project with
           | None
           | Some "auto" ->
               autodetect (Sys.getcwd ())
           | Some project ->
               match Autofonce_share.Files.content ( project ^ ".env" ) with
               | content -> (project, content)
               | exception Not_found ->
                   Misc.error "Project %S is not known" project
         with
         | (project, content) ->
             (Printf.sprintf "for project %S" project, content)
         | exception Not_found ->
             Printf.eprintf "Warning: autodetection of project failed\n%!";
             let content = {|
# set env variables for tests here
# (you can gather inspiration from existing atlocal/atconfig files)
|}
             in
             let project = "with EMPTY CONTENT." in
             (project, content)
       in
       EzFile.write_file autotest_env content;
       Printf.eprintf "File %S created %s.\n%!"
         autotest_env project;

    )
    ~args
    ~doc: "Initialize project to run the testsuite with autofonce"
    ~man:[
      `S "DESCRIPTION";
      `Blocks [
        `P {|To run tests with $(b,autofonce), tests typically require
some environment variables to be set. For that, $(b,autofonce) uses a
file named $(b,autofonce.env) in the project. $(b,autofonce) will also
use this file to create a directory $(b,_autotest/) where tests are
run and results are kept.|} ;
        `P {|This command can be used to create the file $(b,autofonce.env)
in the current directory. By default, the file is empty, and you will
need to fill it from inspecting files in the project, such as $(b,atconfig)
and $(b,atlocal), typically used by GNU Autoconf testsuites.|} ;
        `P {|Yet, in some cases, $(b,autofonce) knows the project in which
you are and can provide you will an example of $(b,autofonce.env) for that
particular project.|} ;
        `P {|You can use the following command to list known projects:|} ;
        `Pre {|\$ autofonce init --list|};
        `P {|You can then select the project using:|} ;
        `Pre {|\$ autofonce init -p gnucobol|};
        `P {|$(b,autofonce) will also inspect the path to see if it
recognize the name of a project it knows. In such cases, you won't need
to provide the project name, as it is automatically detected.|};
      ];
    ]
