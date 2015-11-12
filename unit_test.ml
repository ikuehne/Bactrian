(* 
 * Copyright 2015 Ian Kuehne.
 *
 * Email: ikuehne@caltech.edu
 *
 * This file is part of Bogoscheme.
 *
 * Bogoscheme is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * Bogoscheme is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * Bogoscheme.  If not, see <http://www.gnu.org/licenses/>.
 *
 *)

open Core.Std

module Make(M : sig
   val load_test : string -> string list
   val test_folder      : string
   val test_extension   : string
   val result_folder    : string
   val result_extension : string
   val tests : string list
   val name : string
end) = struct
   open M
   (* Get a list of strings from the correct output. *)
   let read_result = In_channel.input_lines ~fix_win_eol:false

   let test_file f1 f2 =
      let result = In_channel.create ~binary:false f2 in
      let passed = ((load_test f1) = (read_result result)) in
      In_channel.close result;
      passed

   let make_test_file_name s = test_folder ^ "/" ^ s ^ test_extension
   let make_result_file_name s = result_folder ^ "/" ^ s ^ result_extension

   (* Print the stream the unit produces. Useful for debugging or for generating
    * new tests when the lexer is known to be good. *)
   let print_stream filename = List.iter ~f:(fun s -> printf "%s\n" s)
                                         (load_test filename)

   (* Same as print_stream, but takes a test name rather than a full filename. *)
   let generate_stream filename = print_stream filename

   let run_tests () = List.map tests ~f:(fun test ->
      let message = " test for " ^ test ^ "\n" in
      let test_in = make_test_file_name test in
      let correct_out = make_result_file_name test in
      match test_file test_in correct_out with
      | true  -> "passed" ^ message
      | false -> "failed" ^ message)

   let print = Out_channel.output_string Out_channel.stdout


   (* Command line parsing functions. *)

   (* Test name type for Command.Spec.Arg_type. Only recognizes names that
    * correspond to test files. They do not necessarily have to correspond to
    * result files. *)
   let test_name = 
      Command.Spec.Arg_type.create (fun testname ->
         let filename = make_test_file_name testname in
         match Sys.is_file filename with
         | `Yes -> filename
         | `No | `Unknown ->
            eprintf "'%s' is not a valid test name.\n" testname;
            exit 1)

   let specification = Command.Spec.(
      empty 
      +> flag "-g"
              (optional test_name) 
              ~doc:("testname print results from " ^ name ^ " on the given test."))

   let command =
      Command.basic
         ~summary:("Run a unit test for the " ^ name ^ " and print results.")
         specification
         (fun g () -> match g with
          | None   -> List.iter (run_tests ()) ~f:print
          | Some f -> generate_stream f)

end
