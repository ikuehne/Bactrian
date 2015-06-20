(* 
 * 
 * Unit test for the ocamllex lexer.
 *
 * Most of this implementation should be factored out into a generic unit test
 * functor. The only functions that definitely belong here are:
 * 
 *    - load_test, since this is specific to what kind of stream is being used.
 *      This should possibly be modified to output a Sexp.t. For the lexer
 *      this is redundant, but other types are recursively defined and will be
 *      much more difficult to make into a list of strings that contains all of
 *      the information required.
 *
 *    - test_folder and test_extionsion, since the input types vary depending
 *      on the unit being tested. Many units can be modeled as taking the 
 *      "previous" unit's results as inputs, so hopefully these will match
 *      the previous unit's results. A good way of achieving this would be the 
 *      Sexp module; each unit could output its results as a serialized
 *      s-expression, which could easily be picked up by the next unit.
 *
 *    - result_folder and result_extension.  Same as above.
 *
 *    - tests.  Some units (the ones that don't fit into the strictly layered
 *      model described above) will use different tests, but most should be
 *      fine here.  Ideally the tests for the layered units could be factored
 *      out as metadata, but that is not urgent.
 *
 * The functor should output a 'command' value something like the one 
 * implemented below. A simple 'did_pass' function or similar would also be a
 * good idea to allow for less involved tests.
 *
 *)

open Core.Std

(* Name of folder from which to retrieve tests. *)
let test_folder = "bs_tests"
(* Extension for test files. *)
let test_extension = ".bs"

(* Name of folder from which to retrieve correct results. *)
let result_folder = "lexer_results"
(* Extension for result files. *)
let result_extension = ".res"

(* Make a short alias for the lexer function. *)
let lex = Lexer.lex

(* Get a buffer from a filename. *)
let buffer_from_file = Lexing.from_channel

(* Get a list of strings from the lexer and a buffer. *)
let get_lexer_output buf = 
   let rec aux accum =
      match lex buf with 
      | Parser.TOK_EOF -> "TOK_EOF" :: accum
      | token          -> aux ((Token.to_string token) :: accum)
   in
   List.rev (aux [])

(* Get a list of strings representing tokens by lexing the provided in_channel
 * and converting the tokens to strings. *)
let load_test f = f |> buffer_from_file |> get_lexer_output

(* Get a list of strings from the correct output. *)
let read_result f = In_channel.input_lines ~fix_win_eol:false f

let test_file f1 f2 =
   let test   = In_channel.create ~binary:false f1 in
   let result = In_channel.create ~binary:false f2 in
   let passed = ((load_test test) = (read_result result)) in
   In_channel.close test;
   In_channel.close result;
   passed

let make_test_file_name s = test_folder ^ "/" ^ s ^ test_extension
let make_result_file_name s = result_folder ^ "/" ^ s ^ result_extension

let tests =
   let test_list = In_channel.create ~binary:false "lexer_test_list" in
   let result    = In_channel.input_lines ~fix_win_eol:false test_list in
   In_channel.close test_list;
   result

(* Print the stream the lexer produces. Useful for debugging or for generating
 * new tests when the lexer is known to be good. *)
let print_stream filename = 
   let in_file   = In_channel.create ~binary:false filename in
   let in_buffer = buffer_from_file in_file in
   List.iter ~f:(fun s -> printf "%s\n" s)
             (get_lexer_output in_buffer);
   In_channel.close in_file

(* Same as print_stream, but takes a test name rather than a full filename. *)
let generate_stream filename =
   print_stream filename

let run_tests () = List.map tests ~f:(fun test ->
   let message = " lexer_test for " ^ test ^ "\n" in
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
           ~doc:"Print results from lexer on the given test.")

let command =
   Command.basic
      ~summary:"Run a unit test for the lexer and print results."
      specification
      (fun g () -> match g with
       | None   -> List.iter (run_tests ()) ~f:print
       | Some f -> generate_stream f)


let () = Command.run command
