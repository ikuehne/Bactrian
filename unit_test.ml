open Core.Std

module Make(M : sig
   val load_test : In_channel.t -> string list
   val test_folder      : string
   val test_extension   : string
   val result_folder    : string
   val result_extension : string
   val tests : string list
   val name : string
end) = struct
   open M
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

   (* Print the stream the unit produces. Useful for debugging or for generating
    * new tests when the lexer is known to be good. *)
   let print_stream filename = 
      let in_file   = In_channel.create ~binary:false filename in
      List.iter ~f:(fun s -> printf "%s\n" s)
                (load_test in_file);
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
              ~doc:("testname print results from " ^ name ^ " on the given test."))

   let command =
      Command.basic
         ~summary:("Run a unit test for the " ^ name ^ " and print results.")
         specification
         (fun g () -> match g with
          | None   -> List.iter (run_tests ()) ~f:print
          | Some f -> generate_stream f)

end
