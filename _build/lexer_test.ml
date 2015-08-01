(* 
 * lexer_test.ml
 *
 *     Unit test for the ocamllex lexer.
 *
 *     Ian Kuehne, 2015.
 *
 *     Implements a unit test using the Unit_test functor.
 *
 *)

open Core.Std

module T = struct
   (* Name of folder from which to retrieve tests. *)
   let test_folder = "bs_tests"
   (* Extension for test files. *)
   let test_extension = ".bs"

   (* Name of folder from which to retrieve correct results. *)
   let result_folder = "lexer_results"
   (* Extension for result files. *)
   let result_extension = ".lex"

   (* Make a short alias for the lexer function. *)
   let lex = Lexer.lex

   (* Get a buffer from a filename. *)
   let buffer_from_file fname = 
      let test_file = In_channel.create ~binary:false fname in
      let result = Lexing.from_channel test_file in
      result

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

   let tests =
      let test_list = In_channel.create ~binary:false "lexer_test_list" in
      let result    = In_channel.input_lines ~fix_win_eol:false test_list in
      In_channel.close test_list;
      result

   let name = "lexer"

end

module M = Unit_test.Make(T)

let () = Command.run M.command
