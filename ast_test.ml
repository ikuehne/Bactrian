(* 
 * ast_test.ml
 *
 *     Unit test for the Ast module.
 *
 *     Ian Kuehne, 2015.
 *
 *     Implements a regression test as a command-line application using the
 *     Unit_test functor.
 *
 *)

open Core.Std

module T = struct
   let test_folder = "parser_results"
   let test_extension = ".par"
   let result_folder = "ast_results"
   let result_extension = ".ast"

   let load_test f = f 
                   |> Sexpr.load
                   |> List.map ~f:Ast.ast_of_sexpr
                   |> List.map ~f:Ast.to_string

   let tests =
      let test_list = In_channel.create ~binary:false "ast_test_list" in
      let result    = In_channel.input_lines ~fix_win_eol:false test_list in
      In_channel.close test_list;
      result

   let name = "ast"

end

module M = Unit_test.Make(T)

let () = Command.run M.command
