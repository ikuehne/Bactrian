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

(**
    Unit test for the Ast module.
 
    Implements a regression test as a command-line application using the
    Unit_test functor.
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
