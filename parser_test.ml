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
    Unit test for the Yacc module.
 
    Implements a regression test as a command-line application using the
    Unit_test functor.
 *)

open Core.Std

module T = struct
   let test_folder      = "bs_tests"
   let test_extension   = ".bs"
   let result_folder    = "parser_results"
   let result_extension = ".par"

   let parse = Yacc.parse Lexer.lex

   (* Get a buffer from a filename. *)
   let buffer_from_file fname = 
      let test_file = In_channel.create ~binary:false fname in
      let result = Lexing.from_channel test_file in
      result

   let buffer_to_string_list buffer = 
      let rec aux accum =
         match parse buffer with
         | None -> accum
         | Some sexpr -> aux (Sexpr.to_string sexpr :: accum)
      in
      List.rev (aux [])

   (* Get a list of strings representing tokens by lexing the provided in_channel
    * and converting the tokens to strings. *)
   let load_test f = f |> buffer_from_file |> buffer_to_string_list

   let tests =
      let test_list = In_channel.create ~binary:false "parser_test_list" in
      let result    = In_channel.input_lines ~fix_win_eol:false test_list in
      In_channel.close test_list;
      result

   let name = "parser"

end

module M = Unit_test.Make(T)

let () = Command.run M.command
