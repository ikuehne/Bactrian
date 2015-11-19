(* 
 * Copyright 2015 Ian Kuehne.
 *
 * Email: ikuehne@caltech.edu
 *
 * This file is part of Bactrian.
 *
 * Bactrian is free software: you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 *
 * Bactrian is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * Bactrian.  If not, see <http://www.gnu.org/licenses/>.
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
         | Yacc.TOK_EOF -> "TOK_EOF" :: accum
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
