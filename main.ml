(* * Copyright 2015 Ian Kuehne.
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

open Errors
open Core.Std

(* Return a new string that will print blue. *)
let blue s = "\027[34m" ^ s ^ "\027[0m"
(* Return a new string that will print green. *)
let green s = "\027[32m" ^ s ^ "\027[0m"

(* Execute an S-expression using the given environment.  May modify the
 * environment.  If a value is produced, it prints an arrow followed by the
 * value.  If any errors are thrown, it prints them to stderr and does nothing.
 * Returns (). *)
let execute_expression env = function
   | None       -> ()
   | Some sexpr -> let value = Env.eval (Ast.ast_of_sexpr sexpr) env in
                   match value with
                   | Ok value ->
                      print_string (blue "->> ");
                      print_endline (Env.string_of_value value)
                   | Error es -> List.iter es ~f:Errors.print;
                                 flush stderr

(* Take an evaluation and either print the associated errors or ignore the
 * value. *)
let ignore_val = function
   | Error es -> List.iter es ~f:Errors.print;
                 flush stderr
   | _ -> ()

(* Execute the given program file (given as a channel), loading values it
 * produces in the given environment. *)
let load_program env infile =
   Sequence.iter (Parser.stream_from_channel infile)
      ~f:(fun sexpr ->
         let expr = Ast.ast_of_sexpr sexpr in
         ignore_val (Env.eval expr env))

(* Run the program given in a fresh environment, and return nothing. *)
let run_program infile = 
   Sequence.iter (Parser.stream_from_channel infile)
      ~f:(fun sexpr ->
         let expr = Ast.ast_of_sexpr sexpr in
         let s = Env.eval expr Primitives.initial in
         match s with 
            | Error es -> List.iter es ~f:Errors.print;
                          flush stderr
            | _ -> ())

(* Create a new environment and run an interactive REPL in it. *)
let repl_loop () =
   let rec loop env =
      print_string (green "=> ");
      flush stdout;
      begin
         try execute_expression env (Parser.sexpr_from_channel stdin)
         with e -> print_exn e
      end;
      loop env in
   loop Primitives.initial

(* Entry point of the interpreter. *)
let () = 
   if Array.length Sys.argv <> 2 then
      begin
         print_endline (blue "Welcome to Bactrian v. 0.0! ");
         repl_loop ()
      end
   else
      let infile = try In_channel.create Sys.argv.(1)
                   with e -> print_exn e; exit 1 in
      begin
         try
            run_program infile;
            In_channel.close infile;
            exit 0
         with e -> print_exn e;
                   In_channel.close infile;
                   exit 1
      end
