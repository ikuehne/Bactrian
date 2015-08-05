(*
 * main.ml
 *
 *     Entry point to the bogoscheme interpreter.
 *
 *     Ian Kuehne, 2015.
 *
 *)

(* 
 * Helper functions for REPL.
 *)

open Errors
open Core.Std

(* Return a new string that will print red. *)
let red s = "\027[31;1m" ^ s ^ "\027[0m"
(* Return a new string that will print blue. *)
let blue s = "\027[34m" ^ s ^ "\027[0m"
(* Return a new string that will print green. *)
let green s = "\027[32m" ^ s ^ "\027[0m"

let execute_expression env = function
   | None       -> ()
   | Some sexpr -> let value = Eval.eval (Ast.ast_of_sexpr sexpr) env in
                   match value with
                   | Ok value ->
                      print_string (blue "->> ");
                      print_endline (Env.string_of_value value)
                   | Error es -> List.iter es ~f:Errors.print;
                     flush stderr


let repl_loop () =
   let rec loop env =
      print_string (green "=> ");
      flush stdout;
      begin try execute_expression env (Parser.sexpr_from_channel stdin)
      with Failure f ->
              Printf.fprintf stderr "%s %s\n" (red "Error: ") f;
              flush stderr
         | Syntax_Error s ->
              Printf.fprintf stderr "%s %s\n" (red "Syntax Error: ") s;
              flush stderr
         | Name_Error e ->
              Printf.fprintf stderr "%s %s not bound.\n"
                                    (red "Name Error: ") e;
              flush stderr
         | Invalid_Args (f, e, r) ->
              let plural = if e = 1 then ""
                                    else "s" in
              Printf.fprintf stderr "%s Expected %d argument%s to %s; got %d.\n"
                                    (red "Argument Error: ")
                                    e plural f r;
              flush stderr
         | Type_Error (e, r) ->
              Printf.fprintf stderr "%s Expected %s, got %s.\n"
                                  (red "Type error: ") e r;
              flush stderr
      end;
      loop env in
   let env = Env.make None in
   Primitives.load env;
   loop env

let run_program infile =
   let env = Env.make None in 
   Primitives.load env;
   ignore (
   Sequence.iter (Parser.stream_from_channel infile)
   ~f:(fun sexpr ->
   let expr = Ast.ast_of_sexpr sexpr in
   let s = Eval.eval expr env in
   match s with 
      | Error es -> List.iter es ~f:Errors.print;
                    flush stderr
      | _ -> ()))


(* Entry point of the interpreter. *)
let () = 
   if Array.length Sys.argv <> 2 then
      begin
         print_endline (blue "Welcome to bogoscheme v. 0.0! ");
         repl_loop ()
      end
   else
      let infile = In_channel.create Sys.argv.(1) in
         begin
            try
               run_program infile
            with e -> begin
               match e with
                  | Failure f ->
                       Printf.fprintf stderr "%s %s\n" (red "Error: ") f
                  | Syntax_Error s -> 
                       Printf.fprintf stderr "%s %s\n" (red "Syntax Error: ") s
                  | Name_Error e ->
                       Printf.fprintf stderr "%s %s\n" (red "Name Error: Undefined name: ") e
                  | Invalid_Args (f, e, r) ->
                       let plural = if e = 1 then ""
                                             else "s" in
                       Printf.fprintf stderr "%s Expected %d argument%s to %s; got %d.\n"
                                             (red "Argument Error: ")
                                             e plural f r
                  | e -> raise e
               end;
               In_channel.close infile;
               exit 1
         end;
         In_channel.close infile;
         exit 0
