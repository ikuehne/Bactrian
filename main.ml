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

let check_matched s =
   let rec aux = function
      | [] -> 
         begin function
            | 0 -> `Good
            | _ -> `Incomplete
         end
      | '(' :: xs -> fun n -> aux xs (n + 1)
      | ')' :: xs -> 
         begin function
            | n when n < 1 -> `Bad
            | n -> aux xs (n - 1)
            end
      | _ :: xs -> aux xs in
   aux (String.to_list s) 0


(* Lex a string. Used for the REPL; a string is input and a list of tokens
 * is returned. *)
let lex strng = 
   let lexbuf = Lexing.from_string strng in
   let rec loop tokens =
      let token = Lexer.lex lexbuf in
      match token with
      | Yacc.TOK_EOF -> tokens
      | other   -> loop (other :: tokens) in
   List.rev (loop [])

(* Read an expression from input. Input stops when the use presses enter and
 * the parentheses are matched. *)
let read_input in_channel =
   let rec input_aux previous_lines =
      (* Keep checking if parentheses are matched; if they are, process the
       * line. *)
      let new_line = input_line in_channel in
      let line = previous_lines ^ new_line in
      match check_matched line with
         | `Good       -> line
         | `Incomplete -> input_aux (previous_lines ^ " " ^ new_line ^ " ")
         | `Bad        -> 
            raise (Errors.Syntax_Error "Mismatched parentheses.") in
   input_aux ""

(* Take in an expression from in_channel and return the corresponding s-expression. *)
let parse_input in_channel =
   let lines  = read_input in_channel in
   let lexbuf = Lexing.from_string lines in
   Yacc.parse Lexer.lex lexbuf

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
                     


let env = Env.make None

let repl_loop env =
   let rec loop env =
      print_string (green "=> ");
      flush stdout;
      begin try execute_expression env (parse_input stdin)
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
      end;
      loop env in
   Primitives.load env;
   loop env

let run_program infile =
   let lexbuf = Lexing.from_channel infile in
   let env     = Env.make None in
   let rec loop env =
      let sexpr  = Yacc.parse Lexer.lex lexbuf in
         match sexpr with
            | None -> ()
            | Some s ->
               let expr = Ast.ast_of_sexpr s in
               let s = Eval.eval expr env in
               begin match s with 
                  | Error es -> List.iter es ~f:Errors.print;
                       flush stderr;
                       loop env
                  | _ -> ()
               end;
               loop env
   in
      Primitives.load env;
      loop env


(* Entry point of the interpreter. *)
let _ = 
   if Array.length Sys.argv <> 2 then
      begin
         print_endline (blue "Welcome to bogoscheme v. 0.0! ");
         repl_loop env
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
                       Printf.fprintf stderr "%s %s\n" (red "Name Error: ") e
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
