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

open Env
open Errors
open Core.Std

let cons_of_list = List.fold_right ~init:Val_unit ~f:(fun x y -> Val_cons (x, y))

let rec quote_to_list =
   let eval_atom = function
      | Atom.Unit           -> Val_unit
      | Atom.Bool b         -> Val_bool b
      | Atom.Int (Ok i)     -> Val_int i
      | Atom.Int (Error e)  -> Errors.throw e
      | Atom.Float f        -> Val_float f
      | Atom.Char (Ok c)    -> Val_char c
      | Atom.Char (Error e) -> Errors.throw e
      | Atom.String s       -> Val_string s
      | Atom.ID s           -> Val_string s in

   function
   | Sexpr.Atom a  -> eval_atom a
   | Sexpr.List l  -> cons_of_list (List.map ~f:quote_to_list l)
   | Sexpr.Quote q -> quote_to_list q

let rec eval_checked ast env =

   let rec eval_lambda env xs = match xs with
      | [] -> Val_unit
      | [x] -> eval_checked x env
      | x :: xs -> ignore (eval_checked x env);
                   eval_lambda env xs in

   let handle_lookup name = function
      | None   -> raise (Name_Error name)
      | Some v -> v in

   match ast with
      | Check_ast.Unit -> Val_unit
      | Check_ast.Bool b -> Val_bool b
      | Check_ast.Char c -> Val_char c
      | Check_ast.String s -> Val_string s
      | Check_ast.Int i -> Val_int i
      | Check_ast.Float f -> Val_float f
      | Check_ast.ID id -> handle_lookup id (lookup env id)
      | Check_ast.Define (id, e) -> 
         let definition = eval_checked e env in
         add env id definition;
         Val_unit
      | Check_ast.If (test_e, then_e, else_e) -> 
         begin 
            match eval_checked test_e env with
            | Val_bool true -> (eval_checked then_e env)
            | Val_bool false -> (eval_checked else_e env)
            | x -> raise (Type_Error ("ID",
                                      type_of_value x))
         end
      | Check_ast.Lambda (ids, args, exprs) -> 
         Val_lambda (env, ids, args, exprs)
      | Check_ast.Apply (f, args) ->
         (* Evaluate all the arguments. *)
         let operands = List.map ~f:(fun x -> eval_checked x env) args in
         begin
            (* Evaluate the function. *)
            match eval_checked f env with
               | Val_prim prim -> prim env operands
               | (Val_lambda (env, ids, None, exprs)) as lambda ->
                     let new_env  = make (Some env) in
                     begin
                        try
                           add_all new_env ids operands;
                        with Invalid_argument _ ->
                           raise (Invalid_Args (string_of_value lambda,
                                                List.length ids,
                                                List.length operands))
                     end;
                     eval_lambda new_env exprs
               | Val_lambda (env, [], Some args, exprs) ->
                     let new_env  = make (Some env) in
                     let arg_list = operands
                                 |> cons_of_list in
                     add new_env args arg_list;
                     eval_lambda new_env exprs
               | x -> raise (Syntax_Error ("Value "
                                         ^ (string_of_value x)
                                         ^ " is not a function; "
                                         ^ "cannot apply."))
         end
      | Check_ast.Quote s -> quote_to_list s

let eval ast env =
   let checked_ast = Check_ast.check ast in
   match checked_ast with
   | Ok ast -> Ok (eval_checked ast env)
   | (Error e) as x -> x
