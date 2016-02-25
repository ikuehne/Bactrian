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

open Errors
open Core.Std

(* Default starting size for an environment. *)
let env_size = 20

type value = 
   | Val_unit
   | Val_bool   of bool
   | Val_int    of int
   | Val_float  of float
   | Val_char   of char
   | Val_string of string
   | Val_id     of string
   | Val_cons   of value * value
   | Val_nil
   | Val_lambda of (t -> value list -> value)

and t = value String.Table.t list

let rec string_of_value =
   let rec aux_cons = function
      | Val_nil           -> ")"
      | Val_cons (x1, x2) -> (string_of_value x1) ^ " " ^ (aux_cons x2)
      | other             -> ". " ^ (string_of_value other) ^ ")" in
   function
   | Val_unit          -> "#u"
   | Val_nil           -> "()"
   | Val_bool true     -> "#t"
   | Val_bool false    -> "#f"
   | Val_int i         -> string_of_int i
   | Val_float f       -> Float.to_string f
   | Val_char c        -> "#\\" ^ String.make 1 c
   | Val_string s      -> "\"" ^ s ^ "\""
   | Val_id s           -> s
   | (Val_cons _) as c -> "(" ^ (aux_cons c)
   | Val_lambda _      -> "lambda expression"

let type_of_value = function
   | Val_unit       -> "Unit"
   | Val_bool _     -> "Bool"
   | Val_int _      -> "Int"
   | Val_float _    -> "Float"
   | Val_char _     -> "Char"
   | Val_string _   -> "String"
   | Val_id _       -> "Identifier"
   | Val_cons _     -> "List"
   | Val_lambda _   -> "Lambda"
   | Val_nil        -> "Nil"

(* 
 * Environment operations.
 *)

let make =
   let new_frame = String.Table.create () ~size:env_size in
   function
   | None   -> [new_frame]
   | Some p -> new_frame :: p

let rec lookup env name = 
   match env with
   | [] -> None
   | x :: p -> begin
       match Hashtbl.find x name with
       | None -> lookup p name
       | x    -> x
   end

let add env key data = Hashtbl.set (List.hd_exn env) ~key ~data

let add_all env names values = 
   match List.zip names values with
   | Some l -> List.iter ~f:(fun (x, y) -> add env x y) l
   | None   -> raise (Errors.Invalid_Args ("[lambda expression]",
                                           List.length names,
                                           List.length values))


(*
 * Evaluation of expressions.
 *)

let cons_of_list = List.fold_right ~init:Val_nil
                                   ~f:(fun x y -> Val_cons (x, y))

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
      | Atom.ID s           -> Val_id s in

   function
   | Sexpr.Atom a  -> eval_atom a
   | Sexpr.List l  -> cons_of_list (List.map ~f:quote_to_list l)
   | Sexpr.Quote q -> quote_to_list q


let rec eval_checked ast env =

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
      | Check_ast.Lambda l -> Val_lambda (function_of_lambda l env)
      | Check_ast.Apply (f, args) ->
         (* Evaluate all the arguments. *)
         let operands = List.map ~f:(fun x -> eval_checked x env) args in
         begin
            (* Evaluate the function. *)
            match eval_checked f env with
               | Val_lambda f -> f env operands
               | x -> raise (Syntax_Error ("Value "
                                         ^ (string_of_value x)
                                         ^ " is not a function; "
                                         ^ "cannot apply."))
         end
      | Check_ast.Quote s -> quote_to_list s
and function_of_lambda {Check_ast.args; var_arg; code} env =
   let closure = String.Table.create () ~size:env_size :: env in
   let rec eval_lambda = function
      | [] -> Val_unit
      | [x] -> eval_checked x closure
      | x :: xs -> ignore (eval_checked x closure);
                   eval_lambda xs in
   fun _ arguments ->
      begin
         match var_arg with
         | None -> begin
                      try
                         add_all closure args arguments;
                      with Invalid_argument _ ->
                          raise (Invalid_Args ("<lambda expression>",
                                               List.length args,
                                               List.length arguments))
                   end
         | Some args -> let arg_list = cons_of_list arguments in
                        add closure args arg_list;
      end;
      eval_lambda code


let eval ast env =
   let checked_ast = Check_ast.check ast in
   match checked_ast with
   | Ok ast -> Ok (eval_checked ast env)
   | (Error _) as x -> x

