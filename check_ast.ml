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
open Result.Monad_infix

(* Type of AST expressions without errors. *)
type t =
   | Unit
   | Bool   of bool
   | Int    of int
   | Float  of float
   | Char   of char
   | String of string
   | ID     of string
   | Define of string * t
   | If     of t * t * t
   | Lambda of lambda
   | Apply  of t * t list
   | Quote  of Sexpr.t
and lambda = { args:    string list;
               var_arg: string option;
               code:    t list }

(* Propagate errors when checking a list of checked asts. If a single list of
 * errors is found, start accumulating the errors into a Error list; 
 * otherwise, keep accumulating the checked ASTs in a Ok list. *)
let propagate asts =
   let rec propagate_bad errs = function
      | [] -> Error (List.rev errs)
      | (Ok _) :: xs    -> propagate_bad errs xs
      | (Error e) :: xs -> propagate_bad (e @ errs) xs in
   let rec propagate_good goods = function
      | [] -> Ok (List.rev goods)
      | (Ok good) :: xs -> propagate_good (good :: goods) xs
      | (Error e) :: xs -> propagate_bad e xs in
   propagate_good [] asts
   
let rec check = function

   (* Atomic constructors. *)
   | Ast.Unit           -> Ok Unit
   | Ast.Bool  b        -> Ok (Bool b)
   | Ast.Int  (Ok i)    -> Ok (Int i)
   | Ast.Int  (Error e) -> Error [e]
   | Ast.Float f        -> Ok (Float f)
   | Ast.String s       -> Ok (String s)
   | Ast.Char (Ok c)    -> Ok (Char c)
   | Ast.Char (Error e) -> Error [e]
   | Ast.ID  id -> Ok (ID id)

   (* List constructors. *)
   | Ast.Define (Ok (id, ast)) -> check ast >>= fun ast ->
                                  Ok (Define (id, ast))
   | Ast.Define (Error e)      -> Error [e]
   | Ast.If (ast1, ast2, ast3) ->
         let checked = List.map ~f:check [ast1; ast2; ast3] in
         begin
            match propagate checked with
            | Ok [ast1; ast2; ast3] ->
                  Ok (If (ast1, ast2, ast3))
            | (Error _) as x -> x
            | _ -> assert false
         end
   | Ast.Lambda (Ok l) ->
        let checked = List.map ~f:check l.code in
        begin
           match propagate checked with
           | Ok code ->
                 Ok (Lambda {args=l.args; var_arg=l.var_arg; code})
           | (Error _) as x -> x
        end
   | Ast.Lambda (Error e) -> Error [e]
   | Ast.Apply (Ok (ast, asts)) ->
         let checked = List.map ~f:check (ast :: asts) in
         begin match propagate checked with
            | Ok (ast :: asts) ->
                  Ok (Apply (ast, asts))
            | (Error _) as x -> x
            | _ -> assert false
         end
   | Ast.Apply (Error e) -> Error [e]
   | Ast.Quote s -> Ok (Quote s)
