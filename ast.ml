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

module S = Sexpr

module T = struct
    (* Type of Bactrian expressions, representing the abstract syntax tree. *)
    type t =
       | Unit
       | Bool     of bool
       | Int      of (int,  Errors.t)  Result.t
       | Float    of float
       | Char     of (char, Errors.t) Result.t
       | String   of string
       | ID       of string
       | Define   of (string * t, Errors.t) Result.t
       | If       of t * t * t
       | Macro    of (lambda, Errors.t) Result.t
       | Lambda   of (lambda, Errors.t) Result.t
       | Apply    of (t * Sexpr.t list, Errors.t) Result.t
       | Quote    of S.t
       | Quasi    of S.t
       | Unquote  of S.t
    and lambda = { args:    string list;
                   var_arg: string option;
                   code:    t list } with sexp

end
include T
include Serial.Serialized(T)

let get_type = function
   | Unit     -> "Unit"
   | Bool     _ -> "Bool"
   | Int      _ -> "Int"
   | Float    _ -> "Float"
   | Char     _ -> "Char"
   | String   _ -> "String"
   | ID       _ -> "ID"
   | Define   _ -> "Define"
   | If       _ -> "If"
   | Lambda   _ -> "Function"
   | Macro    _ -> "Macro"
   | Apply    _ -> "Apply"
   | Quote    _ -> "Quoted Expression"
   | Quasi    _ -> "Quasiquoted Expression"
   | Unquote  _ -> "Escaped Expression"

let ok_exn = function
   | Ok x    -> x
   | Error _ -> failwith "ok_exn"

(* Get the corresponding AST expression from an atomic s-expression. *)
let ast_of_atom = function
   | Atom.Unit     -> Unit
   | Atom.Bool   b -> Bool   b
   | Atom.Int    i -> Int    i
   | Atom.Float  f -> Float  f
   | Atom.Char   c -> Char   c
   | Atom.String s -> String s
   | Atom.ID    id -> ID    id

(* Create an AST from an s-expression representing an if statement. *)
let rec ast_of_if = function
   | [a; b; c] -> If ((ast_of_sexpr a), (ast_of_sexpr b), (ast_of_sexpr c))
   | lst       -> raise (Invalid_Args ("if", 3, (List.length lst)))

(* Create an AST from an s-expression representing a lambda. *)
and ast_of_lambda = function
   | (S.Atom (Atom.ID arg)) :: (_ :: _ as body) ->
      let code   = List.map ~f:ast_of_sexpr body in
      let var_arg = Some arg in
      let lambda = {args=[]; var_arg; code} in
      Lambda (Ok lambda)
   | args :: (_ :: _ as body) ->
      begin
         match check_list args with
         | Ok args ->
            let args = List.map ~f:check_id     args in
            let code = List.map ~f:ast_of_sexpr body in
            begin
               match List.filter ~f:is_error args with
               | []      -> let args   = List.map ~f:ok_exn args in
                            let lambda = {args; var_arg=None; code} in
                            Lambda (Ok lambda)
               | e :: _ ->
                  begin
                     match e with
                     | Error e -> Lambda (Error e)
                     | _       -> assert false
                  end
            end
         | Error e -> Lambda (Error e)
      end
   | lst -> Lambda (Error (Argument ("lambda", 2, (List.length lst))))

(* Create a new macro. *)
and ast_of_macro x =
   match ast_of_lambda x with
   | Lambda f -> Macro f
   | _        -> assert false

(* Create an AST from an s-expression representing a function 
 * application. *)
and ast_of_apply = function
   | f :: args -> begin
                     match check_f f with
                     | Ok    f -> Apply (Ok (f, args))
                     | Error e -> Apply (Error e)
                  end
   | []        -> assert false

(* Check that an s-expression is an identifier, returning a Result.t with
 * an Error.Type _ if it is not. *)
and check_id sexpr =
   match ast_of_sexpr sexpr with
   | ID id -> Ok id
   | ast   -> Error (Errors.Type ("ID", get_type ast))

(* Create an AST from an s-expression representing a definition. *)
and ast_of_def = function
(* Case simple definition. *)
| [a; b] -> begin
               match a with
               | S.Atom _ ->
                  begin
                     match check_id a with
                     | Ok id   -> Define (Ok (id, ast_of_sexpr b))
                     | Error e -> Define (Error e)
                  end
               | S.List ((S.Atom (Atom.ID name)) :: args) ->
                  let lambda = ast_of_lambda ((S.List args) :: [b]) in
                  Define (Ok (name, lambda))
               | _ -> 
                  let error_type = a |> ast_of_sexpr |> get_type in
                  Define (Error (Errors.Type ("ID or ID list", error_type)))
            end
| a :: l -> begin
               match a with
               | S.List ((S.Atom (Atom.ID name)) :: args) ->
                  let lambda = ast_of_lambda ((S.List args) :: l) in
                  Define (Ok (name, lambda))
               | _ -> 
                  let error_type = a |> ast_of_sexpr |> get_type in
                  Define (Error (Errors.Type ("ID or ID list", error_type)))
            end
| [] -> Define (Error (Argument ("define", 2, 0)))

(* Check that an s-expression is a list, returning a Result.t with an
 * Error.Type _ if it is not. *)
and check_list = function
   | S.List lst         -> Ok lst
   | sexp -> Error (Errors.Type ("List", sexp 
                                      |> ast_of_sexpr
                                      |> get_type))

(* Create an AST from a list of s-expressions. *)
and ast_of_list = function
   | [] -> Unit
   | [S.Atom atom] -> ast_of_atom atom
   | (S.Atom (Atom.ID "define")) :: def  -> ast_of_def def
   | (S.Atom (Atom.ID "if")) :: if_then  -> ast_of_if if_then
   | (S.Atom (Atom.ID "macro")) :: body  -> ast_of_macro body
   | (S.Atom (Atom.ID "lambda")) :: body -> ast_of_lambda body
   | other -> ast_of_apply other

and ast_of_quote s = Quote s

and ast_of_quasi s = Quasi s

and ast_of_unquote s = Unquote s

(* Check that an s-expression could be a function, returning a Result.t with
 * an Error.Type _ if it is not. *)
and check_f f = 
   match ast_of_sexpr f with
   | (ID     _) as id    -> Ok id
   | (Lambda _) as l     -> Ok l
   | (Macro _)  as m     -> Ok m
   | (Apply  _) as apply -> Ok apply
   | other -> Error (Errors.Type ("Lambda", get_type other))

(* Generate an abstract syntax tree from an s-expression. *)
and ast_of_sexpr = function
   | S.Atom atom -> ast_of_atom    atom
   | S.List lst  -> ast_of_list    lst
   | S.Quote q   -> ast_of_quote   q
   | S.Quasi q   -> ast_of_quasi   q
   | S.Unquote u -> ast_of_unquote u
