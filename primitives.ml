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

(* 
 * Define the primitive functions. 
 *)

(* Subtract two integers. *)
let add _ = function
   | [Env.Val_int i1; Env.Val_int i2] ->
         Env.Val_int (i1 + i2)
   | [Env.Val_float f1; Env.Val_int i2] ->
         Env.Val_float (f1 +. (Float.of_int i2))
   | [Env.Val_float f1; Env.Val_float f2] ->
         Env.Val_float (f1 +. f2)
   | [Env.Val_int i1; Env.Val_float f2] ->
         Env.Val_float ((Float.of_int i1) +. f2)
   | [Env.Val_int _; v] | [v; _] ->
         raise (Type_Error ("Int", Env.type_of_value v))
   | l ->
         raise (Invalid_Args ("+", 2, List.length l))

(* Subtract two integers. *)
let mul _ = function
   | [Env.Val_int i1; Env.Val_int i2] -> Env.Val_int (i1 * i2)
   | [Env.Val_int i1; Env.Val_float f2] ->
         Env.Val_float ((Float.of_int i1) *. f2)
   | [Env.Val_float f1; Env.Val_float f2] -> Env.Val_float (f1 *. f2)
   | [Env.Val_float f1; Env.Val_int i2] -> 
         Env.Val_float (f1 *. (Float.of_int i2))
   | l -> raise (Invalid_Args ("*", 2, List.length l))

(* Subtract two integers. *)
let sub _ = function
   | [Env.Val_int i1] -> Env.Val_int (- i1)  (* Unary minus *)
   | [Env.Val_int i1; Env.Val_float f2] ->
         Env.Val_float ((Float.of_int i1) -. f2)
   | [Env.Val_float f1; Env.Val_float f2] -> Env.Val_float (f1 -. f2)
   | [Env.Val_float f1; Env.Val_int i2] -> 
         Env.Val_float (f1 -. (Float.of_int i2))
   | [Env.Val_int i1; Env.Val_int i2] -> Env.Val_int (i1 - i2)
   | l -> raise (Invalid_Args ("-", 2, List.length l))

(* Divide two integers. *)
let div _ = function
   | [Env.Val_int i1; Env.Val_int i2] -> Env.Val_int (i1 / i2)
   | [Env.Val_int i1; Env.Val_float f2] ->
         Env.Val_float ((Float.of_int i1) /. f2)
   | [Env.Val_float f1; Env.Val_float f2] -> Env.Val_float (f1 /. f2)
   | [Env.Val_float f1; Env.Val_int i2] -> 
         Env.Val_float (f1 /. (Float.of_int i2))
   | l -> raise (Invalid_Args ("/", 2, List.length l))

(* Define binary operators. *)
let eq _ = function
   | [Env.Val_int   i1; Env.Val_int   i2] -> Env.Val_bool (i1 = i2)
   | [Env.Val_int   i1; Env.Val_float f2] -> Env.Val_bool ((Float.of_int i1) = f2)
   | [Env.Val_float f1; Env.Val_float f2] -> Env.Val_bool (f1 = f2)
   | [Env.Val_float f1; Env.Val_int   i2] -> Env.Val_bool (f1 = (Float.of_int i2))
   | [v1; v2] -> Env.Val_bool (v1 = v2)
   | l -> raise (Invalid_Args ("=", 2, List.length l))
let ne _ = function
   | [Env.Val_int   i1; Env.Val_int   i2] -> Env.Val_bool (i1 <> i2)
   | [Env.Val_int   i1; Env.Val_float f2] -> Env.Val_bool ((Float.of_int i1) <> f2)
   | [Env.Val_float f1; Env.Val_float f2] -> Env.Val_bool (f1 <> f2)
   | [Env.Val_float f1; Env.Val_int   i2] -> Env.Val_bool (f1 <> (Float.of_int i2))
   | l -> raise (Invalid_Args ("!=", 2, List.length l))
let lt _ = function
   | [Env.Val_int   i1; Env.Val_int   i2] -> Env.Val_bool (i1 < i2)
   | [Env.Val_int   i1; Env.Val_float f2] -> Env.Val_bool ((Float.of_int i1) < f2)
   | [Env.Val_float f1; Env.Val_float f2] -> Env.Val_bool (f1 < f2)
   | [Env.Val_float f1; Env.Val_int   i2] -> Env.Val_bool (f1 < (Float.of_int i2))
   | l -> raise (Invalid_Args ("<", 2, List.length l))
let gt _ = function
   | [Env.Val_int   i1; Env.Val_int   i2] -> Env.Val_bool (i1 > i2)
   | [Env.Val_int   i1; Env.Val_float f2] -> Env.Val_bool ((Float.of_int i1) > f2)
   | [Env.Val_float f1; Env.Val_float f2] -> Env.Val_bool (f1 > f2)
   | [Env.Val_float f1; Env.Val_int   i2] -> Env.Val_bool (f1 > (Float.of_int i2))
   | l -> raise (Invalid_Args (">", 2, List.length l))
let le _ = function
   | [Env.Val_int   i1; Env.Val_int   i2] -> Env.Val_bool (i1 <= i2)
   | [Env.Val_int   i1; Env.Val_float f2] -> Env.Val_bool ((Float.of_int i1) <= f2)
   | [Env.Val_float f1; Env.Val_float f2] -> Env.Val_bool (f1 <= f2)
   | [Env.Val_float f1; Env.Val_int   i2] -> Env.Val_bool (f1 <= (Float.of_int i2))
   | l -> raise (Invalid_Args ("<=", 2, List.length l))
let ge _ = function
   | [Env.Val_int   i1; Env.Val_int   i2] -> Env.Val_bool (i1 >= i2)
   | [Env.Val_int   i1; Env.Val_float f2] -> Env.Val_bool ((Float.of_int i1) >= f2)
   | [Env.Val_float f1; Env.Val_float f2] -> Env.Val_bool (f1 >= f2)
   | [Env.Val_float f1; Env.Val_int   i2] -> Env.Val_bool (f1 >= (Float.of_int i2))
   | l -> raise (Invalid_Args (">=", 2, List.length l))

(* Type conversions. *)
let int_to_float _ = function
   | [Env.Val_int i] -> Env.Val_float (Float.of_int i)
   | [v] -> raise (Type_Error ("Int", Env.type_of_value v))
   | l -> raise (Invalid_Args ("int->float", 1, List.length l))

let float_to_int _ = function
   | [Env.Val_float f] -> Env.Val_int (Float.to_int f)
   | [v] -> raise (Type_Error ("Float", Env.type_of_value v))
   | l -> raise (Invalid_Args ("float->int", 1, List.length l))

(* Print a value. *)
let print _ = function
   | [value] -> 
        Printf.printf "%s" (Env.string_of_value value);
        Env.Val_unit
   | l -> raise (Invalid_Args ("print", 1, List.length l))

(* Exits the repl. *)
let exit _ = function
   | [value] -> begin match value with
                   | Env.Val_int i -> exit i
                   | _ -> raise (Type_Error ("Int", 
                                             Env.type_of_value value))
                end
   | l -> raise (Invalid_Args ("exit", 1, List.length l))

(* List construction functions. *)
let cons_of_list = List.fold_right ~init:Env.Val_unit
                                   ~f:(fun x y -> Env.Val_cons (x, y))

let rec list_of_cons = function
   | Env.Val_cons (x1, x2) -> x1 :: (list_of_cons x2)
   | Env.Val_unit          -> []
   | other                 -> raise (Type_Error ("List",
                                                 Env.type_of_value other))

let cons _ = function
   | [v1; v2] -> Env.Val_cons (v1, v2)
   | l -> raise (Invalid_Args ("cons", 2, List.length l))

let cdr _ = function
   | [Env.Val_cons (_, x)] -> x
   | [v] -> raise (Type_Error ("List", Env.type_of_value v))
   | l -> raise (Invalid_Args ("cdr", 1, List.length l))

let car _ = function
   | [Env.Val_cons (x, _)] -> x
   | [v] -> raise (Type_Error ("List", Env.type_of_value v))
   | l -> raise (Invalid_Args ("cdr", 2, List.length l))

let nil = Env.Val_unit

(* String handling functions. *)
let strindex _ = function
   | [Env.Val_int i; Env.Val_string s] -> Env.Val_char s.[i]
   | [Env.Val_int _; v2] -> raise (Type_Error ("String", Env.type_of_value v2))
   | [v1; _] -> raise (Type_Error ("Int", Env.type_of_value v1))
   | l -> raise (Invalid_Args ("string-index", 2, List.length l))

let string_to_list _ = function
   | [Env.Val_string s] -> String.to_list s
                        |> List.map ~f:(fun c -> Env.Val_char c)
                        |> cons_of_list
   | [v] -> raise (Type_Error ("String", Env.type_of_value v))
   | l -> raise (Invalid_Args ("string->list", 1, List.length l))

let list_to_string _ =
   let rec aux accum = function
      | (Env.Val_char c) :: xs -> aux (c::accum) xs
      | [] -> List.rev accum
      | v :: _ -> raise (Type_Error ("Char", Env.type_of_value v)) in
   function 
      | [l] -> l
            |> list_of_cons
            |> aux []
            |> String.of_char_list
            |> fun x -> Env.Val_string x
      | l -> raise (Invalid_Args ("list->string", 1, List.length l))

(* Evaluate a list or quoted expression. *)
let eval env l =(* function
   | [Env.Val_quote l] ->
      let result = l
                |> Ast.ast_of_sexpr
                |> fun a -> Eval.eval a env in
      begin
         match result with
         | Ok x -> x
         | Error (e::es) -> Errors.throw e
      end
   | l ->*) raise (Invalid_Args ("eval", 1, List.length l))

(* Load the primitive functions into an environment, 
   along with their names. *)
let load env =
   let ops = [ (add, "+")
             ; (sub, "-")
             ; (mul, "*")
             ; (div, "/")
             ; (eq, "=") 
             ; (ne, "!=")
             ; (lt, "<")
             ; (gt, ">")
             ; (le, "<=")
             ; (ge, ">=")
             ; (print, "print")
             ; (exit, "exit")
             ; (float_to_int, "float->int")
             ; (int_to_float, "int->float")
             ; (string_to_list, "string->list")
             ; (list_to_string, "list->string")
             ; (cdr, "cdr")
             ; (car, "car")
             ; (cons, "cons")
             ; (strindex, "string-index")
             ; (eval, "eval") ] in
   List.iter ~f:(fun (op, name) -> Env.add env name (Env.Val_prim op)) ops;
