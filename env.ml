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

type value = 
   | Val_unit
   | Val_bool   of bool
   | Val_int    of int
   | Val_float  of float
   | Val_char   of char
   | Val_string of string
   | Val_id     of string
   | Val_cons   of value * value
   | Val_prim   of (t -> value list -> value)
   | Val_lambda of t * string list * string option * Check_ast.t list

and t = { parent: t option; bindings: value String.Table.t }

let rec string_of_value =
   let rec aux_cons = function
      | Val_cons (x1, Val_unit) -> (string_of_value x1) ^ ")"
      | Val_cons (x1, x2) -> (string_of_value x1) ^ " " ^ (aux_cons x2)
      | other             -> ". " ^ (string_of_value other) ^ ")" in
   function
   | Val_unit          -> "#u"
   | Val_bool true     -> "#t"
   | Val_bool false    -> "#f"
   | Val_int i         -> string_of_int i
   | Val_float f       -> Float.to_string f
   | Val_char c        -> "#\\" ^ String.make 1 c
   | Val_string s      -> "\"" ^ s ^ "\""
   | Val_id s           -> s
   | (Val_cons _) as c -> "(" ^ (aux_cons c)
   | Val_prim _
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
   | Val_prim _ 
   | Val_lambda _   -> "Lambda"

(* Environments. *)

let make parent = { parent = parent; bindings = String.Table.create ()
                                                                    ~size:20 }

let rec lookup env name = 
   let { parent = p; bindings = b } = env in
   let open Option.Monad_infix in
   match Hashtbl.find b name with
   | None   -> p >>= (fun e -> lookup e name)
   | x -> x

let add env key data = 
   Hashtbl.set env.bindings ~key ~data

let add_all env names values = 
   match List.zip names values with
   | Some l -> List.iter ~f:(fun (x, y) -> add env x y) l
   | None   -> raise (Errors.Invalid_Args ("[lambda expression]",
                                           List.length names,
                                           List.length values))
