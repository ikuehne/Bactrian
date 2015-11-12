(* 
 * Copyright 2015 Ian Kuehne.
 *
 * Email: ikuehne@caltech.edu
 *
 * This file is part of Bogoscheme.
 *
 * Bogoscheme is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * Bogoscheme is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * Bogoscheme.  If not, see <http://www.gnu.org/licenses/>.
 *
 *)

(**
    Checks an AST from Ast and ensures that there are no error types in
    it. Returns a checked AST with no error types and a list of errors.
 *)

open Core.Std

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
   | Lambda of string list * string option * t list
   | Apply  of t * t list
   | Quote  of Sexpr.t

val check : Ast.t -> (t, Errors.t list) Result.t
