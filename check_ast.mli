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

(**
    Pulling errors out of general ASTs.
 *)

open Core.Std

(** Type of AST expressions without errors. *)
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

(** Take an AST potentially with error types scattered throughout and return 
    either an AST guaranteed to have no errors, or a list of errors found. *)
val check : Ast.t -> (t, Errors.t list) Result.t
