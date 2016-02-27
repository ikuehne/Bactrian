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
    Abstract syntax tree for Bactrian.
 *)

open Core.Std

(** Type of AST expressions. *)
type t =
   | Unit
   | Bool   of bool
   | Int    of (int,  Errors.t)  Result.t
   | Float  of float
   | Char   of (char, Errors.t) Result.t
   | String of string
   | ID     of string
   | Define of (string * t, Errors.t) Result.t
   | If     of t * t * t
   | Macro  of (lambda, Errors.t) Result.t
   | Lambda of (lambda, Errors.t) Result.t
   | Apply  of (t * t list, Errors.t) Result.t
   | Quote  of Sexpr.t
and lambda = { args:    string list;
               var_arg: string option;
               code:    t list }

(** Make an s-expression from an AST. *)
val to_sexp : t -> Sexp.t

(** Retrieve the data from an Sexp.t. Raises a Sexplib.Conv.Of_sexp_error on
    improper input. *)
val of_sexp : Sexp.t -> t

(** Show an AST as a Sexp-like string. *)
val to_string : t -> string

(** Retrieve an AST from a Sexp-like string. *)
val of_string : string -> t

(** 'load file' gets a list of 't's from the file named 'file' containing
    properly formatted s-expressions as strings. *)
val load : string -> t list

(** Convert an S-expression into an AST expression. *)
val ast_of_sexpr : Sexpr.t -> t
