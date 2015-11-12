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
    Atoms for s-expressions.
 
    Atom contains the type representing possible atoms in s-expressions. 
    Used to define s-expressions in Sexpr. Serialization capabilities are
    also provided.
 *)

open Core.Std

type t =
   | Unit
   | Bool of bool
   | Int  of (int, Errors.t)  Result.t
   | Float  of float
   | Char of (char, Errors.t) Result.t
   | String of string
   | ID   of string with sexp

(** Make an s-expression with an Atom.t. *)
val to_sexp : t -> Sexp.t

(** Retrieve the data from an s-expression. Raises a Sexplib.Conv.Of_sexp_error
    on improper input. *)
val of_sexp : Sexp.t -> t

(** Show an atom as a Sexp-like string. *)
val to_string : t -> string

(** Retrieve an atom from a Sexp-like string. *)
val of_string : string -> t

(** 'load file' gets a list of 't's from the file named 'file' containing
    properly formatted s-expressions as strings. *)
val load : string -> t list

(** Print an atom in a human-readable way reminiscent of the input. *)
val recreate_input : t -> string
