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
    Scheme s-expressions.
 
    Sexpr contains the type representing s-expressions for the Scheme
    interpreter, not to be comfused with the serialization format from Core,
    which is contained in Sexp.
 *)

open Core.Std

(** Type of all S-expressions. *)
type t =
   | Atom of Atom.t
   | List of t list
   | Quote of t

(** Make an s-expression with an s-expression. *)
val to_sexp : t -> Sexp.t
val sexp_of_t : t -> Sexp.t

(** Retrieve the data from an Sexp.t. Raises a Sexplib.Conv.Of_sexp_error on
    improper input. *)
val of_sexp : Sexp.t -> t
val t_of_sexp : Sexp.t -> t

(** Show an s-expression as a Sexp-like string. *)
val to_string : t -> string

(** Retrieve an s-expression from a Sexp-like string. *)
val of_string : string -> t

(** 'load file' gets a list of 't's from the file named 'file' containing
    properly formatted s-expressions as strings. *)
val load : string -> t list

(** Convert a t to a human-readable string in a manner that can be used as
    input for the interpreter to get the same result. *)
val recreate_input : t -> string
