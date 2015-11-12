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
   Convenient serialization via s-expressions.
 
   Serial provides a simple interface to use s-expressions to serialize data
   in the interpreter.
 *)

open Core.Std

(** A functor that provides convenient serialization via s-expressions. *)
module Serialized(M : sig
   (** Type of data to be serialized. *)
   type t

   (** Make an s-expression from the data to be serialized. Conveniently
       provided by the 'with sexp' syntax extension. *)
   val sexp_of_t : t -> Sexp.t

   (** Get the data out of an s-expression. Conveniently provided by the 'with
       sexp' syntax extension. Should raise a Sexplib.Conv.Of_sexp_error on
       improper input. *)
   val t_of_sexp : Sexp.t -> t

end) : sig
   (** Make an s-expression with the data. *)
   val to_sexp : M.t -> Sexp.t

   (** Retrieve the data from an s-expression. Should raise a
       Sexplib.Conv.Of_sexp_error on improper input. *)
   val of_sexp : Sexp.t -> M.t

   (** Show the data as a Sexp-like string. *)
   val to_string : M.t -> string

   (** Retrieve the data from a Sexp-like string. *)
   val of_string : string -> M.t

   (** 'load file' gets a list of 't's from the file named 'file' containing
       properly formatted s-expressions as strings. *)
   val load : string -> M.t list
end
