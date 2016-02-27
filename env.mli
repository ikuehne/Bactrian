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
    Environments and the values that they contain.
 
    Env implements a mutable type which matches identifiers to values.  The
    mapping is implemented as a hashtable with string keys.
 *)

open Core.Std
open Errors

(** Type of legal values i.e. results of evaluation. *)
type value = 
   | Val_unit
   | Val_bool    of bool
   | Val_int     of int
   | Val_float   of float
   | Val_char    of char
   | Val_string  of string
   | Val_id      of string
   | Val_cons    of value * value
   | Val_nil
   | Val_macro  of (t -> value list -> Check_ast.t)
   | Val_lambda of (t -> value list -> value)

(** Type of environments. *)
and t

(** Convert a value into a string representing its type. *)
val type_of_value : value -> string

(** Convert a value into a string representing its value. *)
val string_of_value : value -> string

(** Create an empty environment with a specified parent environment. *)
val make : t option -> t

(** Look up an identifier in an environment, returning a value. 
    Raises Not_found if the value doesn't exist. *)
val lookup : t -> string -> value option

(** Add a name/value binding to an environment. 
    This can bind a new name or rebind it if it's already bound. *)
val add : t -> string -> value -> unit

(** Add lists of names and values to an environment. *)
val add_all : t -> string list -> value list -> unit

(** Take an AST, an environment in which to evaluate it, and produce either a
    value or a list of errors. *)
val eval : Ast.t -> t -> (value, Errors.t list) Result.t

(** Take an OCaml list and turn it into a Bactrian list. *)
val cons_of_list : value list -> value

(** Take a Bactrian list and turn it into an OCaml list. *)
val list_of_cons : value -> value list

(** Take a Bactrian list (i.e., a quoted piec of code) and return a Sexpr.t. *)
val sexpr_of_cons : value -> Sexpr.t
