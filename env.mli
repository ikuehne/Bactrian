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
   | Val_cons    of value * value
   | Val_prim    of (t -> value list -> value)
   | Val_lambda  of t * string list * string option * Check_ast.t list

(** Type of environments. *)
and t = { parent: t option; bindings: value String.Table.t }

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

(** Add lists of names and values to an environment.
    This is just for convenience. *)
val add_all : t -> string list -> value list -> unit
