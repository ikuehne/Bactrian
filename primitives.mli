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
    Primitive values in the language.

    Primitives exports basic arithmetic operators, an overloaded print
    function, and comparisons over integers. Like all Bactrian functions,
    they can raise Invalid_Args. They can also raise Type_Errors; since the
    only type information in Bactrian comes from primitives and built-in
    functions, this means that all type errors will be caught by the
    interpreter at runtime.
 *)

(** Load all primitive functions into an environment. *)
val load : Env.t -> unit
