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

open Core.Std

(* Make a throwaway module to use the Serialized functor. *)
module T = struct
   type t =
      | Unit
      | Bool of bool
      | Int  of (int, Errors.t)  Result.t
      | Float of float
      | Char of (char, Errors.t) Result.t
      | String of string
      | ID   of string with sexp
end
include T
include Serial.Serialized(T)

let recreate_input = function
   | Unit           -> "()"
   | Bool b         -> Bool.to_string b
   | Int (Ok i)     -> Int.to_string i
   | Int (Error e)  -> Errors.throw e
   | Float f        -> Float.to_string f
   | Char (Ok c)    -> Char.to_string c
   | Char (Error e) -> Errors.throw e
   | String s       -> s
   | ID s           -> s
