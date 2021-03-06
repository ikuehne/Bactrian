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

open Core.Std

(* Make a throwaway module to use the Serialized functor. *)
module T = struct
   type t =
      | Literal  of string
      | Type     of string * string
      | Argument of string * int * int with sexp

end
include T
include Serial.Serialized(T)

exception Type_Error of string * string

exception Syntax_Error of string

exception Invalid_Args of string * int * int

exception Invalid_Literal of string

exception Name_Error of string

let red s = "\027[31;1m" ^ s ^ "\027[0m"

let throw = function
   | Literal s          -> raise (Invalid_Literal s)
   | Type (t1, t2)      -> raise (Type_Error (t1, t2))
   | Argument (f, e, g) -> raise (Invalid_Args (f, e, g))

let print = function
   | Literal s ->
      Printf.fprintf stderr "%s Invalid literal: %s.\n"
                            (red "Literal error: ") s;
      flush stderr
   | Type (e, r) ->
      Printf.fprintf stderr "%s Expected %s, got %s.\n"
                            (red "Type error: ") e r;
      flush stderr
   | Argument (f, e, r) -> 
      let plural = if e = 1 then ""
                            else "s" in
      Printf.fprintf stderr "%s Expected %d argument%s to %s; got %d.\n"
                            (red "Argument Error: ")
                            e plural f r;
      flush stderr

let print_exn = function
   | Failure f ->
        Printf.fprintf stderr "%s %s\n" (red "Error: ") f;
        flush stderr
   | Syntax_Error s -> 
        Printf.fprintf stderr "%s %s\n" (red "Syntax Error: ") s;
        flush stderr
   | Name_Error e ->
        Printf.fprintf stderr "%s Undefined name: %s\n" (red "Name Error: ") e;
        flush stderr
   | Type_Error (e, r) ->
        Printf.fprintf stderr "%s Expected %s, got %s.\n"
                              (red "Type Error: ") e r;
        flush stderr
   | Invalid_Args (f, e, r) ->
        let plural = if e = 1 then ""
                              else "s" in
        Printf.fprintf stderr "%s Expected %d argument%s to %s; got %d.\n"
                              (red "Argument Error: ")
                              e plural f r;
        flush stderr
   | Sys_error e ->
         Printf.fprintf stderr "%s %s.\n" (red "System error: ") e;
         flush stderr
   | e -> raise e
