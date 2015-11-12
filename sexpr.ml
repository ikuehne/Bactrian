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

type t =
   | Atom of Atom.t
   | List of t list
   | Quote of t

let rec to_sexp = function
   | Atom a  -> Sexp.List [Sexp.Atom "Atom"; Atom.to_sexp a]
   | List xs -> Sexp.List [Sexp.Atom "List"; Sexp.List (List.map ~f:to_sexp xs)]
   | Quote t -> Sexp.List [Sexp.Atom "Quote"; to_sexp t]

let rec of_sexp = function
   | Sexp.List [Sexp.Atom "Atom"; s]  -> Atom (Atom.of_sexp s)
   | Sexp.List [Sexp.Atom "List"; Sexp.List l] -> List (List.map ~f:of_sexp l)
   | Sexp.List [Sexp.Atom "Quote"; s] -> Quote (of_sexp s)

let to_string t = t |> to_sexp |> Sexp.to_string
let of_string s = s |> Sexp.of_string |> of_sexp
let load f = f |> Sexp.load_sexps |> List.map ~f:of_sexp

let t_of_sexp = of_sexp
let sexp_of_t = to_sexp

let rec recreate_input = function
   | Atom a  -> Atom.recreate_input a
   | List ss -> let f str sexpr = str ^ " " ^ (recreate_input sexpr) in
                (List.fold_left ss ~init:"(" ~f) ^ ")"
   | Quote q -> sprintf "(quote %s )" (recreate_input q)
