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

module Serialized(M : sig
   type t
   val sexp_of_t : t -> Sexp.t
   val t_of_sexp : Sexp.t -> t
end) = struct
   let to_sexp     = M.sexp_of_t
   let of_sexp     = M.t_of_sexp
   let to_string t = t |> to_sexp |> Sexp.to_string
   let of_string s = s |> String.strip    |> Sexp.of_string |> of_sexp
   let load      f = f |> Sexp.load_sexps |> List.map ~f:of_sexp
end
