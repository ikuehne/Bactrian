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
    Convenient access to parser functionality.
 
    Ocamlyacc provides very little control over the API to the parser, so
    this module provides convenient access to parser functionality.
 *)


open Core.Std

(** Gets a single sexpr from a channel. If the end of one S-expression is on the
    same line as the start of another, the start of the second one is
    discarded. *)
val sexpr_from_channel : In_channel.t -> Sexpr.t option

(** Gets a list of sexprs from a channel. Pulls all sexpr's from the file. *)
val sexpr_list_from_channel : In_channel.t -> Sexpr.t list

(** Gets a Sequence.t of sexpr's from an In_channel.t *)
val stream_from_channel : In_channel.t -> Sexpr.t Sequence.t
