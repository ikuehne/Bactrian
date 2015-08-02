(*
 * new_parser.mli
 *
 *     Convenient access to parser functionality.
 *
 *     Ian Kuehne, 2015.
 *
 *     Ocamlyacc provides very little control over the API to the parser, so
 *     this module provides convenient access to parser functionality.
 *
 *)


open Core.Std

(** Gets a single sexpr from a file. If the end of one S-expression is on the
    same line as the start of another, the start of the second one is
    discarded. *)
val sexpr_from_channel : In_channel.t -> Sexpr.t option

(** Gets a list of sexprs from a file. Pulls all sexpr's from the file. *)
val sexpr_list_from_channel : In_channel.t -> Sexpr.t list

(** Gets a Sequence.t of sexpr's from an In_channel.t *)
val stream_from_channel : In_channel.t -> Sexpr.t Sequence.t
