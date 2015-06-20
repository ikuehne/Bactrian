(*
 * sexpr.mli
 *
 *     Scheme s-expressions.
 *
 *     Ian Kuehne, 2015.
 *
 *     Sexpr contains the type representing s-expressions for the Scheme
 *     interpreter, not to be comfused with the serialization format from Core,
 *     which is contained in Sexp. Also provides serialization capabilities.
 *
 *)

open Core.Std

(* Type of all S-expressions. *)
type t =
   | Atom of Atom.t
   | List of t list with sexp

(** Make an s-expression with an s-expression. *)
val to_sexp : t -> Sexp.t

(** Retrieve the data from an Sexp.t. Raises a Sexplib.Conv.Of_sexp_error on
    improper input. *)
val of_sexp : Sexp.t -> t

(** Show an s-expression as a Sexp-like string. *)
val to_string : t -> string

(** Retrieve an s-expression from a Sexp-like string. *)
val of_string : string -> t

(** 'load file' gets a list of 't's from the file named 'file' containing
    properly formatted s-expressions as strings. *)
val load : string -> t list
