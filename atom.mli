(* 
 * atom.mli
 *
 *     Atoms for s-expressions.
 *
 *     Ian Kuehne, 2015.
 *
 *     Atom contains the type representing possible atoms in s-expressions. 
 *     Used to define s-expressions in Sexpr. Serialization capabilities are
 *     also provided.
 *
 *)

open Core.Std

type t =
   | Unit
   | Bool of bool
   | Int  of (int, Errors.t)  Result.t
   | Float  of float
   | Char of (char, Errors.t) Result.t
   | String of string
   | ID   of string with sexp

(** Make an s-expression with an Atom.t. *)
val to_sexp : t -> Sexp.t

(** Retrieve the data from an s-expression. Raises a Sexplib.Conv.Of_sexp_error
    on improper input. *)
val of_sexp : Sexp.t -> t

(** Show an atom as a Sexp-like string. *)
val to_string : t -> string

(** Retrieve an atom from a Sexp-like string. *)
val of_string : string -> t

(** 'load file' gets a list of 't's from the file named 'file' containing
    properly formatted s-expressions as strings. *)
val load : string -> t list
