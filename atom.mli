(* 
 * atom.mli
 *
 * Atoms for s-expressions.
 *
 * Ian Kuehne, 2015.
 *
 * Atom contains the type representing possible atoms in s-expressions. Used to
 * define s-expressions in Sexpr.
 *
 *)

open Core.Std

type t =
   | Unit
   | Bool of bool
   | Int  of (int, Errors.t)  Result.t
   | Char of (char, Errors.t) Result.t
   | ID   of string

val to_string : t -> string
