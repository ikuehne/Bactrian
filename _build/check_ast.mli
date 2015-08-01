(*
 * check_ast.mli
 *
 *     Checks an AST from Ast and ensures that there are no error types in
 *     it. Returns a checked AST with no error types and a list of errors.
 *
 *     Ian Kuehne, 2015.
 *
 *)

open Core.Std

(* Type of AST expressions without errors. *)
type t =
   | Unit
   | Bool   of bool
   | Int    of int
   | Char   of char
   | ID     of string
   | Define of string * t
   | If     of t * t * t
   | Lambda of string list * t list
   | Apply  of t * t list

val check : Ast.t -> (t, Errors.t list) Result.t
