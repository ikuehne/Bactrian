(*
 * ast.mli
 *
 *     Abstract syntax tree for bogoscheme for the CS11 OCaml track.
 *
 *     Ian Kuehne, 2015.
 *
 *)

open Core.Std

(** Type of AST expressions. *)
type t =
   | Unit
   | Bool   of bool
   | Int    of (int, Errors.t)  Result.t
   | Char   of (char, Errors.t) Result.t
   | ID     of string
   | Define of (string * t, Errors.t) Result.t
   | If     of t * t * t
   | Lambda of ((string list) * t list, Errors.t) Result.t
   | Apply  of (t * t list, Errors.t) Result.t

(** Convert an S-expression into an AST expression. *)
val ast_of_sexpr : Sexpr.t -> t

(** Convert an AST expression into a string. *)
val string_of_ast : t -> string
