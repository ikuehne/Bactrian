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
   | Float  of float
   | Char   of (char, Errors.t) Result.t
   | String of string
   | ID     of string
   | Define of (string * t, Errors.t) Result.t
   | If     of t * t * t
   (* A lambda is represented by a tuple containing: a list of argument names,
    * an optional name for the rest of the arguments (for variadic functions)
    * and a list of ASTs representing the body of the lambda. *)
   | Lambda of (string list * string option * t list, Errors.t) Result.t
   | Apply  of (t * t list, Errors.t) Result.t

(** Make an s-expression from an AST. *)
val to_sexp : t -> Sexp.t

(** Retrieve the data from an Sexp.t. Raises a Sexplib.Conv.Of_sexp_error on
    improper input. *)
val of_sexp : Sexp.t -> t

(** Show an AST as a Sexp-like string. *)
val to_string : t -> string

(** Retrieve an AST from a Sexp-like string. *)
val of_string : string -> t

(** 'load file' gets a list of 't's from the file named 'file' containing
    properly formatted s-expressions as strings. *)
val load : string -> t list

(** Convert an S-expression into an AST expression. *)
val ast_of_sexpr : Sexpr.t -> t
