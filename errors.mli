(*
 * Errors.mli
 *
 *     Errors and related values.
 *
 *     Ian Kuehne, 2015.
 *
 *     Error contains a number of useful exceptions for handling bad input.
 *     These are meant to deal with problems originating with the user, and
 *     particularly to allow for helpful error messages.
 *
 *     Two styles of error handling are currently supported: the Errors.t type
 *     and exception throwing. There is some overlap; for example, a
 *     Type_Error can either be encoded as an exception or as an Errors.t.
 *
 *     Errors.t has the advantage that the error information can safely be
 *     accumulated in a Result type without stopping the interpretation. It is
 *     used extensively in the "lower" layers of the interpreter, up to Ast and
 *     below Env and Eval.  Higher up, it would be more complicated to
 *     accumulate errors and exceptions are still used. In general, the two
 *     are treated exactly the same way in the end: Main eventually prints an 
 *     error message and depending on context either quits or discards the last
 *     expression.
 *
 *)

open Core.Std

(* A type containing all possible errors. *)
type t =
   (* Error for invalid literals, such as a nonexistent character (for
      example #\foo). The string simply holds the entered literal. *)
   | Literal  of string
   (* Exception for type errors. Contains a string representing the expected
      type and a string representing the received type, in that order. *)
   | Type     of string * string
   (* Exception for invalid number of agruments. Contains the name of the
      function, the number of expected arguments, and the number of arguments
      received. *)
   | Argument of string * int * int


(** Exception for type errors. Contains a string representing the expected
    type and a string representing the received type, in that order. *)
exception Type_Error of string * string

(** Exception for syntax errors. *)
exception Syntax_Error of string

(** Exception for invalid number of agruments. Contains the name of the
    function, the number of expected arguments, and the number of arguments
    received. *)
exception Invalid_Args of string * int * int

(** Exception for invalid literals, such as a nonexistent character (for
    example #\foo). The string simply holds the entered literal. *)
exception Invalid_Literal of string

(** Exception type for referencing an unbound identifier. Contains the unbound
    identifier. *)
exception Name_Error of string

(** Print an error to stderr.  *)
val print : t -> unit

(** Convert an error to a string, intended for unit testing. *)
val to_string : t -> string
