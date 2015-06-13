(*
 * primitives.mli
 *
 *     Primitives.
 *
 *     Ian Kuehne, 2015.
 *
 *     Primitives exports basic arithmetic operators, an overloaded print
 *     function, and comparisons over integers. Like all Bogoscheme functions,
 *     they can raise Invalid_Args. They can also raise Type_Errors; since
 *     the only type information in Bogoscheme comes from primitives and
 *     built-in functions, this means that all type errors will be caught by
 *     the interpreter at runtime.
 *
 *)

(** Load all primitive functions into an environment. *)
val load : Env.t -> unit
