(*
 * eval.mli
 *
 *     Evaluator.
 *
 *)

open Core.Std

val eval : Ast.t -> Env.t -> (Env.value, Errors.t list) Result.t
