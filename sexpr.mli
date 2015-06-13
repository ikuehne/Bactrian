(*
 * sexpr.mli
 *
 *     S-expressions.
 *
 *)


(* Type of all S-expressions. *)
type t =
   | Atom of Atom.t
   | List of t list
