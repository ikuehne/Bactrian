open Core.Std

(* Make a throwaway module to use the Serialized functor. *)
module T = struct
   type t =
      | Atom of Atom.t
      | List of t list with sexp
end
include T
include Serial.Serialized(T)
