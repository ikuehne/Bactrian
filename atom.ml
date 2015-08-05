open Core.Std

(* Make a throwaway module to use the Serialized functor. *)
module T = struct
   type t =
      | Unit
      | Bool of bool
      | Int  of (int, Errors.t)  Result.t
      | Float of float
      | Char of (char, Errors.t) Result.t
      | String of string
      | ID   of string with sexp
end
include T
include Serial.Serialized(T)
