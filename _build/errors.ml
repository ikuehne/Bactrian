open Core.Std

(* Make a throwaway module to use the Serialized functor. *)
module T = struct
   type t =
      | Literal  of string
      | Type     of string * string
      | Argument of string * int * int with sexp

end
include T
include Serial.Serialized(T)

exception Type_Error of string * string

exception Syntax_Error of string

exception Invalid_Args of string * int * int

exception Invalid_Literal of string

exception Name_Error of string

let red s = "\027[31;1m" ^ s ^ "\027[0m"

let print = function
   | Literal s ->
      Printf.fprintf stderr "%s Invalid literal: %s.\n"
                            (red "Literal error: ") s
   | Type (e, r) ->
      Printf.fprintf stderr "%s Expected %s, got %s.\n"
                            (red "Type error: ") e r
   | Argument (f, e, r) -> 
      let plural = if e = 1 then ""
                            else "s" in
     Printf.fprintf stderr "%s Expected %d argument%s to %s; got %d.\n"
                           (red "Argument Error: ")
                           e plural f r
