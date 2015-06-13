open Core.Std

type t =
   | Unit
   | Bool of bool
   | Int  of (int, Errors.t)  Result.t
   | Char of (char, Errors.t) Result.t
   | ID   of string

let to_string = function
   | Unit   -> "#u"
   | Bool b -> if b then "#t" else "#f"
   | Int  r -> begin
                  match r with
                  | Ok i    -> string_of_int i
                  | Error _ -> assert false
               end
   | Char r -> begin
                  match r with
                  | Ok c    -> Char.to_string c
                  | Error _ -> assert false
               end
   | ID   s -> s
