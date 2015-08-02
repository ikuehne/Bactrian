(* Types. *)

open Errors
open Core.Std

type value = 
   | Val_unit
   | Val_bool    of bool
   | Val_int     of int
   | Val_float   of float
   | Val_char    of char
   | Val_prim    of (value list -> value)      (* primitive functions *)
   | Val_lambda  of t * string list * Check_ast.t list

and t = { parent: t option; bindings: value String.Table.t }

(* Values. *)

let string_of_value = function
   | Val_unit       -> "#u"
   | Val_bool true  -> "#t"
   | Val_bool false -> "#f"
   | Val_int i      -> string_of_int i
   | Val_float f    -> Float.to_string f
   | Val_char c     -> String.make 1 c
   | Val_prim _     -> "primitive function"
   | Val_lambda _   -> "lambda expression"

let type_of_value = function
   | Val_unit       -> "Unit"
   | Val_bool _     -> "Bool"
   | Val_int _      -> "Int"
   | Val_char _     -> "Char"
   | Val_prim _ 
   | Val_lambda _   -> "Lambda"


(* Environments. *)

let make parent = { parent = parent; bindings = String.Table.create ()
                                                                    ~size:20 }

let rec lookup env name = 
   let { parent = p; bindings = b } = env in
   let open Option.Monad_infix in
   match Hashtbl.find b name with
   | None   -> p >>= (fun e -> lookup e name)
   | x -> x

let add env key data = 
   Hashtbl.set env.bindings ~key ~data

let add_all env names values = 
   match List.zip names values with
   | Some l -> List.iter ~f:(fun (x, y) -> add env x y) l
   | None   -> raise (Errors.Invalid_Args ("[lambda expression]",
                                           List.length names,
                                           List.length values))
