open Errors
open Core.Std

(* 
 * Define the primitive functions. 
 *)

(* Subtract two integers. *)
let add = function
   | [Env.Val_int i1; Env.Val_int i2] ->
         Env.Val_int (i1 + i2)
   | [Env.Val_float f1; Env.Val_int i2] ->
         Env.Val_float (f1 +. (Float.of_int i2))
   | [Env.Val_float f1; Env.Val_float f2] ->
         Env.Val_float (f1 +. f2)
   | [Env.Val_int i1; Env.Val_float f2] ->
         Env.Val_float ((Float.of_int i1) +. f2)
   | [Env.Val_int _; v] | [v; _] ->
         raise (Type_Error ("Int", Env.type_of_value v))
   | l ->
         raise (Invalid_Args ("+", 2, List.length l))

(* Subtract two integers. *)
let mul = function
   | [Env.Val_int i1; Env.Val_int i2] -> Env.Val_int (i1 * i2)
   | [Env.Val_int i1; Env.Val_float f2] ->
         Env.Val_float ((Float.of_int i1) *. f2)
   | [Env.Val_float f1; Env.Val_float f2] -> Env.Val_float (f1 *. f2)
   | [Env.Val_float f1; Env.Val_int i2] -> 
         Env.Val_float (f1 *. (Float.of_int i2))
   | l -> raise (Invalid_Args ("*", 2, List.length l))

(* Subtract two integers. *)
let sub = function
   | [Env.Val_int i1] -> Env.Val_int (- i1)  (* Unary minus *)
   | [Env.Val_int i1; Env.Val_float f2] ->
         Env.Val_float ((Float.of_int i1) -. f2)
   | [Env.Val_float f1; Env.Val_float f2] -> Env.Val_float (f1 -. f2)
   | [Env.Val_float f1; Env.Val_int i2] -> 
         Env.Val_float (f1 -. (Float.of_int i2))
   | [Env.Val_int i1; Env.Val_int i2] -> Env.Val_int (i1 - i2)
   | l -> raise (Invalid_Args ("-", 2, List.length l))

(* Divide two integers. *)
let div = function
   | [Env.Val_int i1; Env.Val_int i2] -> Env.Val_int (i1 / i2)
   | [Env.Val_int i1; Env.Val_float f2] ->
         Env.Val_float ((Float.of_int i1) /. f2)
   | [Env.Val_float f1; Env.Val_float f2] -> Env.Val_float (f1 /. f2)
   | [Env.Val_float f1; Env.Val_int i2] -> 
         Env.Val_float (f1 /. (Float.of_int i2))
   | l -> raise (Invalid_Args ("/", 2, List.length l))

(* Create a boolean operator acting on two integers. *)
let make_binary_bool_operator op name = function
   | [Env.Val_int i1; Env.Val_int i2] -> Env.Val_bool (op i1 i2)
   | l -> raise (Invalid_Args (name, 2, List.length l))
  
(* Define binary operators. *)
let eq = function
   | [Env.Val_int   i1; Env.Val_int   i2] -> Env.Val_bool (i1 = i2)
   | [Env.Val_int   i1; Env.Val_float f2] -> Env.Val_bool ((Float.of_int i1) = f2)
   | [Env.Val_float f1; Env.Val_float f2] -> Env.Val_bool (f1 = f2)
   | [Env.Val_float f1; Env.Val_int   i2] -> Env.Val_bool (f1 = (Float.of_int i2))
   | l -> raise (Invalid_Args ("=", 2, List.length l))
let ne = function
   | [Env.Val_int   i1; Env.Val_int   i2] -> Env.Val_bool (i1 <> i2)
   | [Env.Val_int   i1; Env.Val_float f2] -> Env.Val_bool ((Float.of_int i1) <> f2)
   | [Env.Val_float f1; Env.Val_float f2] -> Env.Val_bool (f1 <> f2)
   | [Env.Val_float f1; Env.Val_int   i2] -> Env.Val_bool (f1 <> (Float.of_int i2))
   | l -> raise (Invalid_Args ("!=", 2, List.length l))
let lt = function
   | [Env.Val_int   i1; Env.Val_int   i2] -> Env.Val_bool (i1 < i2)
   | [Env.Val_int   i1; Env.Val_float f2] -> Env.Val_bool ((Float.of_int i1) < f2)
   | [Env.Val_float f1; Env.Val_float f2] -> Env.Val_bool (f1 < f2)
   | [Env.Val_float f1; Env.Val_int   i2] -> Env.Val_bool (f1 < (Float.of_int i2))
   | l -> raise (Invalid_Args ("<", 2, List.length l))
let gt = function
   | [Env.Val_int   i1; Env.Val_int   i2] -> Env.Val_bool (i1 > i2)
   | [Env.Val_int   i1; Env.Val_float f2] -> Env.Val_bool ((Float.of_int i1) > f2)
   | [Env.Val_float f1; Env.Val_float f2] -> Env.Val_bool (f1 > f2)
   | [Env.Val_float f1; Env.Val_int   i2] -> Env.Val_bool (f1 > (Float.of_int i2))
   | l -> raise (Invalid_Args (">", 2, List.length l))
let le = function
   | [Env.Val_int   i1; Env.Val_int   i2] -> Env.Val_bool (i1 <= i2)
   | [Env.Val_int   i1; Env.Val_float f2] -> Env.Val_bool ((Float.of_int i1) <= f2)
   | [Env.Val_float f1; Env.Val_float f2] -> Env.Val_bool (f1 <= f2)
   | [Env.Val_float f1; Env.Val_int   i2] -> Env.Val_bool (f1 <= (Float.of_int i2))
   | l -> raise (Invalid_Args ("<=", 2, List.length l))
let ge = function
   | [Env.Val_int   i1; Env.Val_int   i2] -> Env.Val_bool (i1 >= i2)
   | [Env.Val_int   i1; Env.Val_float f2] -> Env.Val_bool ((Float.of_int i1) >= f2)
   | [Env.Val_float f1; Env.Val_float f2] -> Env.Val_bool (f1 >= f2)
   | [Env.Val_float f1; Env.Val_int   i2] -> Env.Val_bool (f1 >= (Float.of_int i2))
   | l -> raise (Invalid_Args (">=", 2, List.length l))

(* Type conversions. *)
let int_to_float = function
   | [Env.Val_int i] -> Env.Val_float (Float.of_int i)
   | [v] -> raise (Type_Error ("Int", Env.type_of_value v))
   | l -> raise (Invalid_Args ("int-to-float", 1, List.length l))

let float_to_int = function
   | [Env.Val_float f] -> Env.Val_int (Float.to_int f)
   | [v] -> raise (Type_Error ("Float", Env.type_of_value v))
   | l -> raise (Invalid_Args ("float-to-int", 1, List.length l))

(* Print a value. *)
let print = function
   | [value] -> 
        Printf.printf "%s" (Env.string_of_value value);
        Env.Val_unit
   | l -> raise (Invalid_Args ("print", 1, List.length l))

(* Exits the repl. *)
let exit = function
   | [value] -> begin match value with
                   | Env.Val_int i -> exit i
                   | _ -> raise (Type_Error ("Int", 
                                             Env.type_of_value value))
                end
   | l -> raise (Invalid_Args ("exit", 1, List.length l))



(* Load the primitive functions into an environment, 
   along with their names. *)

let load env =
   let ops = [(add, "+"); (sub, "-"); (mul, "*"); (div, "/");
              (eq, "="); (ne, "!="); (lt, "<"); (gt, ">");
              (le, "<="); (ge, ">="); (print, "print"); (exit, "exit");
              (float_to_int, "float-to-int"); (int_to_float, "int-to-float")] in
      List.iter ~f:(fun (op, name) -> Env.add env name (Env.Val_prim op)) ops
