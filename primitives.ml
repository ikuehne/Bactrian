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
   | [v1; v2] -> Env.Val_bool (v1 = v2)
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
   | l -> raise (Invalid_Args ("int->float", 1, List.length l))

let float_to_int = function
   | [Env.Val_float f] -> Env.Val_int (Float.to_int f)
   | [v] -> raise (Type_Error ("Float", Env.type_of_value v))
   | l -> raise (Invalid_Args ("float->int", 1, List.length l))

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

(* List construction functions. *)
let cons = function
   | [v1; Env.Val_list v2] -> Env.Val_list (v1 :: v2)
   | [_; v2] -> raise (Type_Error ("List", Env.type_of_value v2))
   | l -> raise (Invalid_Args ("cons", 2, List.length l))

let make_list = function
   | l -> Env.Val_list l

let cdr = function
   | [Env.Val_list (x::xs)] -> Env.Val_list xs
   | [Env.Val_list []] -> raise (Syntax_Error "cdr: list has no tail.")
   | [v] -> raise (Type_Error ("List", Env.type_of_value v))
   | l -> raise (Invalid_Args ("cdr", 1, List.length l))

let car = function
   | [Env.Val_list (x::xs)] -> x
   | [Env.Val_list []] -> raise (Syntax_Error "car: list has no tail.")
   | [v] -> raise (Type_Error ("List", Env.type_of_value v))
   | l -> raise (Invalid_Args ("cdr", 2, List.length l))

let nil = Env.Val_list []

(* String handling functions. *)
let strindex = function
   | [Env.Val_int i; Env.Val_string s] -> Env.Val_char s.[i]
   | [Env.Val_int i; v2] -> raise (Type_Error ("String", Env.type_of_value v2))
   | [v1; _] -> raise (Type_Error ("Int", Env.type_of_value v1))
   | l -> raise (Invalid_Args ("string-index", 2, List.length l))

let string_to_list = function
   | [Env.Val_string s] -> String.to_list s
                        |> List.map ~f:(fun c -> Env.Val_char c)
                        |> fun l -> Env.Val_list (l)
   | [v] -> raise (Type_Error ("String", Env.type_of_value v))
   | l -> raise (Invalid_Args ("string->list", 1, List.length l))

let list_to_string =
   let rec aux accum = function
      | (Env.Val_char c) :: xs -> aux (c::accum) xs
      | [] -> List.rev accum
      | v :: xs -> raise (Type_Error ("Char", Env.type_of_value v)) in
   function
      | [Env.Val_list l] -> Env.Val_string (String.of_char_list (aux [] l))
      | [v] -> raise (Type_Error ("List", Env.type_of_value v))
      | l -> raise (Invalid_Args ("list->string", 1, List.length l))

(* Load the primitive functions into an environment, 
   along with their names. *)
let load env =
   let ops = [(add, "+"); (sub, "-"); (mul, "*"); (div, "/");
              (eq, "="); (ne, "!="); (lt, "<"); (gt, ">");
              (le, "<="); (ge, ">="); (print, "print"); (exit, "exit");
              (float_to_int, "float->int"); (int_to_float, "int->float");
              (make_list, "list"); (cdr, "cdr"); (car, "car"); (cons, "cons");
              (strindex, "string-index"); (string_to_list, "string->list");
              (list_to_string, "list->string")] in
   let vals = [(nil, "nil")] in
   List.iter ~f:(fun (op, name) -> Env.add env name (Env.Val_prim op)) ops;
   List.iter ~f:(fun (v, name) -> Env.add env name v) vals
