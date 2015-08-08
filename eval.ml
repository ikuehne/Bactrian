(*
 * eval.ml
 *
 *     Evaluator.
 *
 *)

open Env
open Errors
open Core.Std

let rec eval_checked ast env =

   let rec eval_lambda env xs = match xs with
      | [] -> Val_unit
      | [x] -> eval_checked x env
      | x :: xs -> ignore (eval_checked x env);
                   eval_lambda env xs in

   let handle_lookup name = function
      | None   -> raise (Name_Error name)
      | Some v -> v in

   match ast with
      | Check_ast.Unit -> Val_unit
      | Check_ast.Bool b -> Val_bool b
      | Check_ast.Char c -> Val_char c
      | Check_ast.String s -> Val_string s
      | Check_ast.Int i -> Val_int i
      | Check_ast.Float f -> Val_float f
      | Check_ast.ID id -> handle_lookup id (lookup env id)
      | Check_ast.Define (id, e) -> 
         let definition = eval_checked e env in
         add env id definition;
         Val_unit
      | Check_ast.If (test_e, then_e, else_e) -> 
         begin 
            match eval_checked test_e env with
            | Val_bool true -> (eval_checked then_e env)
            | Val_bool false -> (eval_checked else_e env)
            | x -> raise (Type_Error ("ID",
                                      type_of_value x))
         end
      | Check_ast.Lambda (ids, args, exprs) -> 
         Val_lambda (env, ids, args, exprs)
      | Check_ast.Apply (f, args) ->
         (* Evaluate all the arguments. *)
         let operands = List.map ~f:(fun x -> eval_checked x env) args in
         (* Evaluate the function. *)
         match eval_checked f env with
            | Val_prim prim -> prim operands
            | (Val_lambda (env, ids, None, exprs)) as lambda ->
                  let new_env  = make (Some env) in
                  begin
                     try
                        add_all new_env ids operands;
                     with Invalid_argument _ ->
                        raise (Invalid_Args (string_of_value lambda,
                                             List.length ids,
                                             List.length operands))
                  end;
                  eval_lambda new_env exprs
            | (Val_lambda (env, [], Some args, exprs)) as lambda ->
                  let new_env  = make (Some env) in
                  let arg_list = Val_list operands in
                  add new_env args arg_list;
                  eval_lambda new_env exprs
            | x -> raise (Syntax_Error ("Value "
                                      ^ (string_of_value x)
                                      ^ " is not a function; "
                                      ^ "cannot apply."))

let eval ast env =
   let checked_ast = Check_ast.check ast in
   match checked_ast with
   | Ok ast -> Ok (eval_checked ast env)
   | (Error _) as x -> x
