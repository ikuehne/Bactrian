open Errors
open Core.Std

module T = struct
    (* Type of bogoscheme expressions, representing the abstract syntax tree. *)
    type t =
       | Unit
       | Bool   of bool
       | Int    of (int,  Errors.t)  Result.t
       | Float  of float
       | Char   of (char, Errors.t) Result.t
       | String of string
       | ID     of string
       | Define of (string * t, Errors.t) Result.t
       | If     of t * t * t
       | Lambda of (string list * t list, Errors.t) Result.t
       | Apply  of (t * t list, Errors.t) Result.t with sexp
end
include T
include Serial.Serialized(T)

let get_type = function
   | Unit     -> "Unit"
   | Bool   _ -> "Bool"
   | Int    _ -> "Int"
   | Float  _ -> "Float"
   | Char   _ -> "Char"
   | String _ -> "String"
   | ID     _ -> "ID"
   | Define _ -> "Define"
   | If     _ -> "If"
   | Lambda _ -> "Function"
   | Apply  _ -> "Apply"

let ok_exn = function
   | Ok x    -> x
   | Error _ -> failwith "ok_exn"

(* Generate an abstract syntax tree from an s-expression. *)
let rec ast_of_sexpr sexpr =
   let module S = Sexpr in

   (* Check that an s-expression is an identifier, returning a Result.t with
    * an Error.Type _ if it is not. *)
   let check_id sexpr =
      match ast_of_sexpr sexpr with
      | ID id -> Ok id
      | ast   -> Error (Errors.Type ("ID", get_type ast)) in
 
   (* Check that an s-expression is a list, returning a Result.t with an
    * Error.Type _ if it is not. *)
   let check_list = function
   | S.List lst         -> Ok lst
   | (S.Atom _) as sexp -> Error (Errors.Type ("List", sexp 
                                                  |> ast_of_sexpr
                                                  |> get_type)) in

   (* Check that an s-expression could be a function, returning a Result.t with
    * an Error.Type _ if it is not. *)
   let check_f f = 
      match ast_of_sexpr f with
      | (ID     _) as id     -> Ok id
      | (Lambda _) as lambda -> Ok lambda
      | (Apply  _) as apply  -> Ok apply
      | other -> Error (Errors.Type ("Lambda", get_type other)) in

   (* Get the corresponding AST expression from an atomic s-expression. *)
   let ast_of_atom = function
   | Atom.Unit     -> Unit
   | Atom.Bool b   -> Bool b
   | Atom.Int  i   -> Int  i
   | Atom.Float f  -> Float f
   | Atom.Char c   -> Char c
   | Atom.String s -> String s
   | Atom.ID  id   -> ID  id in

   (* Create an AST from an s-expression representing a definition. *)
   let ast_of_def = function
   | [a; b] -> begin
                  match check_id a with
                  | Ok id   -> Define (Ok (id, ast_of_sexpr b))
                  | Error e -> Define (Error e)
               end
   | lst    -> Define (Error (Argument ("define", 2, (List.length lst))))
   in

   (* Create an AST from an s-expression representing an if statement. *)
   let ast_of_if = function
   | [a; b; c] -> If ((ast_of_sexpr a), (ast_of_sexpr b), (ast_of_sexpr c))
   | lst       -> raise (Invalid_Args ("if", 3, (List.length lst))) in

   (* Create an AST from an s-expression representing a lambda. *)
   let ast_of_lambda = function
   | args :: (_ :: _ as body) ->
      begin
         match check_list args with
         | Ok args ->
            let ids  = List.map ~f:check_id     args in
            let body = List.map ~f:ast_of_sexpr body in
            begin
               match List.filter ~f:is_error ids with
               | []      -> Lambda (Ok (List.map ~f:ok_exn ids, body))
               | e :: _ ->
                  begin
                     match e with
                     | Error error -> Lambda (Error error)
                     | _           -> assert false
                  end
            end
         | Error e -> Lambda (Error e)
      end
   | lst -> Lambda (Error (Argument ("lambda", 2, (List.length lst))))
   in

   (* Create an AST from an s-expression representing a function 
    * application. *)
   let ast_of_apply = function
   | f :: args -> 
      begin
         match check_f f with
         | Ok    f -> Apply (Ok (f, (List.map ~f:ast_of_sexpr args)))
         | Error e -> Apply (Error e)
      end
   | []        -> assert false in

   (* Create an AST from a list of s-expressions. *)
   let ast_of_list = function
   | [] -> Unit
   | [Sexpr.Atom atom] -> ast_of_atom atom
   | (Sexpr.Atom (Atom.ID "define")) :: def  -> ast_of_def def
   | (Sexpr.Atom (Atom.ID "if")) :: if_then  -> ast_of_if if_then
   | (Sexpr.Atom (Atom.ID "lambda")) :: body -> ast_of_lambda body
   | other -> ast_of_apply other in

   (* Entry point. *)
   match sexpr with
   | Sexpr.Atom atom -> ast_of_atom atom
   | Sexpr.List lst  -> ast_of_list lst

and string_of_ast ast =
   let sprintf  = Printf.sprintf in
   let spaces n = String.make n ' ' in
   let rec string_of_ids id_lst = 
      match id_lst with
         | [] -> ""
         | [id] -> id
         | h :: t -> h ^ " " ^ (string_of_ids t)
   in

   let rec iter ast indent =
      let string_of_exprs e_list =
         (List.fold_left ~f:(^) ~init:""
             (List.map
                 ~f:(fun e -> "\n" ^ iter e (indent + 2))
                 e_list))
      in
      match ast with
         | Unit    -> sprintf "%sUNIT"       (spaces indent) 
         | Bool b  -> sprintf "%sBOOL[ %b ]" (spaces indent) b
         | Int  (Ok i)  -> sprintf "%sINT[ %d ]"  (spaces indent) i
         | Float f -> sprintf "%sFLOAT[ %f ]" (spaces indent) f
         | Char (Ok c)  -> sprintf "%sCHAR[ %c ]" (spaces indent) c
         | String s -> sprintf "%sSTRING[ %s ]" (spaces indent) s
         | ID   id -> sprintf "%sID[ %s ]"   (spaces indent) id
         | Define (Ok (id, e)) -> 
              sprintf "%sDEFINE[%s\n%s ]" 
                 (spaces indent) id (iter e (indent + 2))
         | If (test_clause, then_clause, else_clause) ->
              sprintf "%sIF[\n%s\n%s\n%s ]"
                 (spaces indent) 
                 (iter test_clause (indent + 2))
                 (iter then_clause (indent + 2))
                 (iter else_clause (indent + 2))
         | Lambda (Ok (ids, body)) ->
              sprintf "%sLAMBDA[(%s)%s ]"
                 (spaces indent)
                 (string_of_ids ids)
                 (string_of_exprs body)
         | Apply (Ok (operator, operands)) ->
              sprintf "%sAPPLY[\n%s%s ]"
                 (spaces indent)
                 (iter operator (indent + 2))
                 (string_of_exprs operands)
         (* Errors. *)
         | Int    (Error _)
         | Char   (Error _)
         | Define (Error _)
         | Lambda (Error _)
         | Apply  (Error _) -> sprintf "%sERROR" (spaces indent)
   in
      "\n" ^ iter ast 0 ^ "\n"
