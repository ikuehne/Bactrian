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
       | Lambda of (string list * string option * t list, Errors.t) Result.t
       | Apply  of (t * t list, Errors.t) Result.t with sexp
end
include T
include Serial.Serialized(T)

module S = Sexpr

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

(* Get the corresponding AST expression from an atomic s-expression. *)
let ast_of_atom = function
| Atom.Unit     -> Unit
| Atom.Bool   b -> Bool   b
| Atom.Int    i -> Int    i
| Atom.Float  f -> Float  f
| Atom.Char   c -> Char   c
| Atom.String s -> String s
| Atom.ID    id -> ID    id

(* Create an AST from an s-expression representing an if statement. *)
let rec ast_of_if = function
| [a; b; c] -> If ((ast_of_sexpr a), (ast_of_sexpr b), (ast_of_sexpr c))
| lst       -> raise (Invalid_Args ("if", 3, (List.length lst)))

(* Create an AST from an s-expression representing a lambda. *)
and ast_of_lambda = function
| (S.Atom (Atom.ID args)) :: (_ :: _ as body) ->
   let body = List.map ~f:ast_of_sexpr body in
   Lambda (Ok ([], Some args, body))
| args :: (_ :: _ as body) ->
   begin
      match check_list args with
      | Ok args ->
         let ids  = List.map ~f:check_id     args in
         let body = List.map ~f:ast_of_sexpr body in
         begin
            match List.filter ~f:is_error ids with
            | []      -> Lambda (Ok (List.map ~f:ok_exn ids, None, body))
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

(* Create an AST from an s-expression representing a function 
 * application. *)
and ast_of_apply = function
| f :: args -> begin
                  match check_f f with
                  | Ok    f -> Apply (Ok (f, (List.map ~f:ast_of_sexpr args)))
                  | Error e -> Apply (Error e)
               end
| []        -> assert false

(* Check that an s-expression is an identifier, returning a Result.t with
 * an Error.Type _ if it is not. *)
and check_id sexpr =
   match ast_of_sexpr sexpr with
   | ID id -> Ok id
   | ast   -> Error (Errors.Type ("ID", get_type ast))
 
(* Create an AST from an s-expression representing a definition. *)
and ast_of_def = function
| [a; b] -> begin
               match check_id a with
               | Ok id   -> Define (Ok (id, ast_of_sexpr b))
               | Error e -> Define (Error e)
            end
| lst    -> Define (Error (Argument ("define", 2, (List.length lst))))

(* Check that an s-expression is a list, returning a Result.t with an
 * Error.Type _ if it is not. *)
and check_list = function
| S.List lst         -> Ok lst
| (S.Atom _) as sexp -> Error (Errors.Type ("List", sexp 
                                                 |> ast_of_sexpr
                                                 |> get_type))

(* Create an AST from a list of s-expressions. *)
and ast_of_list = function
| [] -> Unit
| [Sexpr.Atom atom] -> ast_of_atom atom
| (Sexpr.Atom (Atom.ID "define")) :: def  -> ast_of_def def
| (Sexpr.Atom (Atom.ID "if")) :: if_then  -> ast_of_if if_then
| (Sexpr.Atom (Atom.ID "lambda")) :: body -> ast_of_lambda body
| other -> ast_of_apply other

(* Check that an s-expression could be a function, returning a Result.t with
 * an Error.Type _ if it is not. *)
and check_f f = 
   match ast_of_sexpr f with
   | (ID     _) as id     -> Ok id
   | (Lambda _) as lambda -> Ok lambda
   | (Apply  _) as apply  -> Ok apply
   | other -> Error (Errors.Type ("Lambda", get_type other))

(* Generate an abstract syntax tree from an s-expression. *)
and ast_of_sexpr = function
| S.Atom atom -> ast_of_atom atom
| S.List lst  -> ast_of_list lst
