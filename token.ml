type t = Parser.token

open Parser
open Core.Std

let to_string = function
   | TOK_LPAREN -> "TOK_LPAREN"
   | TOK_RPAREN -> "TOK_RPAREN"
   | TOK_UNIT   -> "TOK_UNIT"
   | TOK_BOOL b -> Printf.sprintf "TOK_BOOL[%s]" (Bool.to_string b)
   | TOK_ID   s -> Printf.sprintf "TOK_ID[%s]" s
   | TOK_EOF    -> "TOK_EOF"
   | TOK_INT (Ok i)     -> Printf.sprintf "TOK_INT[%s]"  (Int.to_string i)
   | TOK_INT (Error e)  -> Printf.sprintf "TOK_INT[%s]"  (Errors.to_string e)
   | TOK_CHAR (Ok c)    -> Printf.sprintf "TOK_CHAR[%s]" (Char.to_string c)
   | TOK_CHAR (Error e) -> Printf.sprintf "TOK_CHAR[%s]" (Errors.to_string e)
