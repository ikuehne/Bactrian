(*
 *  lexer.mll
 *  
 *     bogoscheme lexer for CS11 OCaml track.
 *
 *     Ian Kuehne, 2015.
 *
 *)

{
   open Yacc
   open Core.Std

   let char_of_string s =
      match s with
      | "newline" -> Result.Ok '\n'
      | "tab"  -> Result.Ok '\t'
      | "space" -> Result.Ok ' '
      | s when String.length s = 1 -> Result.Ok (Char.of_string s)
      | _   -> Result.Error (Errors.Literal s)

   let int_of_string_result num = 
      try Result.Ok (int_of_string num)
      with Failure "int_of_string" -> Result.Error (Errors.Literal num)
}

(* Some useful definitions. *)
let comment    = ';' [^'\n']*
let whitespace = [' ' '\t' '\n']
let integer    = '-'? ['0' - '9']+
let id_char = [^ '(' ')' ';'] # whitespace
let id         = id_char (id_char | ['0' - '9'])*
let lit_char   = (_ # (whitespace)) [^ ' ' '\t' '\n' '(' ')']*
let lit_float  = integer '.' ['0' - '9']* (['e' 'E'] integer)?

(* The lexer definition itself. *)
rule lex = parse
  | comment                   { lex lexbuf                           }
  | whitespace                { lex lexbuf                           }
  | '('                       { TOK_LPAREN                           }
  | ')'                       { TOK_RPAREN                           }
  | "#u"                      { TOK_UNIT                             }
  | "#t"                      { TOK_BOOL true                        }
  | "#f"                      { TOK_BOOL false                       }
  | (integer as num)          { TOK_INT (int_of_string_result num)   }
  | (lit_float as num)        { TOK_FLOAT (Float.of_string num)      }
  | "#\\" (lit_char as c)     { TOK_CHAR (char_of_string c)          }
  | id as identifier          { TOK_ID identifier                    }
  | eof                       { TOK_EOF                              }
  (* lexer error -- this should never happen *)
  | _ { raise (Failure ("Unrecognized token: " ^ (Lexing.lexeme lexbuf))) } 

{
  (* Nothing. *)
}
