(* 
 * Copyright 2015 Ian Kuehne.
 *
 * Email: ikuehne@caltech.edu
 *
 * This file is part of Bogoscheme.
 *
 * Bogoscheme is free software: you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * Bogoscheme is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * Bogoscheme.  If not, see <http://www.gnu.org/licenses/>.
 *
 *)

(**
    Bogoscheme lexer for CS11 OCaml track.
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
let id_char    = [^ '0' - '9' '(' ')' ';' '''] # whitespace
let id         = id_char (id_char | ['0' - '9' '''])*
let lit_char   = (_ # (whitespace)) [^ ' ' '\t' '\n' '(' ')']*
let lit_string = ([^'"'] | '\\' '"')*
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
  | '"'(lit_string as s)'"'   { TOK_STRING s                         }
  | id as identifier          { TOK_ID identifier                    }
  | '''                       { TOK_QUOTE                            }
  | eof                       { TOK_EOF                              }
  (* lexer error -- this should never happen *)
  | _ { raise (Failure ("Unrecognized token: " ^ (Lexing.lexeme lexbuf))) } 

{
  (* Nothing. *)
}
