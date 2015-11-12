/* 
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
 */

/**
    Bogoscheme parser for ocamlyacc.
 */

%{
   open Core.Std
%}
  
/* declarations */

%token          TOK_LPAREN TOK_RPAREN TOK_QUOTE
%token          TOK_UNIT
%token <bool>   TOK_BOOL
%token <string> TOK_ID
%token          TOK_EOF
%token <(int, Errors.t)  Core.Std.Result.t> TOK_INT
%token <(char, Errors.t) Core.Std.Result.t> TOK_CHAR
%token <string> TOK_STRING
%token <float> TOK_FLOAT

%start parse
%type <Sexpr.t option> parse
%type <Atom.t>         atom
%type <Sexpr.t>        sexpr
%type <Sexpr.t list>   slist
%type <Sexpr.t list>   sexpr_list

%%

/* Rules */

/* Parsing rule for general expressions.  Simply match for an s-expression and
 * package it in an option. */
parse:
  | sexpr        { if $1 = (Sexpr.List []) then None else Some $1 }

/* Parsing rule for quoted S-Expressions. An S-Expression preceded by a quote.
*/
quoted_sexpr:
   | TOK_QUOTE sexpr { Sexpr.Quote $2 }

/* Parsing rule for s-expressions. Either an atom or a list of s-expressions.
 */
sexpr:
  | atom         { Sexpr.Atom $1 }
  | slist        { Sexpr.List $1 }
  | quoted_sexpr { $1 }

/* Match atoms and package them in an Atom.t. */
atom:  
  | TOK_UNIT   { Atom.Unit      }
  | TOK_BOOL   { Atom.Bool $1   }
  | TOK_INT    { Atom.Int  $1   }
  | TOK_STRING { Atom.String $1 }
  | TOK_FLOAT  { Atom.Float  $1 }       
  | TOK_CHAR   { Atom.Char $1   }
  | TOK_ID     { Atom.ID   $1   }

/* Parsing rule for lists of s-expressions. Match a series of s-expressions
 * in parentheses, or the empty list. */
slist:
  | TOK_LPAREN sexpr_list TOK_RPAREN  { $2 }
  | TOK_LPAREN TOK_RPAREN             { [] }
  | TOK_LPAREN sexpr_list {failwith "Bad!"}
  | TOK_EOF                           { [] }

/* Parsing rule for series of s-expressions. */
sexpr_list:
  | sexpr sexpr_list                  { $1 :: $2 }
  | sexpr                             { [$1] }

%%
