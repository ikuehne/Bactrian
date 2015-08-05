/*
 * parser.mly
 *
 *     bogoscheme parser for the CS11 OCaml track.
 *
 *     Ian Kuehne, 2015.
 *
 */

%{
   open Core.Std
%}
  
/* declarations */

%token          TOK_LPAREN TOK_RPAREN
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
  | sexpr { if $1 = (Sexpr.List []) then None else Some $1 }

/* Parsing rule for s-expressions. Either an atom or a list of s-expressions.
 */
sexpr:
  | atom  { Sexpr.Atom $1 }
  | slist { Sexpr.List $1 }

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
