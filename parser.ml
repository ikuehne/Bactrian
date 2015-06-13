type token =
  | TOK_LPAREN
  | TOK_RPAREN
  | TOK_UNIT
  | TOK_BOOL of (bool)
  | TOK_ID of (string)
  | TOK_EOF
  | TOK_INT of ((int, Errors.t)  Core.Std.Result.t)
  | TOK_CHAR of ((char, Errors.t) Core.Std.Result.t)

open Parsing;;
let _ = parse_error;;
# 11 "parser/parser.mly"
   open Core.Std
# 16 "parser/parser.ml"
let yytransl_const = [|
  257 (* TOK_LPAREN *);
  258 (* TOK_RPAREN *);
  259 (* TOK_UNIT *);
  262 (* TOK_EOF *);
    0|]

let yytransl_block = [|
  260 (* TOK_BOOL *);
  261 (* TOK_ID *);
  263 (* TOK_INT *);
  264 (* TOK_CHAR *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\002\000\002\000\002\000\002\000\002\000\
\004\000\004\000\004\000\005\000\005\000\000\000"

let yylen = "\002\000\
\001\000\001\000\001\000\001\000\001\000\001\000\001\000\001\000\
\003\000\002\000\001\000\002\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\004\000\005\000\008\000\011\000\006\000\
\007\000\014\000\002\000\001\000\003\000\010\000\000\000\000\000\
\012\000\009\000"

let yydgoto = "\002\000\
\010\000\011\000\015\000\013\000\016\000"

let yysindex = "\008\000\
\007\255\000\000\255\254\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\007\255\014\255\
\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\015\255\000\000\
\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\017\000\000\000\004\000"

let yytablesize = 19
let yytable = "\003\000\
\014\000\004\000\005\000\006\000\007\000\008\000\009\000\003\000\
\001\000\004\000\005\000\006\000\007\000\008\000\009\000\018\000\
\013\000\012\000\017\000"

let yycheck = "\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\001\001\
\001\000\003\001\004\001\005\001\006\001\007\001\008\001\002\001\
\002\001\001\000\015\000"

let yynames_const = "\
  TOK_LPAREN\000\
  TOK_RPAREN\000\
  TOK_UNIT\000\
  TOK_EOF\000\
  "

let yynames_block = "\
  TOK_BOOL\000\
  TOK_ID\000\
  TOK_INT\000\
  TOK_CHAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Sexpr.t) in
    Obj.repr(
# 38 "parser/parser.mly"
          ( if _1 = (Sexpr.List []) then None else Some _1 )
# 92 "parser/parser.ml"
               : Sexpr.t option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Atom.t) in
    Obj.repr(
# 43 "parser/parser.mly"
          ( Sexpr.Atom _1 )
# 99 "parser/parser.ml"
               : Sexpr.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Sexpr.t list) in
    Obj.repr(
# 44 "parser/parser.mly"
          ( Sexpr.List _1 )
# 106 "parser/parser.ml"
               : Sexpr.t))
; (fun __caml_parser_env ->
    Obj.repr(
# 48 "parser/parser.mly"
             ( Atom.Unit    )
# 112 "parser/parser.ml"
               : Atom.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 49 "parser/parser.mly"
             ( Atom.Bool _1 )
# 119 "parser/parser.ml"
               : Atom.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : (int, Errors.t)  Core.Std.Result.t) in
    Obj.repr(
# 50 "parser/parser.mly"
             ( Atom.Int  _1 )
# 126 "parser/parser.ml"
               : Atom.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : (char, Errors.t) Core.Std.Result.t) in
    Obj.repr(
# 51 "parser/parser.mly"
             ( Atom.Char _1 )
# 133 "parser/parser.ml"
               : Atom.t))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 52 "parser/parser.mly"
             ( Atom.ID   _1 )
# 140 "parser/parser.ml"
               : Atom.t))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Sexpr.t list) in
    Obj.repr(
# 57 "parser/parser.mly"
                                      ( _2 )
# 147 "parser/parser.ml"
               : Sexpr.t list))
; (fun __caml_parser_env ->
    Obj.repr(
# 58 "parser/parser.mly"
                                      ( [] )
# 153 "parser/parser.ml"
               : Sexpr.t list))
; (fun __caml_parser_env ->
    Obj.repr(
# 59 "parser/parser.mly"
                                      ( [] )
# 159 "parser/parser.ml"
               : Sexpr.t list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Sexpr.t) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Sexpr.t list) in
    Obj.repr(
# 63 "parser/parser.mly"
                                      ( _1 :: _2 )
# 167 "parser/parser.ml"
               : Sexpr.t list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Sexpr.t) in
    Obj.repr(
# 64 "parser/parser.mly"
                                      ( [_1] )
# 174 "parser/parser.ml"
               : Sexpr.t list))
(* Entry parse *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let parse (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Sexpr.t option)
;;
