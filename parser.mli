type token =
  | TOK_LPAREN
  | TOK_RPAREN
  | TOK_UNIT
  | TOK_BOOL of (bool)
  | TOK_ID of (string)
  | TOK_EOF
  | TOK_INT of ((int, Errors.t)  Core.Std.Result.t)
  | TOK_CHAR of ((char, Errors.t) Core.Std.Result.t)

val parse :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Sexpr.t option
