(* 
 * token.mli
 *
 * Tokens as lexer output.
 *
 * Ian Kuehne, 2015.
 *
 * Token deals with the 'token' type used for lexer output.
 *
 *)

(** t is publicly an alias for Parser.token. The type is concrete because 
    this module exists specifically to deal with that type, so it should easily
    be convertable with that type. *)
type t = Parser.token

(** Convert a token to a string, intended for unit tests of the lexer. *)
val to_string : t -> string
