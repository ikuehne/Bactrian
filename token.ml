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

open Core.Std

(* Make a throwaway module to use the Serialized functor. *)
module T = struct
   open Yacc
   type t = token

   let sexp_of_t = function
      | TOK_LPAREN -> Sexp.Atom "TOK_LPAREN"
      | TOK_RPAREN -> Sexp.Atom "TOK_RPAREN"
      | TOK_UNIT   -> Sexp.Atom "TOK_UNIT"
      | TOK_EOF    -> Sexp.Atom "TOK_EOF"
      | TOK_BOOL b -> Sexp.List [Sexp.Atom "TOK_BOOL"; Bool.sexp_of_t b]
      | TOK_ID   s -> Sexp.List [Sexp.Atom "TOK_ID";   String.sexp_of_t s]
      | TOK_FLOAT f -> Sexp.List [Sexp.Atom "TOK_FLOAT";
                                  Float.sexp_of_t f]
      | TOK_STRING s -> Sexp.List [Sexp.Atom "TOK_STRING";
                                   Sexp.Atom s ]
      | TOK_INT  r -> Sexp.List [Sexp.Atom "TOK_INT";
                                 Result.sexp_of_t Int.sexp_of_t
                                                  Errors.sexp_of_t
                                                  r]
      | TOK_CHAR r -> Sexp.List [Sexp.Atom "TOK_CHAR";
                                 Result.sexp_of_t Char.sexp_of_t
                                                  Errors.sexp_of_t
                                                  r]
      | TOK_QUOTE -> Sexp.Atom "TOK_QUOTE"

   let t_of_sexp = function
      | Sexp.Atom "TOK_LPAREN" -> TOK_LPAREN
      | Sexp.Atom "TOK_RPAREN" -> TOK_RPAREN
      | Sexp.Atom "TOK_UNIT"   -> TOK_UNIT  
      | Sexp.Atom "TOK_EOF"    -> TOK_EOF   
      | Sexp.List [Sexp.Atom "TOK_BOOL"; (Sexp.Atom _) as b] ->
            TOK_BOOL (Bool.t_of_sexp b)
      | Sexp.List [Sexp.Atom "TOK_ID";   (Sexp.Atom _) as s] ->
            TOK_ID   (String.t_of_sexp s)
      | Sexp.List [Sexp.Atom "TOK_INT";  (Sexp.Atom _) as r] ->
            TOK_INT  (Result.t_of_sexp Int.t_of_sexp
                                       Errors.t_of_sexp
                                       r)
      | Sexp.List [Sexp.Atom "TOK_CHAR"; (Sexp.Atom _) as r] ->
            TOK_CHAR (Result.t_of_sexp Char.t_of_sexp
                                       Errors.t_of_sexp
                                       r)
      | Sexp.Atom "TOK_QUOTE" -> TOK_QUOTE
      | other -> Sexplib.Conv.of_sexp_error "unexpected sexp" other

end
include T
include Serial.Serialized(T)
