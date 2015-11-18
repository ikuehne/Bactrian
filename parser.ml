(* 
 * Copyright 2015 Ian Kuehne.
 *
 * Email: ikuehne@caltech.edu
 *
 * This file is part of Bactrian.
 *
 * Bactrian is free software: you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any later
 * version.
 *
 * Bactrian is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * Bactrian.  If not, see <http://www.gnu.org/licenses/>.
 *
 *)

open Core.Std

(* Simple state machine for counting parentheses in a list of characters,
 * expressed as a series of mutually-recursive functions. *)
let rec count_normal n = function
   | '#' :: xs -> count_char n xs
   | '"' :: xs -> count_string n xs
   | '(' :: xs -> count_normal (n + 1) xs
   | ')' :: xs -> count_normal (n - 1) xs
   | ';' :: xs -> count_commented n xs
   | [] -> n
   | _   :: xs -> count_normal n xs
and count_char n = function
   | '\\' :: xs -> count_char_double_escaped n xs
   | [] -> n
   | _    :: xs -> count_normal n xs
and count_char_double_escaped n = function
   | c :: xs when (Char.is_whitespace c) -> count_normal n xs
   | [] -> n
   | _ :: xs -> count_char_escaped n xs
and count_char_escaped n = function
   | c :: xs when (Char.is_whitespace c) -> count_normal n xs
   | '(' :: xs -> count_normal (n + 1) xs
   | ')' :: xs -> count_normal (n - 1) xs
   | _ :: xs -> count_char_escaped n xs
   | [] -> n
and count_string n = function
   | '\\' :: xs -> count_string_escaped n xs
   | '"'  :: xs -> count_normal n xs
   | [] -> n
   | _    :: xs -> count_string n xs
and count_string_escaped n = function
   | [] -> n
   | _ :: xs -> count_string n xs
and count_commented n _ = n

let count_matched s = count_normal 0 (String.to_list s)

let check_matched_string s =
   match count_matched s with
   | x when x < 0 -> `Bad
   | x when x = 0 -> `Good
   | _            -> `Incomplete

let check_matched_channel c =
   let rec aux x = 
      match In_channel.input_line c with
      | None   -> if x = 0 then `Good else `Incomplete
      | Some l -> 
         begin match x + (count_matched l) with
            | y when y < 0 -> `Bad
            | y            -> aux y
         end in
   aux 0

let sexpr_from_channel c =
   let open Option.Monad_infix in
   let rec aux accum =
      let new_string = 
         (In_channel.input_line c) >>| (fun l -> accum ^ " " ^ l) in
      match new_string with
      | None ->
         raise (Errors.Syntax_Error "Mismatched parentheses; no matching ')'.")
      | Some s ->
         let lexbuf = Lexing.from_string s in
         begin
            match check_matched_string s with
            | `Bad ->
               raise (Errors.Syntax_Error ("Mismatched parentheses; no "
                                         ^ "matching '('"))
            | `Incomplete -> aux s
            | `Good -> Yacc.parse Lexer.lex lexbuf
         end in
   aux ""

let sexpr_list_from_channel c =
   let lexbuf = Lexing.from_channel c in
   let rec loop accum =
      match Yacc.parse Lexer.lex lexbuf with
      | None -> accum
      | Some s -> loop (s :: accum) in
   match check_matched_channel c with
   | `Bad -> raise (Errors.Syntax_Error ("Mismatched parentheses; no "
                                       ^ "matching '('"))
   | `Incomplete -> raise (Errors.Syntax_Error ("Mismatched parentheses; no "
                                              ^ "matching ')'"))
   | `Good -> In_channel.seek c Int64.zero;
              List.rev (loop [])

let stream_from_channel c =
   begin
      match check_matched_channel c with
      | `Bad -> raise (Errors.Syntax_Error ("Mismatched parentheses; no "
                                          ^ "matching '('"))
      | `Incomplete -> raise (Errors.Syntax_Error ("Mismatched parentheses; no "
                                                 ^ "matching ')'"))
      | `Good -> In_channel.seek c Int64.zero
   end;
   let lexbuf = Lexing.from_channel c in
   let open Option.Monad_infix in
   Sequence.unfold ~init:() ~f:(fun () ->
   (Yacc.parse Lexer.lex lexbuf) >>| (fun x -> (x, ())))
