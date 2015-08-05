open Core.Std

let count_matched s =
   let rec aux lit escaped n = function
      | [] -> n
      | '\\' :: xs -> aux lit true n xs
      | '(' :: xs -> if lit && escaped then aux false false n xs
                                       else aux false false (n + 1) xs
      | ')' :: xs -> if lit && escaped then aux false false n xs
                                       else aux false false (n - 1) xs
      | c :: xs -> aux (lit && (not (Char.is_whitespace c))) false n xs in
   aux false false 0 (String.to_list s)

let check_matched_string s =
   match count_matched s with
   | x when x < 0 -> `Bad
   | x when x = 0 -> `Good
   | _            -> `Incomplete

let check_matched_channel c =
   let rec aux_line = function
      | [] -> Option.some
      | '(' :: xs -> fun n -> aux_line xs (n + 1)
      | ')' :: xs -> 
         begin function
            | n when n < 1 -> None
            | n -> aux_line xs (n - 1)
         end
      | _ :: xs -> aux_line xs in
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
