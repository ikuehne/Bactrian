open Core.Std

let check_matched_string s =
   let rec aux = function
      | [] -> 
         begin function
            | 0 -> `Good
            | _ -> `Incomplete
         end
      | '(' :: xs -> fun n -> aux xs (n + 1)
      | ')' :: xs -> 
         begin function
            | n when n < 1 -> `Bad
            | n -> aux xs (n - 1)
            end
      | _ :: xs -> aux xs in
   aux (String.to_list s) 0

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
         begin match aux_line (String.to_list l) x with
            | None -> `Bad
            | Some l -> aux l
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
            | `Good -> Parser.parse Lexer.lex lexbuf
         end in
   aux ""

let sexpr_list_from_channel c =
   let lexbuf = Lexing.from_channel c in
   let rec loop accum =
      match Parser.parse Lexer.lex lexbuf with
      | None -> accum
      | Some s -> loop (s :: accum) in
   match check_matched_channel c with
   | `Bad -> raise (Errors.Syntax_Error ("Mismatched parentheses; no "
                                       ^ "matching '('"))
   | `Incomplete -> raise (Errors.Syntax_Error ("Mismatched parentheses; no "
                                              ^ "matching ')'"))
   | `Good -> In_channel.seek c Int64.zero;
              List.rev (loop [])
