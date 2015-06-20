open Core.Std

(* Make a short alias for the lexer function. *)
let lex = Lexer.lex

(* Get a buffer from a filename. *)
let buffer_from_file = Lexing.from_channel

(* Get a list of strings from the lexer and a buffer. *)
let get_lexer_output buf = 
   let rec aux accum =
      match lex buf with 
      | Parser.TOK_EOF -> "TOK_EOF" :: accum
      | token          -> aux ((Token.to_string token) :: accum)
   in
   List.rev (aux [])

(* Get a list of strings from the correct output. *)
let get_correct_output = In_channel.input_lines ~fix_win_eol:false

let test_file test_in correct_out =
   test_in
   |> buffer_from_file
   |> get_lexer_output
   |> (=) (get_correct_output correct_out)

let () = let empty_test_file = In_channel.create ~binary:false 
                                                 "bs_tests/empty.bs" in
         let empty_result_file = In_channel.create ~binary:false 
                                                   "bs_results/empty.res" in
         begin
            match test_file empty_test_file empty_result_file with
            | true  -> printf "Passed!\n"
            | false -> printf "No pass!\n"
         end;
         In_channel.close empty_test_file;
         In_channel.close empty_result_file
