(* 
 * lexer_test.mli
 *
 * Unit test for the ocamllex lexer.
 *
 * Ian Kuehne, 2015.
 *
 * Tests that lexer produces archived correct output in bs_results
 *
 *)

open Core.Std

val test_file : In_channel.t -> In_channel.t -> bool
