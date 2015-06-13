open Core.Std

type t =
   | Literal of string
   | Type    of string * string
       

exception Type_Error of string * string

exception Syntax_Error of string

exception Invalid_Args of string * int * int

exception Invalid_Literal of string

exception Name_Error of string

let red s = "\027[31;1m" ^ s ^ "\027[0m"

let print = function
   | Literal s -> Printf.fprintf stderr "%s Invalid literal: %s.\n"
                                        (red "Literal error: ") s
   | Type (e, r) -> Printf.fprintf stderr "%s Expected %s, got %s.\n"
                                          (red "Type error: ") e r
 
