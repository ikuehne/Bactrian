(*
 * env.mli
 *
 *     Environments and the values that they contain.
 *
 *     Ian Kuehne, 2015.
 *
 *     Env implements an env type which matches identifiers to values.
 *     The mapping is implemented as a hashtable with string keys.
 *
 *)

open Core.Std
open Errors

(** Type of legal values i.e. results of evaluation. *)
type value = 
   | Val_unit
   | Val_bool    of bool
   | Val_int     of int
   | Val_float   of float
   | Val_char    of char
   | Val_string  of string
   | Val_list    of value list
   | Val_prim    of (value list -> value)      (* primitive functions *)
   | Val_lambda  of t * string list * Check_ast.t list

(** Type of environments. *)
and t = { parent: t option; bindings: value String.Table.t }

(** Convert a value into a string representing its type. *)
val type_of_value : value -> string

(** Convert a value into a string representing its value. *)
val string_of_value : value -> string

(** Create an empty environment with a specified parent environment. *)
val make : t option -> t

(** Look up an identifier in an environment, returning a value. 
    Raises Not_found if the value doesn't exist. *)
val lookup : t -> string -> value option

(** Add a name/value binding to an environment. 
    This can bind a new name or rebind it if it's already bound. *)
val add : t -> string -> value -> unit

(** Add lists of names and values to an environment.
    This is just for convenience. *)
val add_all : t -> string list -> value list -> unit
