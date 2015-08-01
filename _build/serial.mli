(* 
 * serial.mli
 *
 *    Convenient serialization via s-expressions.
 *
 *    Ian Kuehne, 2015.
 *
 *    Serial provides a simple interface to use s-expressions to serialize data
 *    in the interpreter.
 *
 *)

open Core.Std

(** Provides convenient serialization via s-expressions. *)
module Serialized(M : sig

   (** Type of data to be serialized. *)
   type t

   (** Make an s-expression from the data to be serialized. Conveniently
       provided by the 'with sexp' syntax extension. *)
   val sexp_of_t : t -> Sexp.t

   (** Get the data out of an s-expression. Conveniently provided by the 'with
       sexp' syntax extension. Should raise a Sexplib.Conv.Of_sexp_error on
       improper input. *)
   val t_of_sexp : Sexp.t -> t

end) : sig
   (** Make an s-expression with the data. *)
   val to_sexp : M.t -> Sexp.t

   (** Retrieve the data from an s-expression. Should raise a
       Sexplib.Conv.Of_sexp_error on improper input. *)
   val of_sexp : Sexp.t -> M.t

   (** Show the data as a Sexp-like string. *)
   val to_string : M.t -> string

   (** Retrieve the data from a Sexp-like string. *)
   val of_string : string -> M.t

   (** 'load file' gets a list of 't's from the file named 'file' containing
       properly formatted s-expressions as strings. *)
   val load : string -> M.t list
end
