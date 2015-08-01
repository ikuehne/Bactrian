open Core.Std

module Serialized(M : sig
   type t
   val sexp_of_t : t -> Sexp.t
   val t_of_sexp : Sexp.t -> t
end) = struct
   let to_sexp     = M.sexp_of_t
   let of_sexp     = M.t_of_sexp
   let to_string t = t |> to_sexp |> Sexp.to_string
   let of_string s = s |> String.strip    |> Sexp.of_string |> of_sexp
   let load      f = f |> Sexp.load_sexps |> List.map ~f:of_sexp
end
