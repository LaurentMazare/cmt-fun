open Base

module Arg : sig
  type t = Asttypes.arg_label =
    | Nolabel
    | Labelled of string
    | Optional of string
end

type t =
  | Arrow of Arg.t * t * t
  | Tuple of t list
  | Constr0 of string
  | Constr1 of string * t

val uncurrify : t -> (Arg.t * t) list * t
val of_type_desc : Types.type_desc -> t Or_error.t
val to_string : t -> string

(* [python_of_ml t] returns a string with some ocaml code to convert a value of type [t]
   into a pyobject. *)
val python_of_ml : t -> string
