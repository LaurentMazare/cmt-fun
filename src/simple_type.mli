open Base

module Arg : sig
  type t = Asttypes.arg_label =
    | Nolabel
    | Labelled of string
    | Optional of string
end

type t =
  | Atom of string
  | Tuple2 of t * t
  | Tuple3 of t * t * t
  | Tuple4 of t * t * t * t
  | Tuple5 of t * t * t * t * t
  | Arrow of Arg.t * t * t
  | Apply of t * string

val uncurrify : t -> (Arg.t * t) list * t
val of_type_desc : Types.type_desc -> t Or_error.t
val to_string : t -> string

(* [python_of_ml t] returns a string with some ocaml code to convert a value of type [t]
   into a pyobject. *)
val python_of_ml : t -> string
