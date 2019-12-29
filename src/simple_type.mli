open Base

type t

val of_type_desc : Types.type_desc -> t Or_error.t
val to_string : t -> string
