module P : sig
  type t

  val empty : t
  val append : t -> string -> t
  val names : t -> string list
end

type t

val create : unit -> t
val path : t -> P.t
val enter_module : t -> module_name:string -> t
val add_type : t -> type_name:string -> unit
val find_type : t -> type_name:string -> P.t option
