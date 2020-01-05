open Base

(* This returns a list of all the types that are needed. *)
val write_ml
  :  Stdio.Out_channel.t
  -> Cmi_format.cmi_infos
  -> (Module_env.P.t * string) Hash_set.t

val write_types : Stdio.Out_channel.t -> (Module_env.P.t * string) Hash_set.t -> unit
