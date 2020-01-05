open Base

module P : sig
  type t

  val empty : t
  val append : t -> string -> t
  val names : t -> string list
end = struct
  type t = string list

  let empty = []
  let append t v = v :: t
  let names t = List.rev t
end

type t =
  { path : P.t
  ; types : string Hash_set.t
  ; parent : t option
  }

let path t = t.path
let create () = { path = P.empty; types = Hash_set.create (module String); parent = None }

let enter_module t ~module_name =
  { path = P.append t.path module_name
  ; types = Hash_set.create (module String)
  ; parent = Some t
  }

let exit_module t = t.parent
let add_type t ~type_name = Hash_set.add t.types type_name

let find_type t ~type_name =
  let rec walk t =
    if Hash_set.mem t.types type_name
    then Some t.path
    else (
      match t.parent with
      | None -> None
      | Some parent -> walk parent)
  in
  walk t
