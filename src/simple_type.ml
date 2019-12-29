open Base

type t =
  | Arrow of [ `no_label | `labelled of string | `optional of string ] * t * t
  | Tuple of t list
  | Constr of string

let of_type_desc type_desc =
  let open Or_error.Let_syntax in
  let rec walk (type_desc : Types.type_desc) =
    match type_desc with
    | Tvar _ -> Or_error.error_string "not handled: Tvar"
    | Tunivar _ -> Or_error.error_string "not handled: Tunivar"
    | Tvariant _ -> Or_error.error_string "not handled: Tvariant"
    | Tnil -> Or_error.error_string "not handled: Tnil"
    | Tobject (_, _) -> Or_error.error_string "not handled: Tobject"
    | Tfield (_, _, _, _) -> Or_error.error_string "not handled: Tfield"
    | Tpackage (_, _, _) -> Or_error.error_string "not handled: Tpackage"
    | Tpoly (_, _) -> Or_error.error_string "not handled: Tpoly"
    | Tlink e -> walk e.desc
    | Tsubst e -> walk e.desc
    | Ttuple es ->
      List.map es ~f:(fun e -> walk e.desc)
      |> Or_error.all
      |> Or_error.map ~f:(fun ts -> Tuple ts)
    | Tarrow (kind, e1, e2, _) ->
      let kind =
        match kind with
        | Nolabel -> `no_label
        | Labelled s -> `labelled s
        | Optional s -> `optional s
      in
      let%bind e1 = walk e1.desc in
      let%bind e2 = walk e2.desc in
      Ok (Arrow (kind, e1, e2))
    | Tconstr (constr, _, _) -> Ok (Constr (Path.name constr))
  in
  walk type_desc

let to_string t =
  let rec walk = function
    | Tuple ts -> List.map ts ~f:walk |> String.concat ~sep:", " |> Printf.sprintf "(%s)"
    | Constr name -> name
    | Arrow (_, lhs, rhs) -> Printf.sprintf "%s -> %s" (walk lhs) (walk rhs)
  in
  walk t
