open Base

module Arg = struct
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

(* Assume that the following typenames are never shadowed by something different. *)
let basic_constr0 =
  Set.of_list (module String) [ "int"; "bool"; "string"; "float"; "unit" ]

let supported_constr1 = Set.of_list (module String) [ "list"; "array" ]

let python_of_ml t =
  let rec walk = function
    | Arrow _ -> failwith "TODO"
    | Tuple _ -> "python_of_tuple"
    | Constr0 constr -> "python_of_" ^ constr
    | Constr1 (constr, t) -> Printf.sprintf "(python_of_%s %s)" constr (walk t)
  in
  walk t

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
      let%bind e1 = walk e1.desc in
      let%bind e2 = walk e2.desc in
      Ok (Arrow (kind, e1, e2))
    | Tconstr (constr, [], _) ->
      let last = Path.last constr in
      if Set.mem basic_constr0 last
      then Ok (Constr0 last)
      else Ok (Constr0 (Path.name constr))
    | Tconstr (constr, [ param ], _) ->
      let%bind param = walk param.desc in
      let last = Path.last constr in
      if Set.mem supported_constr1 last
      then Ok (Constr1 (last, param))
      else Or_error.errorf "not handled: type constructor %s" last
    | Tconstr (constr, _ :: _ :: _, _) ->
      Or_error.errorf
        "not handled: constructor with more than one parameter %s"
        (Path.name constr)
  in
  walk type_desc

let to_string t =
  let rec walk = function
    | Tuple ts -> List.map ts ~f:walk |> String.concat ~sep:", " |> Printf.sprintf "(%s)"
    | Constr0 name -> name
    | Constr1 (name, param) ->
      let add_parenthesis =
        match param with
        | Tuple _ | Arrow _ -> true
        | Constr0 _ | Constr1 _ -> false
      in
      if add_parenthesis
      then Printf.sprintf "(%s) %s" (walk param) name
      else Printf.sprintf "%s %s" (walk param) name
    | Arrow (_, lhs, rhs) -> Printf.sprintf "%s -> %s" (walk lhs) (walk rhs)
  in
  walk t

let uncurrify t =
  let rec walk acc t =
    match t with
    | Arrow (arg, t1, t2) -> walk ((arg, t1) :: acc) t2
    | Tuple _ | Constr0 _ | Constr1 _ -> List.rev acc, t
  in
  walk [] t
