open Base

module Arg = struct
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

(* Assume that the following typenames are never shadowed by something different. *)
let basic_constr0 =
  Set.of_list (module String) [ "int"; "bool"; "string"; "float"; "unit" ]

let supported_constr1 = Set.of_list (module String) [ "list"; "array" ]

let python_of_ml t =
  let escape str = String.tr str ~target:'.' ~replacement:'_' |> String.lowercase in
  let rec walk = function
    | Arrow _ -> failwith "TODO"
    | Tuple2 (t1, t2) -> tuple [ t1; t2 ]
    | Tuple3 (t1, t2, t3) -> tuple [ t1; t2; t3 ]
    | Tuple4 (t1, t2, t3, t4) -> tuple [ t1; t2; t3; t4 ]
    | Tuple5 (t1, t2, t3, t4, t5) -> tuple [ t1; t2; t3; t4; t5 ]
    | Atom constr -> "python_of_" ^ escape constr
    | Apply (t, constr) -> Printf.sprintf "(python_of_%s %s)" (escape constr) (walk t)
  and tuple ts =
    let names = List.mapi ts ~f:(fun i t -> Printf.sprintf "t%d" i, t) in
    Printf.sprintf
      "(fun (%s) -> Py.Tuple.of_list [%s])"
      (List.map names ~f:fst |> String.concat ~sep:", ")
      (List.map names ~f:(fun (name, t) -> walk t ^ " " ^ name) |> String.concat ~sep:"; ")
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
      let%bind tuple = List.map es ~f:(fun e -> walk e.desc) |> Or_error.all in
      (match tuple with
      | [] -> Or_error.error_string "empty tuple"
      | [ _ ] -> Or_error.error_string "tuple with a single element"
      | [ t1; t2 ] -> Ok (Tuple2 (t1, t2))
      | [ t1; t2; t3 ] -> Ok (Tuple3 (t1, t2, t3))
      | [ t1; t2; t3; t4 ] -> Ok (Tuple4 (t1, t2, t3, t4))
      | [ t1; t2; t3; t4; t5 ] -> Ok (Tuple5 (t1, t2, t3, t4, t5))
      | _ -> Or_error.errorf "tuple with too many elements (%d)" (List.length tuple))
    | Tarrow (kind, e1, e2, _) ->
      let%bind e1 = walk e1.desc in
      let%bind e2 = walk e2.desc in
      Ok (Arrow (kind, e1, e2))
    | Tconstr (constr, [], _) ->
      let last = Path.last constr in
      if Set.mem basic_constr0 last then Ok (Atom last) else Ok (Atom (Path.name constr))
    | Tconstr (constr, [ param ], _) ->
      let%bind param = walk param.desc in
      let last = Path.last constr in
      if Set.mem supported_constr1 last
      then Ok (Apply (param, last))
      else Or_error.errorf "not handled: type constructor %s" last
    | Tconstr (constr, _ :: _ :: _, _) ->
      Or_error.errorf
        "not handled: constructor with more than one parameter %s"
        (Path.name constr)
  in
  walk type_desc

let to_string t =
  let need_parenthesis = function
    | Tuple2 _ | Tuple3 _ | Tuple4 _ | Tuple5 _ | Arrow _ -> true
    | Atom _ | Apply _ -> false
  in
  let rec walk = function
    | Tuple2 (t1, t2) -> tuple [ t1; t2 ]
    | Tuple3 (t1, t2, t3) -> tuple [ t1; t2; t3 ]
    | Tuple4 (t1, t2, t3, t4) -> tuple [ t1; t2; t3; t4 ]
    | Tuple5 (t1, t2, t3, t4, t5) -> tuple [ t1; t2; t3; t4; t5 ]
    | Atom name -> name
    | Apply (param, name) ->
      if need_parenthesis param
      then Printf.sprintf "(%s) %s" (walk param) name
      else Printf.sprintf "%s %s" (walk param) name
    | Arrow (_, lhs, rhs) ->
      if need_parenthesis lhs
      then Printf.sprintf "(%s) -> %s" (walk lhs) (walk rhs)
      else Printf.sprintf "%s -> %s" (walk lhs) (walk rhs)
  and tuple ts =
    List.map ts ~f:walk |> String.concat ~sep:", " |> Printf.sprintf "(%s)"
  in
  walk t

let uncurrify t =
  let rec walk acc t =
    match t with
    | Arrow (arg, t1, t2) -> walk ((arg, t1) :: acc) t2
    | Tuple2 _ | Tuple3 _ | Tuple4 _ | Tuple5 _ | Atom _ | Apply _ -> List.rev acc, t
  in
  walk [] t
