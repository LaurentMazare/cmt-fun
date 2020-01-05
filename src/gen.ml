open Base
open Stdio

type t =
  | Nothing
  | Function of string
  | Type of string
  | Module of
      { ml_module_name : string
      ; path : string list
      }

let do_not_gen_types = Set.of_list (module String) [ "comparator_witness" ]
let direct_params = Set.of_list (module String) [ "bool"; "int"; "string"; "float" ]

let do_not_gen_modules =
  Set.of_list
    (module String)
    [ "Table"
    ; "Hash_set"
    ; "Hash_queue"
    ; "Replace_polymorphic_compare"
    ; "Map"
    ; "Set"
    ; "Stable"
    ; "O"
    ; "Private"
    ]

let ops =
  Map.of_alist_exn
    (module String)
    [ "<=", "lowereq"
    ; ">=", "greatereq"
    ; "<", "lower"
    ; ">", "greater"
    ; "=", "eq"
    ; "<>", "neq"
    ; "==", "physeq"
    ; "!=", "nphyseq"
    ; "<=.", "lowereq_approx"
    ; ">=.", "greatereq_approx"
    ; "<.", "lower_approx"
    ; ">.", "greater_approx"
    ; "=.", "eq_approx"
    ; "<>.", "neq_approx"
    ; "==.", "physeq_approx"
    ; "!=.", "nphyseq_approx"
    ; "+", "plus"
    ; "-", "minus"
    ; "*", "times"
    ; "/", "divide"
    ; "//", "divide_"
    ]

let escape str ~path =
  List.rev (str :: path) |> String.concat ~sep:"_" |> String.lowercase

let python_name s = Map.find ops s |> Option.value ~default:s
let ocaml_name s = if Map.mem ops s then "(" ^ s ^ ")" else s

let pr outc ~indent =
  Printf.ksprintf (fun line ->
      if not (String.is_empty line)
      then (
        for _i = 1 to indent * 2 do
          Out_channel.output_char outc ' '
        done;
        Out_channel.output_string outc line);
      Out_channel.output_char outc '\n')

let write_value ident value_description outc ~indent ~path =
  let pr s = pr outc ~indent s in
  let path_str = List.rev path |> String.concat ~sep:"." in
  let simple_type = Simple_type.of_type_desc value_description.Types.val_type.desc in
  match simple_type with
  | Ok simple_type ->
    let python_name = Ident.name ident |> python_name in
    let ocaml_name = Ident.name ident |> ocaml_name in
    pr "let %s () = (* %s *)" python_name (Simple_type.to_string simple_type);
    (match Simple_type.uncurrify simple_type with
    | (_ :: _ as args), result ->
      pr "  let%%map_open";
      let args =
        let unique_name =
          let already_used_names = Hash_set.create (module String) in
          fun name ->
            let rec loop d =
              let name = name ^ Int.to_string d in
              if Hash_set.mem already_used_names name then loop (d + 1) else name
            in
            if Hash_set.mem already_used_names name then loop 1 else name
        in
        List.mapi args ~f:(fun i (arg, t) ->
            let name =
              match arg with
              | Nolabel -> "positional_" ^ Int.to_string (i + 1)
              | Labelled l -> unique_name l
              | Optional l -> unique_name l
            in
            name, arg, t)
      in
      let nargs = List.length args in
      List.iteri args ~f:(fun index (name, arg, t) ->
          let kind =
            match arg with
            | Nolabel -> "positional"
            | Labelled _ -> "keyword"
            | Optional _ -> "keyword_opt"
          in
          let rec walk t =
            match (t : Simple_type.t) with
            | Atom a -> if Set.mem direct_params a then a else "param_" ^ escape a ~path
            | Tuple2 (t1, t2) -> Printf.sprintf "pair (%s) (%s)" (walk t1) (walk t2)
            | Tuple3 (t1, t2, t3) ->
              Printf.sprintf "triple (%s) (%s) (%s)" (walk t1) (walk t2) (walk t3)
            | _ -> "pyobject"
          in
          let suffix = if index < nargs - 1 then " and" else "" in
          pr
            "    %s = %s \"%s\" %s ~docstring:\"%s\"%s"
            name
            kind
            name
            (walk t)
            (Simple_type.to_string t)
            suffix);
      pr "  in";
      pr "  %s.%s" path_str ocaml_name;
      List.iter args ~f:(fun (name, arg, _t) ->
          let prefix =
            match arg with
            | Nolabel -> ""
            | Labelled _ -> "~"
            | Optional _ -> "?"
          in
          pr "    %s%s" prefix name);
      pr "  |> %s" (Simple_type.python_of_ml result)
    | [], _ ->
      pr
        "  Defunc.no_arg (fun () -> %s.%s |> %s)"
        path_str
        ocaml_name
        (Simple_type.python_of_ml simple_type));
    pr ";;";
    pr "";
    Function python_name
  | Error _err -> Nothing

let write_type ident outc ~indent ~path =
  let pr s = pr outc ~indent s in
  let ident = Ident.name ident in
  if not (Set.mem do_not_gen_types ident)
  then (
    let ident_with_path =
      match path with
      | [] -> ident
      | path -> List.rev (ident :: path) |> String.concat ~sep:"."
    in
    pr "";
    pr "type %s = %s;;" ident ident_with_path;
    pr "let python_of_%s, %s_of_python =" ident ident;
    pr "  let capsule = lazy (Py.Capsule.make \"%s\") in" ident_with_path;
    pr "  (fun x -> (Lazy.force capsule |> fst) x),";
    pr "  (fun x -> (Lazy.force capsule |> snd) x)";
    pr ";;";
    pr "let param_%s =" ident;
    pr
      "  Defunc.Of_python.create ~type_name:\"%s\" ~conv:%s_of_python"
      ident_with_path
      ident;
    pr ";;";
    pr "";
    Type ident)
  else Nothing

let register_module ts outc ~indent =
  let pr s = pr outc ~indent s in
  pr "let register_module ~module_name =";
  pr "  let modl = Py_module.create module_name in";
  List.iter ts ~f:(function
      | Nothing | Type _ -> ()
      | Function func_name ->
        pr "  Py_module.set modl \"%s\" (%s ());" func_name func_name
      | Module { ml_module_name; path } ->
        let python_module_name =
          List.rev (ml_module_name :: path) |> String.concat ~sep:"__" |> String.lowercase
        in
        pr
          "  let subm = %s.register_module ~module_name:\"%s\" in"
          ml_module_name
          python_module_name;
        pr
          "  Py_module.set_value modl \"%s\" (Py_module.to_pyobject subm);"
          (String.lowercase ml_module_name));
  pr "  modl"

let write_ml outc (cmi_infos : Cmi_format.cmi_infos) =
  let rec walk ~indent ~path (s : Types.signature_item) =
    let pr s = pr outc ~indent s in
    match s with
    | Sig_value (ident, value_description, Exported) ->
      write_value ident value_description outc ~indent ~path
    | Sig_value (_ident, _value_description, Hidden) -> Nothing
    | Sig_type (ident, _, _, _) -> write_type ident outc ~indent ~path
    | Sig_typext (_ident, _, _, _) -> Nothing
    | Sig_module (ident, _, module_declaration, _, _) ->
      (match module_declaration.md_type with
      | Mty_signature signature ->
        let ident = Ident.name ident in
        if not (Set.mem do_not_gen_modules ident)
        then (
          pr "module %s = struct" ident;
          let ts =
            List.map signature ~f:(walk ~indent:(indent + 1) ~path:(ident :: path))
          in
          pr "";
          register_module ts outc ~indent:(indent + 1);
          pr "end;;";
          Module { ml_module_name = ident; path })
        else Nothing
      | Mty_ident _ | Mty_functor (_, _, _) | Mty_alias _ -> Nothing)
    | Sig_modtype (_ident, _, _)
    | Sig_class (_ident, _, _, _)
    | Sig_class_type (_ident, _, _, _) -> Nothing
  in
  let pr s = pr outc ~indent:0 s in
  pr "(* THIS CODE IS GENERATED AUTOMATICALLY, DO NOT EDIT BY HAND *)";
  pr "open! Base";
  pr "open! Python_lib";
  pr "open! Python_lib.Let_syntax";
  pr "open! Gen_import";
  pr "";
  pr "let protect ~f x =";
  pr "  try f x with";
  pr "  | Py.Err _ as err -> raise err";
  pr "  | exn -> raise (Py.Err (SyntaxError, Exn.to_string exn))";
  pr ";;";
  pr "";
  let ts = List.map cmi_infos.cmi_sign ~f:(walk ~indent:0 ~path:[ cmi_infos.cmi_name ]) in
  pr "";
  register_module ts outc ~indent:0
