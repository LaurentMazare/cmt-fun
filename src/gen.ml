open Base
open Stdio

let do_not_gen_types = Set.of_list (module String) [ "comparator_witness" ]

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
    ]

let python_name s = Map.find ops s |> Option.value ~default:s
let ocaml_name s = if Map.mem ops s then "(" ^ s ^ ")" else s

let write_ml outc (cmi_infos : Cmi_format.cmi_infos) =
  let pr ~indent s =
    Printf.ksprintf
      (fun line ->
        for _i = 1 to indent * 2 do
          Out_channel.output_char outc ' '
        done;
        Out_channel.output_string outc line;
        Out_channel.output_char outc '\n')
      s
  in
  let rec walk ~indent ~path (s : Types.signature_item) =
    let pr s = pr ~indent s in
    match s with
    | Sig_value (ident, value_description, Exported) ->
      let simple_type = Simple_type.of_type_desc value_description.val_type.desc in
      (match simple_type with
      | Ok simple_type ->
        let python_name = Ident.name ident |> python_name in
        let ocaml_name = Ident.name ident |> ocaml_name in
        pr "let %s () = (* %s *)" python_name (Simple_type.to_string simple_type);
        (match Simple_type.uncurrify simple_type with
        | (_ :: _ as args), result ->
          pr "  let%%map_open";
          List.map args ~f:(fun (_arg, _t) ->
              Printf.sprintf "    %s = positional \"%s\" %s" "foo" "bar" "baz")
          |> String.concat ~sep:" and\n"
          |> pr "%s";
          pr "  in";
          pr "  %s" ocaml_name;
          List.iter args ~f:(fun (_arg, _t) -> pr "    foo");
          pr "  |> %s" (Simple_type.python_of_ml result)
        | [], _ ->
          pr
            "  no_arg (fun () -> %s |> %s)"
            ocaml_name
            (Simple_type.python_of_ml simple_type));
        pr ";;";
        pr ""
      | Error _err -> ())
    | Sig_value (_ident, _value_description, Hidden) -> ()
    | Sig_type (ident, _, _, _) ->
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
        pr "")
    | Sig_typext (_ident, _, _, _) -> ()
    | Sig_module (ident, _, module_declaration, _, _) ->
      (match module_declaration.md_type with
      | Mty_ident _path -> ()
      | Mty_signature signature ->
        let ident = Ident.name ident in
        if not (Set.mem do_not_gen_modules ident)
        then (
          pr "module %s = struct" ident;
          List.iter signature ~f:(walk ~indent:(indent + 1) ~path:(ident :: path));
          pr "end;;")
      | Mty_functor (_, _, _) -> ()
      | Mty_alias _ -> ())
    | Sig_modtype (_ident, _, _) -> ()
    | Sig_class (_ident, _, _, _) -> ()
    | Sig_class_type (_ident, _, _, _) -> ()
  in
  let pr s = pr ~indent:0 s in
  pr "(* THIS CODE IS GENERATED AUTOMATICALLY, DO NOT EDIT BY HAND *)";
  pr "open! Base";
  pr "open! Python_lib";
  pr "open! Python_lib.Let_syntax";
  pr "";
  List.iter cmi_infos.cmi_sign ~f:(walk ~indent:0 ~path:[ cmi_infos.cmi_name ])
