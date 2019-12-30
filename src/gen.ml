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

let write_ml outc (cmi_infos : Cmi_format.cmi_infos) =
  let rec walk ~indent ~path (s : Types.signature_item) =
    let pr s =
      Printf.ksprintf
        (fun line ->
          for _i = 1 to indent * 2 do
            Out_channel.output_char outc ' '
          done;
          Out_channel.output_string outc line;
          Out_channel.output_char outc '\n')
        s
    in
    match s with
    | Sig_value (ident, value_description, Exported) ->
      let simple_type = Simple_type.of_type_desc value_description.val_type.desc in
      (match simple_type with
      | Ok simple_type ->
        pr
          "let %s : %s = assert false"
          (Ident.name ident)
          (Simple_type.to_string simple_type)
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
        pr "  let capsule = lazy (Py.Capsule.make \"ident_with_path\") in";
        pr "  (fun x -> (Lazy.force capsule |> fst) x),";
        pr "  (fun x -> (Lazy.force capsule |> snd) x)";
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
  List.iter cmi_infos.cmi_sign ~f:(walk ~indent:0 ~path:[ "M" ])
