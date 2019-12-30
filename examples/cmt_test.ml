open! Base
module Simple_type = Cmt_python.Simple_type

let rec print_sign : string -> Types.signature_item -> unit =
 fun indent s ->
  match s with
  | Sig_value (ident, value_description, Exported) ->
    let simple_type = Simple_type.of_type_desc value_description.val_type.desc in
    let simple_type =
      match simple_type with
      | Ok simple_type -> Simple_type.to_string simple_type
      | Error err -> Error.to_string_mach err
    in
    Stdio.printf "%svalue %s: %s\n%!" indent (Ident.name ident) simple_type
  | Sig_value (_ident, _value_description, Hidden) -> ()
  | Sig_type (ident, _, _, _) -> Stdio.printf "%stype %s\n%!" indent (Ident.name ident)
  | Sig_typext (ident, _, _, _) ->
    Stdio.printf "%stypext %s\n%!" indent (Ident.name ident)
  | Sig_module (ident, _, module_declaration, _, _) ->
    Stdio.printf "%smodule %s\n%!" indent (Ident.name ident);
    (match module_declaration.md_type with
    | Mty_ident path -> Stdio.printf "%s  ident %s\n%!" indent (Path.name path)
    | Mty_signature signature ->
      Stdio.printf "%ssig\n%!" indent;
      List.iter signature ~f:(print_sign (indent ^ "  "));
      Stdio.printf "%ssigend\n%!" indent
    | Mty_functor (_, _, _) -> Stdio.printf "      functor\n%!"
    | Mty_alias _ -> Stdio.printf "%s  alias\n%!" indent)
  | Sig_modtype (ident, _, _) -> Stdio.printf "%smodtype %s\n%!" indent (Ident.name ident)
  | Sig_class (ident, _, _, _) -> Stdio.printf "%sclass %s\n%!" indent (Ident.name ident)
  | Sig_class_type (ident, _, _, _) ->
    Stdio.printf "%sclasstype %s\n%!" indent (Ident.name ident)

let () =
  match Sys.get_argv () with
  | [| _; cmt_name; out_ml |] ->
    let cmi_infos, cmt_infos = Cmt_format.read cmt_name in
    Stdio.printf "cmt loaded\n%!";
    Option.iter cmi_infos ~f:(fun cmi_infos ->
        Stdio.Out_channel.with_file out_ml ~f:(fun outc ->
            Cmt_python.Gen.write_ml outc cmi_infos);
        Stdio.printf "cmi_name %s\n%!" cmi_infos.cmi_name;
        Stdio.printf "cmi_sign %d\n%!" (List.length cmi_infos.cmi_sign);
        List.iter cmi_infos.cmi_sign ~f:(print_sign "  "));
    Option.iter cmt_infos ~f:(fun cmt_infos ->
        Stdio.printf "cmt_name %s\n%!" cmt_infos.cmt_modname)
  | _ -> Printf.failwithf "usage: cmt_test.exe example.cmti out.ml" ()
