open Base

let write_ml _outc (cmi_infos : Cmi_format.cmi_infos) =
  let rec walk indent (s : Types.signature_item) =
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
        List.iter signature ~f:(walk (indent ^ "  "));
        Stdio.printf "%ssigend\n%!" indent
      | Mty_functor (_, _, _) -> Stdio.printf "      functor\n%!"
      | Mty_alias _ -> Stdio.printf "%s  alias\n%!" indent)
    | Sig_modtype (ident, _, _) ->
      Stdio.printf "%smodtype %s\n%!" indent (Ident.name ident)
    | Sig_class (ident, _, _, _) ->
      Stdio.printf "%sclass %s\n%!" indent (Ident.name ident)
    | Sig_class_type (ident, _, _, _) ->
      Stdio.printf "%sclasstype %s\n%!" indent (Ident.name ident)
  in
  List.iter cmi_infos.cmi_sign ~f:(walk "")
