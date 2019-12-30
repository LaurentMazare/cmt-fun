open Base
open Stdio

let write_ml outc (cmi_infos : Cmi_format.cmi_infos) =
  let rec walk ~indent (s : Types.signature_item) =
    match s with
    | Sig_value (ident, value_description, Exported) ->
      let simple_type = Simple_type.of_type_desc value_description.val_type.desc in
      let simple_type =
        match simple_type with
        | Ok simple_type -> Simple_type.to_string simple_type
        | Error err -> Error.to_string_mach err
      in
      Out_channel.fprintf
        outc
        "let %s : %s = assert false\n%!"
        (Ident.name ident)
        simple_type
    | Sig_value (_ident, _value_description, Hidden) -> ()
    | Sig_type (ident, _, _, _) ->
      let ident = Ident.name ident in
      Out_channel.fprintf outc "type %s = %s\n" ident ident
    | Sig_typext (_ident, _, _, _) -> ()
    | Sig_module (ident, _, module_declaration, _, _) ->
      (match module_declaration.md_type with
      | Mty_ident _path -> ()
      | Mty_signature signature ->
        Out_channel.fprintf outc "module %s = struct\n" (Ident.name ident);
        List.iter signature ~f:(walk ~indent:(indent + 1));
        Out_channel.fprintf outc "end;;\n"
      | Mty_functor (_, _, _) -> ()
      | Mty_alias _ -> ())
    | Sig_modtype (_ident, _, _) -> ()
    | Sig_class (_ident, _, _, _) -> ()
    | Sig_class_type (_ident, _, _, _) -> ()
  in
  List.iter cmi_infos.cmi_sign ~f:(walk ~indent:0)
