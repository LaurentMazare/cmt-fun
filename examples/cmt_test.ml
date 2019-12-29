open! Base

let rec print_sign : string -> Types.signature_item -> unit =
 fun indent s ->
  match s with
  | Sig_value (ident, _, _) -> Stdio.printf "%svalue %s\n%!" indent (Ident.name ident)
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
  let cmi_infos, cmt_infos = Cmt_format.read "core__Core_date.cmti" in
  Stdio.printf "cmt loaded\n%!";
  Option.iter cmi_infos ~f:(fun cmi_infos ->
      Stdio.printf "cmi_name %s\n%!" cmi_infos.cmi_name;
      Stdio.printf "cmi_sign %d\n%!" (List.length cmi_infos.cmi_sign);
      List.iter cmi_infos.cmi_sign ~f:(print_sign "  "));
  Option.iter cmt_infos ~f:(fun cmt_infos ->
      Stdio.printf "cmt_name %s\n%!" cmt_infos.cmt_modname)
