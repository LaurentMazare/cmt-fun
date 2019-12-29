open! Base;;

let print_sign : Types.signature_item -> unit = function
  | Sig_value (ident, _, _) -> Stdio.printf "  value %s\n%!" (Ident.name ident)
  | Sig_type (ident, _, _, _) -> Stdio.printf "  type %s\n%!" (Ident.name ident)
  | Sig_typext (ident, _, _, _) -> Stdio.printf "  typext %s\n%!" (Ident.name ident)
  | Sig_module (ident, _, _, _, _) -> Stdio.printf "  module %s\n%!" (Ident.name ident)
  | Sig_modtype (ident, _, _) -> Stdio.printf "  modtype %s\n%!" (Ident.name ident)
  | Sig_class (ident, _, _, _) -> Stdio.printf "  class %s\n%!" (Ident.name ident)
  | Sig_class_type (ident, _, _, _) -> Stdio.printf "  classtype %s\n%!" (Ident.name ident)

let () =
  let cmi_infos, cmt_infos = Cmt_format.read "core__Core_date.cmti" in
  Stdio.printf "cmt loaded\n%!";
  Option.iter cmi_infos ~f:(fun cmi_infos ->
    Stdio.printf "cmi_name %s\n%!" cmi_infos.cmi_name;
    Stdio.printf "cmi_sign %d\n%!" (List.length cmi_infos.cmi_sign);
    List.iter cmi_infos.cmi_sign ~f:print_sign;
  );
  Option.iter cmt_infos ~f:(fun cmt_infos ->
    Stdio.printf "cmt_name %s\n%!" cmt_infos.cmt_modname)
;;
