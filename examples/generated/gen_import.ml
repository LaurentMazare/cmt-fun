(* Explicitly add the [no_arg] function. *)
module Defunc = struct
  include Python_lib.Defunc

  let no_arg fn = return () |> map ~f:fn
end

let python_of_unit () = Py.none

let todo_param =
  Defunc.Of_python.create ~type_name:"todo" ~conv:(fun _ -> failwith "todo")

let todo_python_of _ = failwith "todo"
let python_of_bin_prot_shape_t = todo_python_of
let param_sexplib0_sexp_t = todo_param
let python_of_sexplib0_sexp_t = todo_python_of
let python_of_base___sign0_t = todo_python_of
let param_ppx_hash_lib_std_hash_state = todo_param
let python_of_ppx_hash_lib_std_hash_state = todo_python_of
let python_of_ppx_hash_lib_std_hash_hash_value = todo_python_of
let param_base___formatter_t = todo_param
let python_of_parts_t = todo_python_of
let param_core_kernel___int32_t = todo_param
let param_core_kernel___int63_t = todo_param
let python_of_core_kernel___int63_t = todo_python_of
let python_of_core_kernel___unit_of_time_t = todo_python_of
let param_core_kernel___unit_of_time_t = todo_param
let param_core_kernel___percent_t = todo_param
let param_core___import_time_t = todo_param
let python_of_index_t = todo_python_of
let param_core___import_time_date_and_ofday_t = todo_param
let param_index_t = todo_param
let python_of_core___import_time_span_t = todo_python_of
let python_of_core___import_time_t = todo_python_of
let python_of_core___import_time_date_and_ofday_t = todo_python_of
let python_of_ppx_sexp_conv_lib_sexp_t = todo_python_of
let param_unit = todo_param
let python_of_core___import_time_span_parts_t = todo_python_of
let param_core___import_time_span_t = todo_param
let param_core___import_time_ofday_t = todo_param
let param_zone_t = todo_param
let python_of_core___import_time_ofday_t = todo_python_of
let python_of_zone_t = todo_python_of
let param_core___import_date_t = todo_param
let param_core_kernel___date0_t = todo_param
let python_of_core_kernel___date0_t = todo_python_of
let param_core_kernel___zone_t = todo_param
let param_core_kernel___time_intf_date_t = todo_param
let python_of_core_kernel___time_intf_date_t = todo_python_of
let param_core___core_unix_tm = todo_param
let param_core___import_sexp_t = todo_param
let python_of_core___import_sexp_t = todo_python_of
let param_span_t = todo_param
let python_of_core___import_never_returns = todo_python_of
