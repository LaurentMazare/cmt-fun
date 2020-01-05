(* THIS CODE IS GENERATED AUTOMATICALLY, DO NOT EDIT BY HAND *)
open! Base
open! Python_lib
open! Python_lib.Let_syntax
open! Gen_import

let protect ~f x =
  try f x with
  | Py.Err _ as err -> raise err
  | exn -> raise (Py.Err (SyntaxError, Exn.to_string exn))
;;

module Span = struct
  let bin_shape_t () = (* Bin_prot.Shape.t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Span.bin_shape_t |> python_of_bin_prot__shape__t)
  ;;

  let t_of_sexp () = (* Sexplib0.Sexp.t -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" param_sexplib0__sexp__t ~docstring:"Sexplib0.Sexp.t"
    in
    Core__Core_time_float.Span.t_of_sexp
      positional_1
    |> python_of_core__core_time_float__span__t
  ;;

  let sexp_of_t () = (* Core__Core_time_float.Span.t -> Sexplib0.Sexp.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.sexp_of_t
      positional_1
    |> python_of_sexplib0__sexp__t
  ;;

  module Parts = struct
    let compare () = (* Core__Core_time_float.Span.Parts.t -> Core__Core_time_float.Span.Parts.t -> int *)
      let%map_open
        positional_1 = positional "positional_1" param_core__core_time_float__span__parts__t ~docstring:"Core__Core_time_float.Span.Parts.t" and
        positional_2 = positional "positional_2" param_core__core_time_float__span__parts__t ~docstring:"Core__Core_time_float.Span.Parts.t"
      in
      Core__Core_time_float.Span.Parts.compare
        positional_1
        positional_2
      |> python_of_int
    ;;

    let t_of_sexp () = (* Sexplib0.Sexp.t -> Core__Core_time_float.Span.Parts.t *)
      let%map_open
        positional_1 = positional "positional_1" param_sexplib0__sexp__t ~docstring:"Sexplib0.Sexp.t"
      in
      Core__Core_time_float.Span.Parts.t_of_sexp
        positional_1
      |> python_of_core__core_time_float__span__parts__t
    ;;

    let sexp_of_t () = (* Core__Core_time_float.Span.Parts.t -> Sexplib0.Sexp.t *)
      let%map_open
        positional_1 = positional "positional_1" param_core__core_time_float__span__parts__t ~docstring:"Core__Core_time_float.Span.Parts.t"
      in
      Core__Core_time_float.Span.Parts.sexp_of_t
        positional_1
      |> python_of_sexplib0__sexp__t
    ;;


    let register_module ~module_name =
      let modl = Py_module.create module_name in
      Py_module.set modl "compare" (compare ());
      Py_module.set modl "t_of_sexp" (t_of_sexp ());
      Py_module.set modl "sexp_of_t" (sexp_of_t ());
      modl
  end;;
  let greatereq () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.(>=)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let lowereq () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.(<=)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let eq () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.(=)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let greater () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.(>)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let lower () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.(<)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let neq () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.(<>)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let equal () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.equal
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let compare () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> int *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.compare
      positional_1
      positional_2
    |> python_of_int
  ;;

  let min () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.min
      positional_1
      positional_2
    |> python_of_core__core_time_float__span__t
  ;;

  let max () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.max
      positional_1
      positional_2
    |> python_of_core__core_time_float__span__t
  ;;

  let ascending () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> int *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.ascending
      positional_1
      positional_2
    |> python_of_int
  ;;

  let descending () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> int *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.descending
      positional_1
      positional_2
    |> python_of_int
  ;;

  let between () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      low = keyword "low" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      high = keyword "high" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.between
      positional_1
      ~low
      ~high
    |> python_of_bool
  ;;

  let clamp_exn () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      min = keyword "min" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      max = keyword "max" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.clamp_exn
      positional_1
      ~min
      ~max
    |> python_of_core__core_time_float__span__t
  ;;

  let is_positive () = (* Core__Core_time_float.Span.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.is_positive
      positional_1
    |> python_of_bool
  ;;

  let is_non_negative () = (* Core__Core_time_float.Span.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.is_non_negative
      positional_1
    |> python_of_bool
  ;;

  let is_negative () = (* Core__Core_time_float.Span.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.is_negative
      positional_1
    |> python_of_bool
  ;;

  let is_non_positive () = (* Core__Core_time_float.Span.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.is_non_positive
      positional_1
    |> python_of_bool
  ;;

  let sign () = (* Core__Core_time_float.Span.t -> Base__.Sign0.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.sign
      positional_1
    |> python_of_base____sign0__t
  ;;

  let hash_fold_t () = (* Ppx_hash_lib.Std.Hash.state -> Core__Core_time_float.Span.t -> Ppx_hash_lib.Std.Hash.state *)
    let%map_open
      positional_1 = positional "positional_1" param_ppx_hash_lib__std__hash__state ~docstring:"Ppx_hash_lib.Std.Hash.state" and
      positional_2 = positional "positional_2" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.hash_fold_t
      positional_1
      positional_2
    |> python_of_ppx_hash_lib__std__hash__state
  ;;

  let hash () = (* Core__Core_time_float.Span.t -> Ppx_hash_lib.Std.Hash.hash_value *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.hash
      positional_1
    |> python_of_ppx_hash_lib__std__hash__hash_value
  ;;

  let pp () = (* Base__.Formatter.t -> Core__Core_time_float.Span.t -> unit *)
    let%map_open
      positional_1 = positional "positional_1" param_base____formatter__t ~docstring:"Base__.Formatter.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.pp
      positional_1
      positional_2
    |> python_of_unit
  ;;

  let greatereq_approx () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.(>=.)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let lowereq_approx () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.(<=.)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let eq_approx () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.(=.)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let greater_approx () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.(>.)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let lower_approx () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.(<.)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let neq_approx () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.(<>.)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let robustly_compare () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> int *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.robustly_compare
      positional_1
      positional_2
    |> python_of_int
  ;;

  let to_string () = (* Core__Core_time_float.Span.t -> string *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.to_string
      positional_1
    |> python_of_string
  ;;

  let of_string () = (* string -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" string ~docstring:"string"
    in
    Core__Core_time_float.Span.of_string
      positional_1
    |> python_of_core__core_time_float__span__t
  ;;

  let nanosecond () = (* Core__Core_time_float.Span.t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Span.nanosecond |> python_of_core__core_time_float__span__t)
  ;;

  let microsecond () = (* Core__Core_time_float.Span.t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Span.microsecond |> python_of_core__core_time_float__span__t)
  ;;

  let millisecond () = (* Core__Core_time_float.Span.t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Span.millisecond |> python_of_core__core_time_float__span__t)
  ;;

  let second () = (* Core__Core_time_float.Span.t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Span.second |> python_of_core__core_time_float__span__t)
  ;;

  let minute () = (* Core__Core_time_float.Span.t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Span.minute |> python_of_core__core_time_float__span__t)
  ;;

  let hour () = (* Core__Core_time_float.Span.t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Span.hour |> python_of_core__core_time_float__span__t)
  ;;

  let day () = (* Core__Core_time_float.Span.t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Span.day |> python_of_core__core_time_float__span__t)
  ;;

  let robust_comparison_tolerance () = (* Core__Core_time_float.Span.t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Span.robust_comparison_tolerance |> python_of_core__core_time_float__span__t)
  ;;

  let zero () = (* Core__Core_time_float.Span.t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Span.zero |> python_of_core__core_time_float__span__t)
  ;;

  let to_parts () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.Parts.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.to_parts
      positional_1
    |> python_of_core__core_time_float__span__parts__t
  ;;

  let of_ns () = (* float -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" float ~docstring:"float"
    in
    Core__Core_time_float.Span.of_ns
      positional_1
    |> python_of_core__core_time_float__span__t
  ;;

  let of_us () = (* float -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" float ~docstring:"float"
    in
    Core__Core_time_float.Span.of_us
      positional_1
    |> python_of_core__core_time_float__span__t
  ;;

  let of_ms () = (* float -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" float ~docstring:"float"
    in
    Core__Core_time_float.Span.of_ms
      positional_1
    |> python_of_core__core_time_float__span__t
  ;;

  let of_sec () = (* float -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" float ~docstring:"float"
    in
    Core__Core_time_float.Span.of_sec
      positional_1
    |> python_of_core__core_time_float__span__t
  ;;

  let of_int_sec () = (* int -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" int ~docstring:"int"
    in
    Core__Core_time_float.Span.of_int_sec
      positional_1
    |> python_of_core__core_time_float__span__t
  ;;

  let of_int32_seconds () = (* Core_kernel__.Int32.t -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel____int32__t ~docstring:"Core_kernel__.Int32.t"
    in
    Core__Core_time_float.Span.of_int32_seconds
      positional_1
    |> python_of_core__core_time_float__span__t
  ;;

  let of_int63_seconds () = (* Core_kernel__.Int63.t -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel____int63__t ~docstring:"Core_kernel__.Int63.t"
    in
    Core__Core_time_float.Span.of_int63_seconds
      positional_1
    |> python_of_core__core_time_float__span__t
  ;;

  let of_min () = (* float -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" float ~docstring:"float"
    in
    Core__Core_time_float.Span.of_min
      positional_1
    |> python_of_core__core_time_float__span__t
  ;;

  let of_hr () = (* float -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" float ~docstring:"float"
    in
    Core__Core_time_float.Span.of_hr
      positional_1
    |> python_of_core__core_time_float__span__t
  ;;

  let of_day () = (* float -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" float ~docstring:"float"
    in
    Core__Core_time_float.Span.of_day
      positional_1
    |> python_of_core__core_time_float__span__t
  ;;

  let to_ns () = (* Core__Core_time_float.Span.t -> float *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.to_ns
      positional_1
    |> python_of_float
  ;;

  let to_us () = (* Core__Core_time_float.Span.t -> float *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.to_us
      positional_1
    |> python_of_float
  ;;

  let to_ms () = (* Core__Core_time_float.Span.t -> float *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.to_ms
      positional_1
    |> python_of_float
  ;;

  let to_sec () = (* Core__Core_time_float.Span.t -> float *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.to_sec
      positional_1
    |> python_of_float
  ;;

  let to_min () = (* Core__Core_time_float.Span.t -> float *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.to_min
      positional_1
    |> python_of_float
  ;;

  let to_hr () = (* Core__Core_time_float.Span.t -> float *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.to_hr
      positional_1
    |> python_of_float
  ;;

  let to_day () = (* Core__Core_time_float.Span.t -> float *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.to_day
      positional_1
    |> python_of_float
  ;;

  let to_int63_seconds_round_down_exn () = (* Core__Core_time_float.Span.t -> Core_kernel__.Int63.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.to_int63_seconds_round_down_exn
      positional_1
    |> python_of_core_kernel____int63__t
  ;;

  let to_proportional_float () = (* Core__Core_time_float.Span.t -> float *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.to_proportional_float
      positional_1
    |> python_of_float
  ;;

  let plus () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.(+)
      positional_1
      positional_2
    |> python_of_core__core_time_float__span__t
  ;;

  let minus () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.(-)
      positional_1
      positional_2
    |> python_of_core__core_time_float__span__t
  ;;

  let abs () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.abs
      positional_1
    |> python_of_core__core_time_float__span__t
  ;;

  let neg () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.neg
      positional_1
    |> python_of_core__core_time_float__span__t
  ;;

  let scale () = (* Core__Core_time_float.Span.t -> float -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      positional_2 = positional "positional_2" float ~docstring:"float"
    in
    Core__Core_time_float.Span.scale
      positional_1
      positional_2
    |> python_of_core__core_time_float__span__t
  ;;

  let divide () = (* Core__Core_time_float.Span.t -> float -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      positional_2 = positional "positional_2" float ~docstring:"float"
    in
    Core__Core_time_float.Span.(/)
      positional_1
      positional_2
    |> python_of_core__core_time_float__span__t
  ;;

  let divide_ () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t -> float *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.(//)
      positional_1
      positional_2
    |> python_of_float
  ;;

  let next () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.next
      positional_1
    |> python_of_core__core_time_float__span__t
  ;;

  let prev () = (* Core__Core_time_float.Span.t -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.prev
      positional_1
    |> python_of_core__core_time_float__span__t
  ;;

  let to_short_string () = (* Core__Core_time_float.Span.t -> string *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.to_short_string
      positional_1
    |> python_of_string
  ;;

  let to_unit_of_time () = (* Core__Core_time_float.Span.t -> Core_kernel__.Unit_of_time.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
    in
    Core__Core_time_float.Span.to_unit_of_time
      positional_1
    |> python_of_core_kernel____unit_of_time__t
  ;;

  let of_unit_of_time () = (* Core_kernel__.Unit_of_time.t -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel____unit_of_time__t ~docstring:"Core_kernel__.Unit_of_time.t"
    in
    Core__Core_time_float.Span.of_unit_of_time
      positional_1
    |> python_of_core__core_time_float__span__t
  ;;

  let randomize () = (* Core__Core_time_float.Span.t -> Core_kernel__.Percent.t -> Core__Core_time_float.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t" and
      percent = keyword "percent" param_core_kernel____percent__t ~docstring:"Core_kernel__.Percent.t"
    in
    Core__Core_time_float.Span.randomize
      positional_1
      ~percent
    |> python_of_core__core_time_float__span__t
  ;;


  let register_module ~module_name =
    let modl = Py_module.create module_name in
    Py_module.set modl "bin_shape_t" (bin_shape_t ());
    Py_module.set modl "t_of_sexp" (t_of_sexp ());
    Py_module.set modl "sexp_of_t" (sexp_of_t ());
    let subm = Parts.register_module ~module_name:"core__core_time_float__span__parts__parts" in
    Py_module.set_value modl "parts" (Py_module.to_pyobject subm);
    Py_module.set modl "greatereq" (greatereq ());
    Py_module.set modl "lowereq" (lowereq ());
    Py_module.set modl "eq" (eq ());
    Py_module.set modl "greater" (greater ());
    Py_module.set modl "lower" (lower ());
    Py_module.set modl "neq" (neq ());
    Py_module.set modl "equal" (equal ());
    Py_module.set modl "compare" (compare ());
    Py_module.set modl "min" (min ());
    Py_module.set modl "max" (max ());
    Py_module.set modl "ascending" (ascending ());
    Py_module.set modl "descending" (descending ());
    Py_module.set modl "between" (between ());
    Py_module.set modl "clamp_exn" (clamp_exn ());
    Py_module.set modl "is_positive" (is_positive ());
    Py_module.set modl "is_non_negative" (is_non_negative ());
    Py_module.set modl "is_negative" (is_negative ());
    Py_module.set modl "is_non_positive" (is_non_positive ());
    Py_module.set modl "sign" (sign ());
    Py_module.set modl "hash_fold_t" (hash_fold_t ());
    Py_module.set modl "hash" (hash ());
    Py_module.set modl "pp" (pp ());
    Py_module.set modl "greatereq_approx" (greatereq_approx ());
    Py_module.set modl "lowereq_approx" (lowereq_approx ());
    Py_module.set modl "eq_approx" (eq_approx ());
    Py_module.set modl "greater_approx" (greater_approx ());
    Py_module.set modl "lower_approx" (lower_approx ());
    Py_module.set modl "neq_approx" (neq_approx ());
    Py_module.set modl "robustly_compare" (robustly_compare ());
    Py_module.set modl "to_string" (to_string ());
    Py_module.set modl "of_string" (of_string ());
    Py_module.set modl "nanosecond" (nanosecond ());
    Py_module.set modl "microsecond" (microsecond ());
    Py_module.set modl "millisecond" (millisecond ());
    Py_module.set modl "second" (second ());
    Py_module.set modl "minute" (minute ());
    Py_module.set modl "hour" (hour ());
    Py_module.set modl "day" (day ());
    Py_module.set modl "robust_comparison_tolerance" (robust_comparison_tolerance ());
    Py_module.set modl "zero" (zero ());
    Py_module.set modl "to_parts" (to_parts ());
    Py_module.set modl "of_ns" (of_ns ());
    Py_module.set modl "of_us" (of_us ());
    Py_module.set modl "of_ms" (of_ms ());
    Py_module.set modl "of_sec" (of_sec ());
    Py_module.set modl "of_int_sec" (of_int_sec ());
    Py_module.set modl "of_int32_seconds" (of_int32_seconds ());
    Py_module.set modl "of_int63_seconds" (of_int63_seconds ());
    Py_module.set modl "of_min" (of_min ());
    Py_module.set modl "of_hr" (of_hr ());
    Py_module.set modl "of_day" (of_day ());
    Py_module.set modl "to_ns" (to_ns ());
    Py_module.set modl "to_us" (to_us ());
    Py_module.set modl "to_ms" (to_ms ());
    Py_module.set modl "to_sec" (to_sec ());
    Py_module.set modl "to_min" (to_min ());
    Py_module.set modl "to_hr" (to_hr ());
    Py_module.set modl "to_day" (to_day ());
    Py_module.set modl "to_int63_seconds_round_down_exn" (to_int63_seconds_round_down_exn ());
    Py_module.set modl "to_proportional_float" (to_proportional_float ());
    Py_module.set modl "plus" (plus ());
    Py_module.set modl "minus" (minus ());
    Py_module.set modl "abs" (abs ());
    Py_module.set modl "neg" (neg ());
    Py_module.set modl "scale" (scale ());
    Py_module.set modl "divide" (divide ());
    Py_module.set modl "divide_" (divide_ ());
    Py_module.set modl "next" (next ());
    Py_module.set modl "prev" (prev ());
    Py_module.set modl "to_short_string" (to_short_string ());
    Py_module.set modl "to_unit_of_time" (to_unit_of_time ());
    Py_module.set modl "of_unit_of_time" (of_unit_of_time ());
    Py_module.set modl "randomize" (randomize ());
    modl
end;;
module Zone = struct
  let input_tz_file () = (* string -> string -> Core__Core_time_float.Zone.t *)
    let%map_open
      zonename = keyword "zonename" string ~docstring:"string" and
      filename = keyword "filename" string ~docstring:"string"
    in
    Core__Core_time_float.Zone.input_tz_file
      ~zonename
      ~filename
    |> python_of_core__core_time_float__zone__t
  ;;

  let of_utc_offset () = (* int -> Core__Core_time_float.Zone.t *)
    let%map_open
      hours = keyword "hours" int ~docstring:"int"
    in
    Core__Core_time_float.Zone.of_utc_offset
      ~hours
    |> python_of_core__core_time_float__zone__t
  ;;

  let utc () = (* Core__Core_time_float.Zone.t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Zone.utc |> python_of_core__core_time_float__zone__t)
  ;;

  let name () = (* Core__Core_time_float.Zone.t -> string *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
    in
    Core__Core_time_float.Zone.name
      positional_1
    |> python_of_string
  ;;

  let reset_transition_cache () = (* Core__Core_time_float.Zone.t -> unit *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
    in
    Core__Core_time_float.Zone.reset_transition_cache
      positional_1
    |> python_of_unit
  ;;

  module Index = struct
    let next () = (* Core__Core_time_float.Zone.Index.t -> Core__Core_time_float.Zone.Index.t *)
      let%map_open
        positional_1 = positional "positional_1" param_core__core_time_float__zone__index__t ~docstring:"Core__Core_time_float.Zone.Index.t"
      in
      Core__Core_time_float.Zone.Index.next
        positional_1
      |> python_of_core__core_time_float__zone__index__t
    ;;

    let prev () = (* Core__Core_time_float.Zone.Index.t -> Core__Core_time_float.Zone.Index.t *)
      let%map_open
        positional_1 = positional "positional_1" param_core__core_time_float__zone__index__t ~docstring:"Core__Core_time_float.Zone.Index.t"
      in
      Core__Core_time_float.Zone.Index.prev
        positional_1
      |> python_of_core__core_time_float__zone__index__t
    ;;


    let register_module ~module_name =
      let modl = Py_module.create module_name in
      Py_module.set modl "next" (next ());
      Py_module.set modl "prev" (prev ());
      modl
  end;;
  let index () = (* Core__Core_time_float.Zone.t -> Core__.Import.Time.t -> Core__Core_time_float.Zone.Index.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core____import__time__t ~docstring:"Core__.Import.Time.t"
    in
    Core__Core_time_float.Zone.index
      positional_1
      positional_2
    |> python_of_core__core_time_float__zone__index__t
  ;;

  let index_of_date_and_ofday () = (* Core__Core_time_float.Zone.t -> Core__.Import.Time.Date_and_ofday.t -> Core__Core_time_float.Zone.Index.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core____import__time__date_and_ofday__t ~docstring:"Core__.Import.Time.Date_and_ofday.t"
    in
    Core__Core_time_float.Zone.index_of_date_and_ofday
      positional_1
      positional_2
    |> python_of_core__core_time_float__zone__index__t
  ;;

  let index_offset_from_utc_exn () = (* Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.Index.t -> Core__.Import.Time.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__zone__index__t ~docstring:"Core__Core_time_float.Zone.Index.t"
    in
    Core__Core_time_float.Zone.index_offset_from_utc_exn
      positional_1
      positional_2
    |> python_of_core____import__time__span__t
  ;;

  let index_abbreviation_exn () = (* Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.Index.t -> string *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__zone__index__t ~docstring:"Core__Core_time_float.Zone.Index.t"
    in
    Core__Core_time_float.Zone.index_abbreviation_exn
      positional_1
      positional_2
    |> python_of_string
  ;;

  let index_has_prev_clock_shift () = (* Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.Index.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__zone__index__t ~docstring:"Core__Core_time_float.Zone.Index.t"
    in
    Core__Core_time_float.Zone.index_has_prev_clock_shift
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let index_prev_clock_shift_time_exn () = (* Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.Index.t -> Core__.Import.Time.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__zone__index__t ~docstring:"Core__Core_time_float.Zone.Index.t"
    in
    Core__Core_time_float.Zone.index_prev_clock_shift_time_exn
      positional_1
      positional_2
    |> python_of_core____import__time__t
  ;;

  let index_prev_clock_shift_amount_exn () = (* Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.Index.t -> Core__.Import.Time.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__zone__index__t ~docstring:"Core__Core_time_float.Zone.Index.t"
    in
    Core__Core_time_float.Zone.index_prev_clock_shift_amount_exn
      positional_1
      positional_2
    |> python_of_core____import__time__span__t
  ;;

  let index_has_next_clock_shift () = (* Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.Index.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__zone__index__t ~docstring:"Core__Core_time_float.Zone.Index.t"
    in
    Core__Core_time_float.Zone.index_has_next_clock_shift
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let index_next_clock_shift_time_exn () = (* Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.Index.t -> Core__.Import.Time.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__zone__index__t ~docstring:"Core__Core_time_float.Zone.Index.t"
    in
    Core__Core_time_float.Zone.index_next_clock_shift_time_exn
      positional_1
      positional_2
    |> python_of_core____import__time__t
  ;;

  let index_next_clock_shift_amount_exn () = (* Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.Index.t -> Core__.Import.Time.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__zone__index__t ~docstring:"Core__Core_time_float.Zone.Index.t"
    in
    Core__Core_time_float.Zone.index_next_clock_shift_amount_exn
      positional_1
      positional_2
    |> python_of_core____import__time__span__t
  ;;

  let abbreviation () = (* Core__Core_time_float.Zone.t -> Core__.Import.Time.t -> string *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core____import__time__t ~docstring:"Core__.Import.Time.t"
    in
    Core__Core_time_float.Zone.abbreviation
      positional_1
      positional_2
    |> python_of_string
  ;;

  let absolute_time_of_date_and_ofday () = (* Core__Core_time_float.Zone.t -> Core__.Import.Time.Date_and_ofday.t -> Core__.Import.Time.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core____import__time__date_and_ofday__t ~docstring:"Core__.Import.Time.Date_and_ofday.t"
    in
    Core__Core_time_float.Zone.absolute_time_of_date_and_ofday
      positional_1
      positional_2
    |> python_of_core____import__time__t
  ;;

  let date_and_ofday_of_absolute_time () = (* Core__Core_time_float.Zone.t -> Core__.Import.Time.t -> Core__.Import.Time.Date_and_ofday.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core____import__time__t ~docstring:"Core__.Import.Time.t"
    in
    Core__Core_time_float.Zone.date_and_ofday_of_absolute_time
      positional_1
      positional_2
    |> python_of_core____import__time__date_and_ofday__t
  ;;

  let bin_shape_t () = (* Bin_prot.Shape.t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Zone.bin_shape_t |> python_of_bin_prot__shape__t)
  ;;

  let t_of_sexp () = (* Sexplib0.Sexp.t -> Core__Core_time_float.Zone.t *)
    let%map_open
      positional_1 = positional "positional_1" param_sexplib0__sexp__t ~docstring:"Sexplib0.Sexp.t"
    in
    Core__Core_time_float.Zone.t_of_sexp
      positional_1
    |> python_of_core__core_time_float__zone__t
  ;;

  let sexp_of_t () = (* Core__Core_time_float.Zone.t -> Ppx_sexp_conv_lib.Sexp.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
    in
    Core__Core_time_float.Zone.sexp_of_t
      positional_1
    |> python_of_ppx_sexp_conv_lib__sexp__t
  ;;

  let of_string () = (* string -> Core__Core_time_float.Zone.t *)
    let%map_open
      positional_1 = positional "positional_1" string ~docstring:"string"
    in
    Core__Core_time_float.Zone.of_string
      positional_1
    |> python_of_core__core_time_float__zone__t
  ;;

  let to_string () = (* Core__Core_time_float.Zone.t -> string *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
    in
    Core__Core_time_float.Zone.to_string
      positional_1
    |> python_of_string
  ;;

  let pp () = (* Base__.Formatter.t -> Core__Core_time_float.Zone.t -> unit *)
    let%map_open
      positional_1 = positional "positional_1" param_base____formatter__t ~docstring:"Base__.Formatter.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
    in
    Core__Core_time_float.Zone.pp
      positional_1
      positional_2
    |> python_of_unit
  ;;

  let greatereq () = (* Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
    in
    Core__Core_time_float.Zone.(>=)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let lowereq () = (* Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
    in
    Core__Core_time_float.Zone.(<=)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let eq () = (* Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
    in
    Core__Core_time_float.Zone.(=)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let greater () = (* Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
    in
    Core__Core_time_float.Zone.(>)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let lower () = (* Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
    in
    Core__Core_time_float.Zone.(<)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let neq () = (* Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
    in
    Core__Core_time_float.Zone.(<>)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let equal () = (* Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
    in
    Core__Core_time_float.Zone.equal
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let compare () = (* Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.t -> int *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
    in
    Core__Core_time_float.Zone.compare
      positional_1
      positional_2
    |> python_of_int
  ;;

  let min () = (* Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
    in
    Core__Core_time_float.Zone.min
      positional_1
      positional_2
    |> python_of_core__core_time_float__zone__t
  ;;

  let max () = (* Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
    in
    Core__Core_time_float.Zone.max
      positional_1
      positional_2
    |> python_of_core__core_time_float__zone__t
  ;;

  let ascending () = (* Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.t -> int *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
    in
    Core__Core_time_float.Zone.ascending
      positional_1
      positional_2
    |> python_of_int
  ;;

  let descending () = (* Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.t -> int *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
    in
    Core__Core_time_float.Zone.descending
      positional_1
      positional_2
    |> python_of_int
  ;;

  let between () = (* Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      low = keyword "low" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      high = keyword "high" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
    in
    Core__Core_time_float.Zone.between
      positional_1
      ~low
      ~high
    |> python_of_bool
  ;;

  let clamp_exn () = (* Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.t -> Core__Core_time_float.Zone.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      min = keyword "min" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t" and
      max = keyword "max" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
    in
    Core__Core_time_float.Zone.clamp_exn
      positional_1
      ~min
      ~max
    |> python_of_core__core_time_float__zone__t
  ;;

  let hash_fold_t () = (* Ppx_hash_lib.Std.Hash.state -> Core__Core_time_float.Zone.t -> Ppx_hash_lib.Std.Hash.state *)
    let%map_open
      positional_1 = positional "positional_1" param_ppx_hash_lib__std__hash__state ~docstring:"Ppx_hash_lib.Std.Hash.state" and
      positional_2 = positional "positional_2" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
    in
    Core__Core_time_float.Zone.hash_fold_t
      positional_1
      positional_2
    |> python_of_ppx_hash_lib__std__hash__state
  ;;

  let hash () = (* Core__Core_time_float.Zone.t -> Ppx_hash_lib.Std.Hash.hash_value *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
    in
    Core__Core_time_float.Zone.hash
      positional_1
    |> python_of_ppx_hash_lib__std__hash__hash_value
  ;;

  let find_exn () = (* string -> Core__Core_time_float.Zone.t *)
    let%map_open
      positional_1 = positional "positional_1" string ~docstring:"string"
    in
    Core__Core_time_float.Zone.find_exn
      positional_1
    |> python_of_core__core_time_float__zone__t
  ;;

  let initialized_zones () = (* unit -> ((string, Core__Core_time_float.Zone.t)) list *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Zone.initialized_zones () |> (python_of_list (fun (t0, t1) -> Py.Tuple.of_list [python_of_string t0; python_of_core__core_time_float__zone__t t1])))
  ;;

  let init () = (* unit -> unit *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Zone.init () |> python_of_unit)
  ;;


  let register_module ~module_name =
    let modl = Py_module.create module_name in
    Py_module.set modl "input_tz_file" (input_tz_file ());
    Py_module.set modl "of_utc_offset" (of_utc_offset ());
    Py_module.set modl "utc" (utc ());
    Py_module.set modl "name" (name ());
    Py_module.set modl "reset_transition_cache" (reset_transition_cache ());
    let subm = Index.register_module ~module_name:"core__core_time_float__zone__index__index" in
    Py_module.set_value modl "index" (Py_module.to_pyobject subm);
    Py_module.set modl "index" (index ());
    Py_module.set modl "index_of_date_and_ofday" (index_of_date_and_ofday ());
    Py_module.set modl "index_offset_from_utc_exn" (index_offset_from_utc_exn ());
    Py_module.set modl "index_abbreviation_exn" (index_abbreviation_exn ());
    Py_module.set modl "index_has_prev_clock_shift" (index_has_prev_clock_shift ());
    Py_module.set modl "index_prev_clock_shift_time_exn" (index_prev_clock_shift_time_exn ());
    Py_module.set modl "index_prev_clock_shift_amount_exn" (index_prev_clock_shift_amount_exn ());
    Py_module.set modl "index_has_next_clock_shift" (index_has_next_clock_shift ());
    Py_module.set modl "index_next_clock_shift_time_exn" (index_next_clock_shift_time_exn ());
    Py_module.set modl "index_next_clock_shift_amount_exn" (index_next_clock_shift_amount_exn ());
    Py_module.set modl "abbreviation" (abbreviation ());
    Py_module.set modl "absolute_time_of_date_and_ofday" (absolute_time_of_date_and_ofday ());
    Py_module.set modl "date_and_ofday_of_absolute_time" (date_and_ofday_of_absolute_time ());
    Py_module.set modl "bin_shape_t" (bin_shape_t ());
    Py_module.set modl "t_of_sexp" (t_of_sexp ());
    Py_module.set modl "sexp_of_t" (sexp_of_t ());
    Py_module.set modl "of_string" (of_string ());
    Py_module.set modl "to_string" (to_string ());
    Py_module.set modl "pp" (pp ());
    Py_module.set modl "greatereq" (greatereq ());
    Py_module.set modl "lowereq" (lowereq ());
    Py_module.set modl "eq" (eq ());
    Py_module.set modl "greater" (greater ());
    Py_module.set modl "lower" (lower ());
    Py_module.set modl "neq" (neq ());
    Py_module.set modl "equal" (equal ());
    Py_module.set modl "compare" (compare ());
    Py_module.set modl "min" (min ());
    Py_module.set modl "max" (max ());
    Py_module.set modl "ascending" (ascending ());
    Py_module.set modl "descending" (descending ());
    Py_module.set modl "between" (between ());
    Py_module.set modl "clamp_exn" (clamp_exn ());
    Py_module.set modl "hash_fold_t" (hash_fold_t ());
    Py_module.set modl "hash" (hash ());
    Py_module.set modl "find_exn" (find_exn ());
    Py_module.set modl "initialized_zones" (initialized_zones ());
    Py_module.set modl "init" (init ());
    modl
end;;
module Ofday = struct
  let bin_shape_t () = (* Bin_prot.Shape.t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Ofday.bin_shape_t |> python_of_bin_prot__shape__t)
  ;;

  let t_of_sexp () = (* Sexplib0.Sexp.t -> Core__Core_time_float.Ofday.t *)
    let%map_open
      positional_1 = positional "positional_1" param_sexplib0__sexp__t ~docstring:"Sexplib0.Sexp.t"
    in
    Core__Core_time_float.Ofday.t_of_sexp
      positional_1
    |> python_of_core__core_time_float__ofday__t
  ;;

  let sexp_of_t () = (* Core__Core_time_float.Ofday.t -> Sexplib0.Sexp.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.sexp_of_t
      positional_1
    |> python_of_sexplib0__sexp__t
  ;;

  let greatereq () = (* Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.(>=)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let lowereq () = (* Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.(<=)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let eq () = (* Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.(=)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let greater () = (* Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.(>)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let lower () = (* Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.(<)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let neq () = (* Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.(<>)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let equal () = (* Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.equal
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let compare () = (* Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> int *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.compare
      positional_1
      positional_2
    |> python_of_int
  ;;

  let min () = (* Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.min
      positional_1
      positional_2
    |> python_of_core__core_time_float__ofday__t
  ;;

  let max () = (* Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.max
      positional_1
      positional_2
    |> python_of_core__core_time_float__ofday__t
  ;;

  let ascending () = (* Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> int *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.ascending
      positional_1
      positional_2
    |> python_of_int
  ;;

  let descending () = (* Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> int *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.descending
      positional_1
      positional_2
    |> python_of_int
  ;;

  let between () = (* Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      low = keyword "low" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      high = keyword "high" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.between
      positional_1
      ~low
      ~high
    |> python_of_bool
  ;;

  let clamp_exn () = (* Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      min = keyword "min" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      max = keyword "max" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.clamp_exn
      positional_1
      ~min
      ~max
    |> python_of_core__core_time_float__ofday__t
  ;;

  let hash_fold_t () = (* Ppx_hash_lib.Std.Hash.state -> Core__Core_time_float.Ofday.t -> Ppx_hash_lib.Std.Hash.state *)
    let%map_open
      positional_1 = positional "positional_1" param_ppx_hash_lib__std__hash__state ~docstring:"Ppx_hash_lib.Std.Hash.state" and
      positional_2 = positional "positional_2" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.hash_fold_t
      positional_1
      positional_2
    |> python_of_ppx_hash_lib__std__hash__state
  ;;

  let hash () = (* Core__Core_time_float.Ofday.t -> Ppx_hash_lib.Std.Hash.hash_value *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.hash
      positional_1
    |> python_of_ppx_hash_lib__std__hash__hash_value
  ;;

  let pp () = (* Base__.Formatter.t -> Core__Core_time_float.Ofday.t -> unit *)
    let%map_open
      positional_1 = positional "positional_1" param_base____formatter__t ~docstring:"Base__.Formatter.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.pp
      positional_1
      positional_2
    |> python_of_unit
  ;;

  let greatereq_approx () = (* Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.(>=.)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let lowereq_approx () = (* Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.(<=.)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let eq_approx () = (* Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.(=.)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let greater_approx () = (* Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.(>.)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let lower_approx () = (* Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.(<.)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let neq_approx () = (* Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> bool *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.(<>.)
      positional_1
      positional_2
    |> python_of_bool
  ;;

  let robustly_compare () = (* Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> int *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.robustly_compare
      positional_1
      positional_2
    |> python_of_int
  ;;

  let of_string () = (* string -> Core__Core_time_float.Ofday.t *)
    let%map_open
      positional_1 = positional "positional_1" string ~docstring:"string"
    in
    Core__Core_time_float.Ofday.of_string
      positional_1
    |> python_of_core__core_time_float__ofday__t
  ;;

  let to_string () = (* Core__Core_time_float.Ofday.t -> string *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.to_string
      positional_1
    |> python_of_string
  ;;

  let to_parts () = (* Core__Core_time_float.Ofday.t -> Core__.Import.Time.Span.Parts.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.to_parts
      positional_1
    |> python_of_core____import__time__span__parts__t
  ;;

  let start_of_day () = (* Core__Core_time_float.Ofday.t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Ofday.start_of_day |> python_of_core__core_time_float__ofday__t)
  ;;

  let start_of_next_day () = (* Core__Core_time_float.Ofday.t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Ofday.start_of_next_day |> python_of_core__core_time_float__ofday__t)
  ;;

  let approximate_end_of_day () = (* Core__Core_time_float.Ofday.t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Ofday.approximate_end_of_day |> python_of_core__core_time_float__ofday__t)
  ;;

  let to_span_since_start_of_day () = (* Core__Core_time_float.Ofday.t -> Core__.Import.Time.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.to_span_since_start_of_day
      positional_1
    |> python_of_core____import__time__span__t
  ;;

  let of_span_since_start_of_day_exn () = (* Core__.Import.Time.Span.t -> Core__Core_time_float.Ofday.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core____import__time__span__t ~docstring:"Core__.Import.Time.Span.t"
    in
    Core__Core_time_float.Ofday.of_span_since_start_of_day_exn
      positional_1
    |> python_of_core__core_time_float__ofday__t
  ;;

  let of_span_since_start_of_day () = (* Core__.Import.Time.Span.t -> Core__Core_time_float.Ofday.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core____import__time__span__t ~docstring:"Core__.Import.Time.Span.t"
    in
    Core__Core_time_float.Ofday.of_span_since_start_of_day
      positional_1
    |> python_of_core__core_time_float__ofday__t
  ;;

  let diff () = (* Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> Core__.Import.Time.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.diff
      positional_1
      positional_2
    |> python_of_core____import__time__span__t
  ;;

  let small_diff () = (* Core__Core_time_float.Ofday.t -> Core__Core_time_float.Ofday.t -> Core__.Import.Time.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t" and
      positional_2 = positional "positional_2" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.small_diff
      positional_1
      positional_2
    |> python_of_core____import__time__span__t
  ;;

  let to_string_trimmed () = (* Core__Core_time_float.Ofday.t -> string *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.to_string_trimmed
      positional_1
    |> python_of_string
  ;;

  let to_sec_string () = (* Core__Core_time_float.Ofday.t -> string *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.to_sec_string
      positional_1
    |> python_of_string
  ;;

  let to_millisecond_string () = (* Core__Core_time_float.Ofday.t -> string *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.to_millisecond_string
      positional_1
    |> python_of_string
  ;;

  let to_millisec_string () = (* Core__Core_time_float.Ofday.t -> string *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__ofday__t ~docstring:"Core__Core_time_float.Ofday.t"
    in
    Core__Core_time_float.Ofday.to_millisec_string
      positional_1
    |> python_of_string
  ;;

  module Zoned = struct
    let bin_shape_t () = (* Bin_prot.Shape.t *)
      Defunc.no_arg (fun () -> Core__Core_time_float.Ofday.Zoned.bin_shape_t |> python_of_bin_prot__shape__t)
    ;;

    let t_of_sexp () = (* Sexplib0.Sexp.t -> Core__Core_time_float.Ofday.Zoned.t *)
      let%map_open
        positional_1 = positional "positional_1" param_sexplib0__sexp__t ~docstring:"Sexplib0.Sexp.t"
      in
      Core__Core_time_float.Ofday.Zoned.t_of_sexp
        positional_1
      |> python_of_core__core_time_float__ofday__zoned__t
    ;;

    let sexp_of_t () = (* Core__Core_time_float.Ofday.Zoned.t -> Sexplib0.Sexp.t *)
      let%map_open
        positional_1 = positional "positional_1" param_core__core_time_float__ofday__zoned__t ~docstring:"Core__Core_time_float.Ofday.Zoned.t"
      in
      Core__Core_time_float.Ofday.Zoned.sexp_of_t
        positional_1
      |> python_of_sexplib0__sexp__t
    ;;

    let hash_fold_t () = (* Ppx_hash_lib.Std.Hash.state -> Core__Core_time_float.Ofday.Zoned.t -> Ppx_hash_lib.Std.Hash.state *)
      let%map_open
        positional_1 = positional "positional_1" param_ppx_hash_lib__std__hash__state ~docstring:"Ppx_hash_lib.Std.Hash.state" and
        positional_2 = positional "positional_2" param_core__core_time_float__ofday__zoned__t ~docstring:"Core__Core_time_float.Ofday.Zoned.t"
      in
      Core__Core_time_float.Ofday.Zoned.hash_fold_t
        positional_1
        positional_2
      |> python_of_ppx_hash_lib__std__hash__state
    ;;

    let hash () = (* Core__Core_time_float.Ofday.Zoned.t -> Ppx_hash_lib.Std.Hash.hash_value *)
      let%map_open
        positional_1 = positional "positional_1" param_core__core_time_float__ofday__zoned__t ~docstring:"Core__Core_time_float.Ofday.Zoned.t"
      in
      Core__Core_time_float.Ofday.Zoned.hash
        positional_1
      |> python_of_ppx_hash_lib__std__hash__hash_value
    ;;

    let pp () = (* Base__.Formatter.t -> Core__Core_time_float.Ofday.Zoned.t -> unit *)
      let%map_open
        positional_1 = positional "positional_1" param_base____formatter__t ~docstring:"Base__.Formatter.t" and
        positional_2 = positional "positional_2" param_core__core_time_float__ofday__zoned__t ~docstring:"Core__Core_time_float.Ofday.Zoned.t"
      in
      Core__Core_time_float.Ofday.Zoned.pp
        positional_1
        positional_2
      |> python_of_unit
    ;;

    let of_string () = (* string -> Core__Core_time_float.Ofday.Zoned.t *)
      let%map_open
        positional_1 = positional "positional_1" string ~docstring:"string"
      in
      Core__Core_time_float.Ofday.Zoned.of_string
        positional_1
      |> python_of_core__core_time_float__ofday__zoned__t
    ;;

    let to_string () = (* Core__Core_time_float.Ofday.Zoned.t -> string *)
      let%map_open
        positional_1 = positional "positional_1" param_core__core_time_float__ofday__zoned__t ~docstring:"Core__Core_time_float.Ofday.Zoned.t"
      in
      Core__Core_time_float.Ofday.Zoned.to_string
        positional_1
      |> python_of_string
    ;;

    let create () = (* Core__.Import.Time.Ofday.t -> Core__Core_time_float.Zone.t -> Core__Core_time_float.Ofday.Zoned.t *)
      let%map_open
        positional_1 = positional "positional_1" param_core____import__time__ofday__t ~docstring:"Core__.Import.Time.Ofday.t" and
        positional_2 = positional "positional_2" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
      in
      Core__Core_time_float.Ofday.Zoned.create
        positional_1
        positional_2
      |> python_of_core__core_time_float__ofday__zoned__t
    ;;

    let create_local () = (* Core__.Import.Time.Ofday.t -> Core__Core_time_float.Ofday.Zoned.t *)
      let%map_open
        positional_1 = positional "positional_1" param_core____import__time__ofday__t ~docstring:"Core__.Import.Time.Ofday.t"
      in
      Core__Core_time_float.Ofday.Zoned.create_local
        positional_1
      |> python_of_core__core_time_float__ofday__zoned__t
    ;;

    let ofday () = (* Core__Core_time_float.Ofday.Zoned.t -> Core__.Import.Time.Ofday.t *)
      let%map_open
        positional_1 = positional "positional_1" param_core__core_time_float__ofday__zoned__t ~docstring:"Core__Core_time_float.Ofday.Zoned.t"
      in
      Core__Core_time_float.Ofday.Zoned.ofday
        positional_1
      |> python_of_core____import__time__ofday__t
    ;;

    let zone () = (* Core__Core_time_float.Ofday.Zoned.t -> Core__Core_time_float.Zone.t *)
      let%map_open
        positional_1 = positional "positional_1" param_core__core_time_float__ofday__zoned__t ~docstring:"Core__Core_time_float.Ofday.Zoned.t"
      in
      Core__Core_time_float.Ofday.Zoned.zone
        positional_1
      |> python_of_core__core_time_float__zone__t
    ;;

    let to_time () = (* Core__Core_time_float.Ofday.Zoned.t -> Core__.Import.Date.t -> Core__.Import.Time.t *)
      let%map_open
        positional_1 = positional "positional_1" param_core__core_time_float__ofday__zoned__t ~docstring:"Core__Core_time_float.Ofday.Zoned.t" and
        positional_2 = positional "positional_2" param_core____import__date__t ~docstring:"Core__.Import.Date.t"
      in
      Core__Core_time_float.Ofday.Zoned.to_time
        positional_1
        positional_2
      |> python_of_core____import__time__t
    ;;

    module With_nonchronological_compare = struct
      let bin_shape_t () = (* Bin_prot.Shape.t *)
        Defunc.no_arg (fun () -> Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.bin_shape_t |> python_of_bin_prot__shape__t)
      ;;

      let t_of_sexp () = (* Sexplib0.Sexp.t -> Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.t *)
        let%map_open
          positional_1 = positional "positional_1" param_sexplib0__sexp__t ~docstring:"Sexplib0.Sexp.t"
        in
        Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.t_of_sexp
          positional_1
        |> python_of_core__core_time_float__ofday__zoned__with_nonchronological_compare__t
      ;;

      let sexp_of_t () = (* Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.t -> Sexplib0.Sexp.t *)
        let%map_open
          positional_1 = positional "positional_1" param_core__core_time_float__ofday__zoned__with_nonchronological_compare__t ~docstring:"Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.t"
        in
        Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.sexp_of_t
          positional_1
        |> python_of_sexplib0__sexp__t
      ;;

      let compare () = (* Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.t -> Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.t -> int *)
        let%map_open
          positional_1 = positional "positional_1" param_core__core_time_float__ofday__zoned__with_nonchronological_compare__t ~docstring:"Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.t" and
          positional_2 = positional "positional_2" param_core__core_time_float__ofday__zoned__with_nonchronological_compare__t ~docstring:"Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.t"
        in
        Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.compare
          positional_1
          positional_2
        |> python_of_int
      ;;

      let hash_fold_t () = (* Ppx_hash_lib.Std.Hash.state -> Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.t -> Ppx_hash_lib.Std.Hash.state *)
        let%map_open
          positional_1 = positional "positional_1" param_ppx_hash_lib__std__hash__state ~docstring:"Ppx_hash_lib.Std.Hash.state" and
          positional_2 = positional "positional_2" param_core__core_time_float__ofday__zoned__with_nonchronological_compare__t ~docstring:"Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.t"
        in
        Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.hash_fold_t
          positional_1
          positional_2
        |> python_of_ppx_hash_lib__std__hash__state
      ;;

      let hash () = (* Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.t -> Ppx_hash_lib.Std.Hash.hash_value *)
        let%map_open
          positional_1 = positional "positional_1" param_core__core_time_float__ofday__zoned__with_nonchronological_compare__t ~docstring:"Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.t"
        in
        Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.hash
          positional_1
        |> python_of_ppx_hash_lib__std__hash__hash_value
      ;;


      let register_module ~module_name =
        let modl = Py_module.create module_name in
        Py_module.set modl "bin_shape_t" (bin_shape_t ());
        Py_module.set modl "t_of_sexp" (t_of_sexp ());
        Py_module.set modl "sexp_of_t" (sexp_of_t ());
        Py_module.set modl "compare" (compare ());
        Py_module.set modl "hash_fold_t" (hash_fold_t ());
        Py_module.set modl "hash" (hash ());
        modl
    end;;

    let register_module ~module_name =
      let modl = Py_module.create module_name in
      Py_module.set modl "bin_shape_t" (bin_shape_t ());
      Py_module.set modl "t_of_sexp" (t_of_sexp ());
      Py_module.set modl "sexp_of_t" (sexp_of_t ());
      Py_module.set modl "hash_fold_t" (hash_fold_t ());
      Py_module.set modl "hash" (hash ());
      Py_module.set modl "pp" (pp ());
      Py_module.set modl "of_string" (of_string ());
      Py_module.set modl "to_string" (to_string ());
      Py_module.set modl "create" (create ());
      Py_module.set modl "create_local" (create_local ());
      Py_module.set modl "ofday" (ofday ());
      Py_module.set modl "zone" (zone ());
      Py_module.set modl "to_time" (to_time ());
      let subm = With_nonchronological_compare.register_module ~module_name:"core__core_time_float__ofday__zoned__with_nonchronological_compare__with_nonchronological_compare" in
      Py_module.set_value modl "with_nonchronological_compare" (Py_module.to_pyobject subm);
      modl
  end;;
  let now () = (* Core__Core_time_float.Zone.t -> Core__Core_time_float.Ofday.t *)
    let%map_open
      zone = keyword "zone" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
    in
    Core__Core_time_float.Ofday.now
      ~zone
    |> python_of_core__core_time_float__ofday__t
  ;;


  let register_module ~module_name =
    let modl = Py_module.create module_name in
    Py_module.set modl "bin_shape_t" (bin_shape_t ());
    Py_module.set modl "t_of_sexp" (t_of_sexp ());
    Py_module.set modl "sexp_of_t" (sexp_of_t ());
    Py_module.set modl "greatereq" (greatereq ());
    Py_module.set modl "lowereq" (lowereq ());
    Py_module.set modl "eq" (eq ());
    Py_module.set modl "greater" (greater ());
    Py_module.set modl "lower" (lower ());
    Py_module.set modl "neq" (neq ());
    Py_module.set modl "equal" (equal ());
    Py_module.set modl "compare" (compare ());
    Py_module.set modl "min" (min ());
    Py_module.set modl "max" (max ());
    Py_module.set modl "ascending" (ascending ());
    Py_module.set modl "descending" (descending ());
    Py_module.set modl "between" (between ());
    Py_module.set modl "clamp_exn" (clamp_exn ());
    Py_module.set modl "hash_fold_t" (hash_fold_t ());
    Py_module.set modl "hash" (hash ());
    Py_module.set modl "pp" (pp ());
    Py_module.set modl "greatereq_approx" (greatereq_approx ());
    Py_module.set modl "lowereq_approx" (lowereq_approx ());
    Py_module.set modl "eq_approx" (eq_approx ());
    Py_module.set modl "greater_approx" (greater_approx ());
    Py_module.set modl "lower_approx" (lower_approx ());
    Py_module.set modl "neq_approx" (neq_approx ());
    Py_module.set modl "robustly_compare" (robustly_compare ());
    Py_module.set modl "of_string" (of_string ());
    Py_module.set modl "to_string" (to_string ());
    Py_module.set modl "to_parts" (to_parts ());
    Py_module.set modl "start_of_day" (start_of_day ());
    Py_module.set modl "start_of_next_day" (start_of_next_day ());
    Py_module.set modl "approximate_end_of_day" (approximate_end_of_day ());
    Py_module.set modl "to_span_since_start_of_day" (to_span_since_start_of_day ());
    Py_module.set modl "of_span_since_start_of_day_exn" (of_span_since_start_of_day_exn ());
    Py_module.set modl "of_span_since_start_of_day" (of_span_since_start_of_day ());
    Py_module.set modl "diff" (diff ());
    Py_module.set modl "small_diff" (small_diff ());
    Py_module.set modl "to_string_trimmed" (to_string_trimmed ());
    Py_module.set modl "to_sec_string" (to_sec_string ());
    Py_module.set modl "to_millisecond_string" (to_millisecond_string ());
    Py_module.set modl "to_millisec_string" (to_millisec_string ());
    let subm = Zoned.register_module ~module_name:"core__core_time_float__ofday__zoned__zoned" in
    Py_module.set_value modl "zoned" (Py_module.to_pyobject subm);
    Py_module.set modl "now" (now ());
    modl
end;;
let next () = (* Core__Core_time_float.t -> Core__Core_time_float.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.next
    positional_1
  |> python_of_core__core_time_float__t
;;

let prev () = (* Core__Core_time_float.t -> Core__Core_time_float.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.prev
    positional_1
  |> python_of_core__core_time_float__t
;;

let to_span_since_epoch () = (* Core__Core_time_float.t -> Core__.Import.Time.Span.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.to_span_since_epoch
    positional_1
  |> python_of_core____import__time__span__t
;;

let of_span_since_epoch () = (* Core__.Import.Time.Span.t -> Core__Core_time_float.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core____import__time__span__t ~docstring:"Core__.Import.Time.Span.t"
  in
  Core__Core_time_float.of_span_since_epoch
    positional_1
  |> python_of_core__core_time_float__t
;;

module Date_and_ofday = struct
  let of_date_ofday () = (* Core_kernel__.Date0.t -> Core__.Import.Time.Ofday.t -> Core__Core_time_float.Date_and_ofday.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core_kernel____date0__t ~docstring:"Core_kernel__.Date0.t" and
      positional_2 = positional "positional_2" param_core____import__time__ofday__t ~docstring:"Core__.Import.Time.Ofday.t"
    in
    Core__Core_time_float.Date_and_ofday.of_date_ofday
      positional_1
      positional_2
    |> python_of_core__core_time_float__date_and_ofday__t
  ;;

  let to_date_ofday () = (* Core__Core_time_float.Date_and_ofday.t -> (Core_kernel__.Date0.t, Core__.Import.Time.Ofday.t) *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__date_and_ofday__t ~docstring:"Core__Core_time_float.Date_and_ofday.t"
    in
    Core__Core_time_float.Date_and_ofday.to_date_ofday
      positional_1
    |> (fun (t0, t1) -> Py.Tuple.of_list [python_of_core_kernel____date0__t t0; python_of_core____import__time__ofday__t t1])
  ;;

  let to_date () = (* Core__Core_time_float.Date_and_ofday.t -> Core_kernel__.Date0.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__date_and_ofday__t ~docstring:"Core__Core_time_float.Date_and_ofday.t"
    in
    Core__Core_time_float.Date_and_ofday.to_date
      positional_1
    |> python_of_core_kernel____date0__t
  ;;

  let to_ofday () = (* Core__Core_time_float.Date_and_ofday.t -> Core__.Import.Time.Ofday.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__date_and_ofday__t ~docstring:"Core__Core_time_float.Date_and_ofday.t"
    in
    Core__Core_time_float.Date_and_ofday.to_ofday
      positional_1
    |> python_of_core____import__time__ofday__t
  ;;

  let of_absolute () = (* Core__.Import.Time.t -> Core__.Import.Time.Span.t -> Core__Core_time_float.Date_and_ofday.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core____import__time__t ~docstring:"Core__.Import.Time.t" and
      offset_from_utc = keyword "offset_from_utc" param_core____import__time__span__t ~docstring:"Core__.Import.Time.Span.t"
    in
    Core__Core_time_float.Date_and_ofday.of_absolute
      positional_1
      ~offset_from_utc
    |> python_of_core__core_time_float__date_and_ofday__t
  ;;

  let to_absolute () = (* Core__Core_time_float.Date_and_ofday.t -> Core__.Import.Time.Span.t -> Core__.Import.Time.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__date_and_ofday__t ~docstring:"Core__Core_time_float.Date_and_ofday.t" and
      offset_from_utc = keyword "offset_from_utc" param_core____import__time__span__t ~docstring:"Core__.Import.Time.Span.t"
    in
    Core__Core_time_float.Date_and_ofday.to_absolute
      positional_1
      ~offset_from_utc
    |> python_of_core____import__time__t
  ;;

  let of_synthetic_span_since_epoch () = (* Core__.Import.Time.Span.t -> Core__Core_time_float.Date_and_ofday.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core____import__time__span__t ~docstring:"Core__.Import.Time.Span.t"
    in
    Core__Core_time_float.Date_and_ofday.of_synthetic_span_since_epoch
      positional_1
    |> python_of_core__core_time_float__date_and_ofday__t
  ;;

  let to_synthetic_span_since_epoch () = (* Core__Core_time_float.Date_and_ofday.t -> Core__.Import.Time.Span.t *)
    let%map_open
      positional_1 = positional "positional_1" param_core__core_time_float__date_and_ofday__t ~docstring:"Core__Core_time_float.Date_and_ofday.t"
    in
    Core__Core_time_float.Date_and_ofday.to_synthetic_span_since_epoch
      positional_1
    |> python_of_core____import__time__span__t
  ;;


  let register_module ~module_name =
    let modl = Py_module.create module_name in
    Py_module.set modl "of_date_ofday" (of_date_ofday ());
    Py_module.set modl "to_date_ofday" (to_date_ofday ());
    Py_module.set modl "to_date" (to_date ());
    Py_module.set modl "to_ofday" (to_ofday ());
    Py_module.set modl "of_absolute" (of_absolute ());
    Py_module.set modl "to_absolute" (to_absolute ());
    Py_module.set modl "of_synthetic_span_since_epoch" (of_synthetic_span_since_epoch ());
    Py_module.set modl "to_synthetic_span_since_epoch" (to_synthetic_span_since_epoch ());
    modl
end;;
let now () = (* unit -> Core__Core_time_float.t *)
  Defunc.no_arg (fun () -> Core__Core_time_float.now () |> python_of_core__core_time_float__t)
;;

let add () = (* Core__Core_time_float.t -> Core__.Import.Time.Span.t -> Core__Core_time_float.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    positional_2 = positional "positional_2" param_core____import__time__span__t ~docstring:"Core__.Import.Time.Span.t"
  in
  Core__Core_time_float.add
    positional_1
    positional_2
  |> python_of_core__core_time_float__t
;;

let sub () = (* Core__Core_time_float.t -> Core__.Import.Time.Span.t -> Core__Core_time_float.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    positional_2 = positional "positional_2" param_core____import__time__span__t ~docstring:"Core__.Import.Time.Span.t"
  in
  Core__Core_time_float.sub
    positional_1
    positional_2
  |> python_of_core__core_time_float__t
;;

let diff () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> Core__.Import.Time.Span.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    positional_2 = positional "positional_2" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.diff
    positional_1
    positional_2
  |> python_of_core____import__time__span__t
;;

let abs_diff () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> Core__.Import.Time.Span.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    positional_2 = positional "positional_2" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.abs_diff
    positional_1
    positional_2
  |> python_of_core____import__time__span__t
;;

let is_earlier () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    than = keyword "than" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.is_earlier
    positional_1
    ~than
  |> python_of_bool
;;

let is_later () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    than = keyword "than" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.is_later
    positional_1
    ~than
  |> python_of_bool
;;

let of_date_ofday () = (* Core_kernel__.Zone.t -> Core_kernel__.Time_intf.Date.t -> Core__.Import.Time.Ofday.t -> Core__Core_time_float.t *)
  let%map_open
    zone = keyword "zone" param_core_kernel____zone__t ~docstring:"Core_kernel__.Zone.t" and
    positional_2 = positional "positional_2" param_core_kernel____time_intf__date__t ~docstring:"Core_kernel__.Time_intf.Date.t" and
    positional_3 = positional "positional_3" param_core____import__time__ofday__t ~docstring:"Core__.Import.Time.Ofday.t"
  in
  Core__Core_time_float.of_date_ofday
    ~zone
    positional_2
    positional_3
  |> python_of_core__core_time_float__t
;;

let to_date_ofday () = (* Core__Core_time_float.t -> Core_kernel__.Zone.t -> (Core_kernel__.Time_intf.Date.t, Core__.Import.Time.Ofday.t) *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    zone = keyword "zone" param_core_kernel____zone__t ~docstring:"Core_kernel__.Zone.t"
  in
  Core__Core_time_float.to_date_ofday
    positional_1
    ~zone
  |> (fun (t0, t1) -> Py.Tuple.of_list [python_of_core_kernel____time_intf__date__t t0; python_of_core____import__time__ofday__t t1])
;;

let to_date () = (* Core__Core_time_float.t -> Core_kernel__.Zone.t -> Core_kernel__.Time_intf.Date.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    zone = keyword "zone" param_core_kernel____zone__t ~docstring:"Core_kernel__.Zone.t"
  in
  Core__Core_time_float.to_date
    positional_1
    ~zone
  |> python_of_core_kernel____time_intf__date__t
;;

let to_ofday () = (* Core__Core_time_float.t -> Core_kernel__.Zone.t -> Core__.Import.Time.Ofday.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    zone = keyword "zone" param_core_kernel____zone__t ~docstring:"Core_kernel__.Zone.t"
  in
  Core__Core_time_float.to_ofday
    positional_1
    ~zone
  |> python_of_core____import__time__ofday__t
;;

let reset_date_cache () = (* unit -> unit *)
  Defunc.no_arg (fun () -> Core__Core_time_float.reset_date_cache () |> python_of_unit)
;;

let epoch () = (* Core__Core_time_float.t *)
  Defunc.no_arg (fun () -> Core__Core_time_float.epoch |> python_of_core__core_time_float__t)
;;

let convert () = (* Core_kernel__.Zone.t -> Core_kernel__.Zone.t -> Core_kernel__.Time_intf.Date.t -> Core__.Import.Time.Ofday.t -> (Core_kernel__.Time_intf.Date.t, Core__.Import.Time.Ofday.t) *)
  let%map_open
    from_tz = keyword "from_tz" param_core_kernel____zone__t ~docstring:"Core_kernel__.Zone.t" and
    to_tz = keyword "to_tz" param_core_kernel____zone__t ~docstring:"Core_kernel__.Zone.t" and
    positional_3 = positional "positional_3" param_core_kernel____time_intf__date__t ~docstring:"Core_kernel__.Time_intf.Date.t" and
    positional_4 = positional "positional_4" param_core____import__time__ofday__t ~docstring:"Core__.Import.Time.Ofday.t"
  in
  Core__Core_time_float.convert
    ~from_tz
    ~to_tz
    positional_3
    positional_4
  |> (fun (t0, t1) -> Py.Tuple.of_list [python_of_core_kernel____time_intf__date__t t0; python_of_core____import__time__ofday__t t1])
;;

let utc_offset () = (* Core__Core_time_float.t -> Core_kernel__.Zone.t -> Core__.Import.Time.Span.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    zone = keyword "zone" param_core_kernel____zone__t ~docstring:"Core_kernel__.Zone.t"
  in
  Core__Core_time_float.utc_offset
    positional_1
    ~zone
  |> python_of_core____import__time__span__t
;;

let to_filename_string () = (* Core__Core_time_float.t -> Core_kernel__.Zone.t -> string *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    zone = keyword "zone" param_core_kernel____zone__t ~docstring:"Core_kernel__.Zone.t"
  in
  Core__Core_time_float.to_filename_string
    positional_1
    ~zone
  |> python_of_string
;;

let of_filename_string () = (* string -> Core_kernel__.Zone.t -> Core__Core_time_float.t *)
  let%map_open
    positional_1 = positional "positional_1" string ~docstring:"string" and
    zone = keyword "zone" param_core_kernel____zone__t ~docstring:"Core_kernel__.Zone.t"
  in
  Core__Core_time_float.of_filename_string
    positional_1
    ~zone
  |> python_of_core__core_time_float__t
;;

let to_string_abs () = (* Core__Core_time_float.t -> Core_kernel__.Zone.t -> string *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    zone = keyword "zone" param_core_kernel____zone__t ~docstring:"Core_kernel__.Zone.t"
  in
  Core__Core_time_float.to_string_abs
    positional_1
    ~zone
  |> python_of_string
;;

let to_string_abs_trimmed () = (* Core__Core_time_float.t -> Core_kernel__.Zone.t -> string *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    zone = keyword "zone" param_core_kernel____zone__t ~docstring:"Core_kernel__.Zone.t"
  in
  Core__Core_time_float.to_string_abs_trimmed
    positional_1
    ~zone
  |> python_of_string
;;

let to_string_abs_parts () = (* Core__Core_time_float.t -> Core_kernel__.Zone.t -> string list *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    zone = keyword "zone" param_core_kernel____zone__t ~docstring:"Core_kernel__.Zone.t"
  in
  Core__Core_time_float.to_string_abs_parts
    positional_1
    ~zone
  |> (python_of_list python_of_string)
;;

let to_string_trimmed () = (* Core__Core_time_float.t -> Core_kernel__.Zone.t -> string *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    zone = keyword "zone" param_core_kernel____zone__t ~docstring:"Core_kernel__.Zone.t"
  in
  Core__Core_time_float.to_string_trimmed
    positional_1
    ~zone
  |> python_of_string
;;

let to_sec_string () = (* Core__Core_time_float.t -> Core_kernel__.Zone.t -> string *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    zone = keyword "zone" param_core_kernel____zone__t ~docstring:"Core_kernel__.Zone.t"
  in
  Core__Core_time_float.to_sec_string
    positional_1
    ~zone
  |> python_of_string
;;

let of_localized_string () = (* Core_kernel__.Zone.t -> string -> Core__Core_time_float.t *)
  let%map_open
    zone = keyword "zone" param_core_kernel____zone__t ~docstring:"Core_kernel__.Zone.t" and
    positional_2 = positional "positional_2" string ~docstring:"string"
  in
  Core__Core_time_float.of_localized_string
    ~zone
    positional_2
  |> python_of_core__core_time_float__t
;;

let to_string_iso8601_basic () = (* Core__Core_time_float.t -> Core_kernel__.Zone.t -> string *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    zone = keyword "zone" param_core_kernel____zone__t ~docstring:"Core_kernel__.Zone.t"
  in
  Core__Core_time_float.to_string_iso8601_basic
    positional_1
    ~zone
  |> python_of_string
;;

let bin_shape_t () = (* Bin_prot.Shape.t *)
  Defunc.no_arg (fun () -> Core__Core_time_float.bin_shape_t |> python_of_bin_prot__shape__t)
;;

let t_of_sexp () = (* Sexplib0.Sexp.t -> Core__Core_time_float.t *)
  let%map_open
    positional_1 = positional "positional_1" param_sexplib0__sexp__t ~docstring:"Sexplib0.Sexp.t"
  in
  Core__Core_time_float.t_of_sexp
    positional_1
  |> python_of_core__core_time_float__t
;;

let sexp_of_t () = (* Core__Core_time_float.t -> Ppx_sexp_conv_lib.Sexp.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.sexp_of_t
    positional_1
  |> python_of_ppx_sexp_conv_lib__sexp__t
;;

let of_string () = (* string -> Core__Core_time_float.t *)
  let%map_open
    positional_1 = positional "positional_1" string ~docstring:"string"
  in
  Core__Core_time_float.of_string
    positional_1
  |> python_of_core__core_time_float__t
;;

let to_string () = (* Core__Core_time_float.t -> string *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.to_string
    positional_1
  |> python_of_string
;;

let pp () = (* Base__.Formatter.t -> Core__Core_time_float.t -> unit *)
  let%map_open
    positional_1 = positional "positional_1" param_base____formatter__t ~docstring:"Base__.Formatter.t" and
    positional_2 = positional "positional_2" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.pp
    positional_1
    positional_2
  |> python_of_unit
;;

let greatereq () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    positional_2 = positional "positional_2" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.(>=)
    positional_1
    positional_2
  |> python_of_bool
;;

let lowereq () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    positional_2 = positional "positional_2" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.(<=)
    positional_1
    positional_2
  |> python_of_bool
;;

let eq () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    positional_2 = positional "positional_2" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.(=)
    positional_1
    positional_2
  |> python_of_bool
;;

let greater () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    positional_2 = positional "positional_2" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.(>)
    positional_1
    positional_2
  |> python_of_bool
;;

let lower () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    positional_2 = positional "positional_2" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.(<)
    positional_1
    positional_2
  |> python_of_bool
;;

let neq () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    positional_2 = positional "positional_2" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.(<>)
    positional_1
    positional_2
  |> python_of_bool
;;

let equal () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    positional_2 = positional "positional_2" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.equal
    positional_1
    positional_2
  |> python_of_bool
;;

let compare () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> int *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    positional_2 = positional "positional_2" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.compare
    positional_1
    positional_2
  |> python_of_int
;;

let min () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> Core__Core_time_float.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    positional_2 = positional "positional_2" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.min
    positional_1
    positional_2
  |> python_of_core__core_time_float__t
;;

let max () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> Core__Core_time_float.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    positional_2 = positional "positional_2" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.max
    positional_1
    positional_2
  |> python_of_core__core_time_float__t
;;

let ascending () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> int *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    positional_2 = positional "positional_2" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.ascending
    positional_1
    positional_2
  |> python_of_int
;;

let descending () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> int *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    positional_2 = positional "positional_2" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.descending
    positional_1
    positional_2
  |> python_of_int
;;

let between () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> Core__Core_time_float.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    low = keyword "low" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    high = keyword "high" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.between
    positional_1
    ~low
    ~high
  |> python_of_bool
;;

let clamp_exn () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> Core__Core_time_float.t -> Core__Core_time_float.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    min = keyword "min" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    max = keyword "max" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.clamp_exn
    positional_1
    ~min
    ~max
  |> python_of_core__core_time_float__t
;;

let hash_fold_t () = (* Ppx_hash_lib.Std.Hash.state -> Core__Core_time_float.t -> Ppx_hash_lib.Std.Hash.state *)
  let%map_open
    positional_1 = positional "positional_1" param_ppx_hash_lib__std__hash__state ~docstring:"Ppx_hash_lib.Std.Hash.state" and
    positional_2 = positional "positional_2" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.hash_fold_t
    positional_1
    positional_2
  |> python_of_ppx_hash_lib__std__hash__state
;;

let hash () = (* Core__Core_time_float.t -> Ppx_hash_lib.Std.Hash.hash_value *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.hash
    positional_1
  |> python_of_ppx_hash_lib__std__hash__hash_value
;;

let get_sexp_zone () = (* unit -> Core__Core_time_float.Zone.t *)
  Defunc.no_arg (fun () -> Core__Core_time_float.get_sexp_zone () |> python_of_core__core_time_float__zone__t)
;;

let set_sexp_zone () = (* Core__Core_time_float.Zone.t -> unit *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
  in
  Core__Core_time_float.set_sexp_zone
    positional_1
  |> python_of_unit
;;

let greatereq_approx () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    positional_2 = positional "positional_2" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.(>=.)
    positional_1
    positional_2
  |> python_of_bool
;;

let lowereq_approx () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    positional_2 = positional "positional_2" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.(<=.)
    positional_1
    positional_2
  |> python_of_bool
;;

let eq_approx () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    positional_2 = positional "positional_2" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.(=.)
    positional_1
    positional_2
  |> python_of_bool
;;

let greater_approx () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    positional_2 = positional "positional_2" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.(>.)
    positional_1
    positional_2
  |> python_of_bool
;;

let lower_approx () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    positional_2 = positional "positional_2" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.(<.)
    positional_1
    positional_2
  |> python_of_bool
;;

let neq_approx () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> bool *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    positional_2 = positional "positional_2" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.(<>.)
    positional_1
    positional_2
  |> python_of_bool
;;

let robustly_compare () = (* Core__Core_time_float.t -> Core__Core_time_float.t -> int *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    positional_2 = positional "positional_2" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t"
  in
  Core__Core_time_float.robustly_compare
    positional_1
    positional_2
  |> python_of_int
;;

let of_tm () = (* Core__.Core_unix.tm -> Core__Core_time_float.Zone.t -> Core__Core_time_float.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core____core_unix__tm ~docstring:"Core__.Core_unix.tm" and
    zone = keyword "zone" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
  in
  Core__Core_time_float.of_tm
    positional_1
    ~zone
  |> python_of_core__core_time_float__t
;;

let of_string_abs () = (* string -> Core__Core_time_float.t *)
  let%map_open
    positional_1 = positional "positional_1" string ~docstring:"string"
  in
  Core__Core_time_float.of_string_abs
    positional_1
  |> python_of_core__core_time_float__t
;;

let t_of_sexp_abs () = (* Core__.Import.Sexp.t -> Core__Core_time_float.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core____import__sexp__t ~docstring:"Core__.Import.Sexp.t"
  in
  Core__Core_time_float.t_of_sexp_abs
    positional_1
  |> python_of_core__core_time_float__t
;;

let sexp_of_t_abs () = (* Core__Core_time_float.t -> Core__Core_time_float.Zone.t -> Core__.Import.Sexp.t *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    zone = keyword "zone" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
  in
  Core__Core_time_float.sexp_of_t_abs
    positional_1
    ~zone
  |> python_of_core____import__sexp__t
;;

let pause () = (* Core__Core_time_float.Span.t -> unit *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__span__t ~docstring:"Core__Core_time_float.Span.t"
  in
  Core__Core_time_float.pause
    positional_1
  |> python_of_unit
;;

let pause_forever () = (* unit -> Core__.Import.never_returns *)
  Defunc.no_arg (fun () -> Core__Core_time_float.pause_forever () |> python_of_core____import__never_returns)
;;

let format () = (* Core__Core_time_float.t -> string -> Core__Core_time_float.Zone.t -> string *)
  let%map_open
    positional_1 = positional "positional_1" param_core__core_time_float__t ~docstring:"Core__Core_time_float.t" and
    positional_2 = positional "positional_2" string ~docstring:"string" and
    zone = keyword "zone" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
  in
  Core__Core_time_float.format
    positional_1
    positional_2
    ~zone
  |> python_of_string
;;

let parse () = (* string -> string -> Core__Core_time_float.Zone.t -> Core__Core_time_float.t *)
  let%map_open
    positional_1 = positional "positional_1" string ~docstring:"string" and
    fmt = keyword "fmt" string ~docstring:"string" and
    zone = keyword "zone" param_core__core_time_float__zone__t ~docstring:"Core__Core_time_float.Zone.t"
  in
  Core__Core_time_float.parse
    positional_1
    ~fmt
    ~zone
  |> python_of_core__core_time_float__t
;;

module Exposed_for_tests = struct
  let ensure_colon_in_offset () = (* string -> string *)
    let%map_open
      positional_1 = positional "positional_1" string ~docstring:"string"
    in
    Core__Core_time_float.Exposed_for_tests.ensure_colon_in_offset
      positional_1
    |> python_of_string
  ;;


  let register_module ~module_name =
    let modl = Py_module.create module_name in
    Py_module.set modl "ensure_colon_in_offset" (ensure_colon_in_offset ());
    modl
end;;

let register_module ~module_name =
  let modl = Py_module.create module_name in
  let subm = Span.register_module ~module_name:"core__core_time_float__span__span" in
  Py_module.set_value modl "span" (Py_module.to_pyobject subm);
  let subm = Zone.register_module ~module_name:"core__core_time_float__zone__zone" in
  Py_module.set_value modl "zone" (Py_module.to_pyobject subm);
  let subm = Ofday.register_module ~module_name:"core__core_time_float__ofday__ofday" in
  Py_module.set_value modl "ofday" (Py_module.to_pyobject subm);
  Py_module.set modl "next" (next ());
  Py_module.set modl "prev" (prev ());
  Py_module.set modl "to_span_since_epoch" (to_span_since_epoch ());
  Py_module.set modl "of_span_since_epoch" (of_span_since_epoch ());
  let subm = Date_and_ofday.register_module ~module_name:"core__core_time_float__date_and_ofday__date_and_ofday" in
  Py_module.set_value modl "date_and_ofday" (Py_module.to_pyobject subm);
  Py_module.set modl "now" (now ());
  Py_module.set modl "add" (add ());
  Py_module.set modl "sub" (sub ());
  Py_module.set modl "diff" (diff ());
  Py_module.set modl "abs_diff" (abs_diff ());
  Py_module.set modl "is_earlier" (is_earlier ());
  Py_module.set modl "is_later" (is_later ());
  Py_module.set modl "of_date_ofday" (of_date_ofday ());
  Py_module.set modl "to_date_ofday" (to_date_ofday ());
  Py_module.set modl "to_date" (to_date ());
  Py_module.set modl "to_ofday" (to_ofday ());
  Py_module.set modl "reset_date_cache" (reset_date_cache ());
  Py_module.set modl "epoch" (epoch ());
  Py_module.set modl "convert" (convert ());
  Py_module.set modl "utc_offset" (utc_offset ());
  Py_module.set modl "to_filename_string" (to_filename_string ());
  Py_module.set modl "of_filename_string" (of_filename_string ());
  Py_module.set modl "to_string_abs" (to_string_abs ());
  Py_module.set modl "to_string_abs_trimmed" (to_string_abs_trimmed ());
  Py_module.set modl "to_string_abs_parts" (to_string_abs_parts ());
  Py_module.set modl "to_string_trimmed" (to_string_trimmed ());
  Py_module.set modl "to_sec_string" (to_sec_string ());
  Py_module.set modl "of_localized_string" (of_localized_string ());
  Py_module.set modl "to_string_iso8601_basic" (to_string_iso8601_basic ());
  Py_module.set modl "bin_shape_t" (bin_shape_t ());
  Py_module.set modl "t_of_sexp" (t_of_sexp ());
  Py_module.set modl "sexp_of_t" (sexp_of_t ());
  Py_module.set modl "of_string" (of_string ());
  Py_module.set modl "to_string" (to_string ());
  Py_module.set modl "pp" (pp ());
  Py_module.set modl "greatereq" (greatereq ());
  Py_module.set modl "lowereq" (lowereq ());
  Py_module.set modl "eq" (eq ());
  Py_module.set modl "greater" (greater ());
  Py_module.set modl "lower" (lower ());
  Py_module.set modl "neq" (neq ());
  Py_module.set modl "equal" (equal ());
  Py_module.set modl "compare" (compare ());
  Py_module.set modl "min" (min ());
  Py_module.set modl "max" (max ());
  Py_module.set modl "ascending" (ascending ());
  Py_module.set modl "descending" (descending ());
  Py_module.set modl "between" (between ());
  Py_module.set modl "clamp_exn" (clamp_exn ());
  Py_module.set modl "hash_fold_t" (hash_fold_t ());
  Py_module.set modl "hash" (hash ());
  Py_module.set modl "get_sexp_zone" (get_sexp_zone ());
  Py_module.set modl "set_sexp_zone" (set_sexp_zone ());
  Py_module.set modl "greatereq_approx" (greatereq_approx ());
  Py_module.set modl "lowereq_approx" (lowereq_approx ());
  Py_module.set modl "eq_approx" (eq_approx ());
  Py_module.set modl "greater_approx" (greater_approx ());
  Py_module.set modl "lower_approx" (lower_approx ());
  Py_module.set modl "neq_approx" (neq_approx ());
  Py_module.set modl "robustly_compare" (robustly_compare ());
  Py_module.set modl "of_tm" (of_tm ());
  Py_module.set modl "of_string_abs" (of_string_abs ());
  Py_module.set modl "t_of_sexp_abs" (t_of_sexp_abs ());
  Py_module.set modl "sexp_of_t_abs" (sexp_of_t_abs ());
  Py_module.set modl "pause" (pause ());
  Py_module.set modl "pause_forever" (pause_forever ());
  Py_module.set modl "format" (format ());
  Py_module.set modl "parse" (parse ());
  let subm = Exposed_for_tests.register_module ~module_name:"core__core_time_float__exposed_for_tests__exposed_for_tests" in
  Py_module.set_value modl "exposed_for_tests" (Py_module.to_pyobject subm);
  modl
