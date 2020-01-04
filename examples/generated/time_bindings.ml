(* THIS CODE IS GENERATED AUTOMATICALLY, DO NOT EDIT BY HAND *)
open! Base
open! Python_lib
open! Python_lib.Let_syntax
open! Gen_import

let protect ~f x =
  try f x with
  | Py.Err _ as err -> raise err
  | exn -> raise (Py.Err (SyntaxError, Exn.to_string exn))

module Span = struct
  type underlying = Core__Core_time_float.Span.underlying

  let python_of_underlying, underlying_of_python =
    let capsule = lazy (Py.Capsule.make "Core__Core_time_float.Span.underlying") in
    (fun x -> (Lazy.force capsule |> fst) x), fun x -> (Lazy.force capsule |> snd) x

  let param_underlying =
    Defunc.Of_python.create
      ~type_name:"Core__Core_time_float.Span.underlying"
      ~conv:underlying_of_python

  type t = Core__Core_time_float.Span.t

  let python_of_t, t_of_python =
    let capsule = lazy (Py.Capsule.make "Core__Core_time_float.Span.t") in
    (fun x -> (Lazy.force capsule |> fst) x), fun x -> (Lazy.force capsule |> snd) x

  let param_t =
    Defunc.Of_python.create ~type_name:"Core__Core_time_float.Span.t" ~conv:t_of_python

  let bin_shape_t () =
    (* Bin_prot.Shape.t *)
    Defunc.no_arg (fun () ->
        Core__Core_time_float.Span.bin_shape_t |> python_of_bin_prot_shape_t)

  let t_of_sexp () =
    (* Sexplib0.Sexp.t -> t *)
    let%map_open positional_1 =
      positional "positional_1" param_sexplib0_sexp_t ~docstring:"Sexplib0.Sexp.t"
    in
    Core__Core_time_float.Span.t_of_sexp positional_1 |> python_of_t

  let sexp_of_t () =
    (* t -> Sexplib0.Sexp.t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Span.sexp_of_t positional_1 |> python_of_sexplib0_sexp_t

  module Parts = struct
    type t = Core__Core_time_float.Span.Parts.t

    let python_of_t, t_of_python =
      let capsule = lazy (Py.Capsule.make "Core__Core_time_float.Span.Parts.t") in
      (fun x -> (Lazy.force capsule |> fst) x), fun x -> (Lazy.force capsule |> snd) x

    let param_t =
      Defunc.Of_python.create
        ~type_name:"Core__Core_time_float.Span.Parts.t"
        ~conv:t_of_python

    let compare () =
      (* t -> t -> int *)
      let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
      and positional_2 = positional "positional_2" param_t ~docstring:"t" in
      Core__Core_time_float.Span.Parts.compare positional_1 positional_2 |> python_of_int

    let t_of_sexp () =
      (* Sexplib0.Sexp.t -> t *)
      let%map_open positional_1 =
        positional "positional_1" param_sexplib0_sexp_t ~docstring:"Sexplib0.Sexp.t"
      in
      Core__Core_time_float.Span.Parts.t_of_sexp positional_1 |> python_of_t

    let sexp_of_t () =
      (* t -> Sexplib0.Sexp.t *)
      let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
      Core__Core_time_float.Span.Parts.sexp_of_t positional_1 |> python_of_sexplib0_sexp_t

    let register_module ~module_name =
      let modl = Py_module.create module_name in
      Py_module.set modl "compare" (compare ());
      Py_module.set modl "t_of_sexp" (t_of_sexp ());
      Py_module.set modl "sexp_of_t" (sexp_of_t ());
      modl
  end

  let greatereq () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Span.( >= ) positional_1 positional_2 |> python_of_bool

  let lowereq () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Span.( <= ) positional_1 positional_2 |> python_of_bool

  let eq () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Span.( = ) positional_1 positional_2 |> python_of_bool

  let greater () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Span.( > ) positional_1 positional_2 |> python_of_bool

  let lower () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Span.( < ) positional_1 positional_2 |> python_of_bool

  let neq () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Span.( <> ) positional_1 positional_2 |> python_of_bool

  let equal () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Span.equal positional_1 positional_2 |> python_of_bool

  let compare () =
    (* t -> t -> int *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Span.compare positional_1 positional_2 |> python_of_int

  let min () =
    (* t -> t -> t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Span.min positional_1 positional_2 |> python_of_t

  let max () =
    (* t -> t -> t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Span.max positional_1 positional_2 |> python_of_t

  let ascending () =
    (* t -> t -> int *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Span.ascending positional_1 positional_2 |> python_of_int

  let descending () =
    (* t -> t -> int *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Span.descending positional_1 positional_2 |> python_of_int

  let between () =
    (* t -> t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and low = keyword "low" param_t ~docstring:"t"
    and high = keyword "high" param_t ~docstring:"t" in
    Core__Core_time_float.Span.between positional_1 ~low ~high |> python_of_bool

  let clamp_exn () =
    (* t -> t -> t -> t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and min = keyword "min" param_t ~docstring:"t"
    and max = keyword "max" param_t ~docstring:"t" in
    Core__Core_time_float.Span.clamp_exn positional_1 ~min ~max |> python_of_t

  let is_positive () =
    (* t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Span.is_positive positional_1 |> python_of_bool

  let is_non_negative () =
    (* t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Span.is_non_negative positional_1 |> python_of_bool

  let is_negative () =
    (* t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Span.is_negative positional_1 |> python_of_bool

  let is_non_positive () =
    (* t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Span.is_non_positive positional_1 |> python_of_bool

  let sign () =
    (* t -> Base__.Sign0.t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Span.sign positional_1 |> python_of_base___sign0_t

  let hash_fold_t () =
    (* Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state *)
    let%map_open positional_1 =
      positional
        "positional_1"
        param_ppx_hash_lib_std_hash_state
        ~docstring:"Ppx_hash_lib.Std.Hash.state"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Span.hash_fold_t positional_1 positional_2
    |> python_of_ppx_hash_lib_std_hash_state

  let hash () =
    (* t -> Ppx_hash_lib.Std.Hash.hash_value *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Span.hash positional_1
    |> python_of_ppx_hash_lib_std_hash_hash_value

  let pp () =
    (* Base__.Formatter.t -> t -> unit *)
    let%map_open positional_1 =
      positional "positional_1" param_base___formatter_t ~docstring:"Base__.Formatter.t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Span.pp positional_1 positional_2 |> python_of_unit

  let greatereq_approx () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Span.( >=. ) positional_1 positional_2 |> python_of_bool

  let lowereq_approx () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Span.( <=. ) positional_1 positional_2 |> python_of_bool

  let eq_approx () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Span.( =. ) positional_1 positional_2 |> python_of_bool

  let greater_approx () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Span.( >. ) positional_1 positional_2 |> python_of_bool

  let lower_approx () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Span.( <. ) positional_1 positional_2 |> python_of_bool

  let neq_approx () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Span.( <>. ) positional_1 positional_2 |> python_of_bool

  let robustly_compare () =
    (* t -> t -> int *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Span.robustly_compare positional_1 positional_2 |> python_of_int

  let to_string () =
    (* t -> string *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Span.to_string positional_1 |> python_of_string

  let of_string () =
    (* string -> t *)
    let%map_open positional_1 = positional "positional_1" string ~docstring:"string" in
    Core__Core_time_float.Span.of_string positional_1 |> python_of_t

  let nanosecond () =
    (* t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Span.nanosecond |> python_of_t)

  let microsecond () =
    (* t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Span.microsecond |> python_of_t)

  let millisecond () =
    (* t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Span.millisecond |> python_of_t)

  let second () =
    (* t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Span.second |> python_of_t)

  let minute () =
    (* t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Span.minute |> python_of_t)

  let hour () =
    (* t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Span.hour |> python_of_t)

  let day () =
    (* t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Span.day |> python_of_t)

  let robust_comparison_tolerance () =
    (* t *)
    Defunc.no_arg (fun () ->
        Core__Core_time_float.Span.robust_comparison_tolerance |> python_of_t)

  let zero () =
    (* t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Span.zero |> python_of_t)

  let to_parts () =
    (* t -> Parts.t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Span.to_parts positional_1 |> python_of_parts_t

  let of_ns () =
    (* float -> t *)
    let%map_open positional_1 = positional "positional_1" float ~docstring:"float" in
    Core__Core_time_float.Span.of_ns positional_1 |> python_of_t

  let of_us () =
    (* float -> t *)
    let%map_open positional_1 = positional "positional_1" float ~docstring:"float" in
    Core__Core_time_float.Span.of_us positional_1 |> python_of_t

  let of_ms () =
    (* float -> t *)
    let%map_open positional_1 = positional "positional_1" float ~docstring:"float" in
    Core__Core_time_float.Span.of_ms positional_1 |> python_of_t

  let of_sec () =
    (* float -> t *)
    let%map_open positional_1 = positional "positional_1" float ~docstring:"float" in
    Core__Core_time_float.Span.of_sec positional_1 |> python_of_t

  let of_int_sec () =
    (* int -> t *)
    let%map_open positional_1 = positional "positional_1" int ~docstring:"int" in
    Core__Core_time_float.Span.of_int_sec positional_1 |> python_of_t

  let of_int32_seconds () =
    (* Core_kernel__.Int32.t -> t *)
    let%map_open positional_1 =
      positional
        "positional_1"
        param_core_kernel___int32_t
        ~docstring:"Core_kernel__.Int32.t"
    in
    Core__Core_time_float.Span.of_int32_seconds positional_1 |> python_of_t

  let of_int63_seconds () =
    (* Core_kernel__.Int63.t -> t *)
    let%map_open positional_1 =
      positional
        "positional_1"
        param_core_kernel___int63_t
        ~docstring:"Core_kernel__.Int63.t"
    in
    Core__Core_time_float.Span.of_int63_seconds positional_1 |> python_of_t

  let of_min () =
    (* float -> t *)
    let%map_open positional_1 = positional "positional_1" float ~docstring:"float" in
    Core__Core_time_float.Span.of_min positional_1 |> python_of_t

  let of_hr () =
    (* float -> t *)
    let%map_open positional_1 = positional "positional_1" float ~docstring:"float" in
    Core__Core_time_float.Span.of_hr positional_1 |> python_of_t

  let of_day () =
    (* float -> t *)
    let%map_open positional_1 = positional "positional_1" float ~docstring:"float" in
    Core__Core_time_float.Span.of_day positional_1 |> python_of_t

  let to_ns () =
    (* t -> float *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Span.to_ns positional_1 |> python_of_float

  let to_us () =
    (* t -> float *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Span.to_us positional_1 |> python_of_float

  let to_ms () =
    (* t -> float *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Span.to_ms positional_1 |> python_of_float

  let to_sec () =
    (* t -> float *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Span.to_sec positional_1 |> python_of_float

  let to_min () =
    (* t -> float *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Span.to_min positional_1 |> python_of_float

  let to_hr () =
    (* t -> float *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Span.to_hr positional_1 |> python_of_float

  let to_day () =
    (* t -> float *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Span.to_day positional_1 |> python_of_float

  let to_int63_seconds_round_down_exn () =
    (* t -> Core_kernel__.Int63.t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Span.to_int63_seconds_round_down_exn positional_1
    |> python_of_core_kernel___int63_t

  let to_proportional_float () =
    (* t -> float *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Span.to_proportional_float positional_1 |> python_of_float

  let plus () =
    (* t -> t -> t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Span.( + ) positional_1 positional_2 |> python_of_t

  let minus () =
    (* t -> t -> t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Span.( - ) positional_1 positional_2 |> python_of_t

  let abs () =
    (* t -> t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Span.abs positional_1 |> python_of_t

  let neg () =
    (* t -> t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Span.neg positional_1 |> python_of_t

  let scale () =
    (* t -> float -> t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" float ~docstring:"float" in
    Core__Core_time_float.Span.scale positional_1 positional_2 |> python_of_t

  let divide () =
    (* t -> float -> t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" float ~docstring:"float" in
    Core__Core_time_float.Span.( / ) positional_1 positional_2 |> python_of_t

  let divide_ () =
    (* t -> t -> float *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Span.( // ) positional_1 positional_2 |> python_of_float

  let next () =
    (* t -> t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Span.next positional_1 |> python_of_t

  let prev () =
    (* t -> t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Span.prev positional_1 |> python_of_t

  let to_short_string () =
    (* t -> string *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Span.to_short_string positional_1 |> python_of_string

  let to_unit_of_time () =
    (* t -> Core_kernel__.Unit_of_time.t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Span.to_unit_of_time positional_1
    |> python_of_core_kernel___unit_of_time_t

  let of_unit_of_time () =
    (* Core_kernel__.Unit_of_time.t -> t *)
    let%map_open positional_1 =
      positional
        "positional_1"
        param_core_kernel___unit_of_time_t
        ~docstring:"Core_kernel__.Unit_of_time.t"
    in
    Core__Core_time_float.Span.of_unit_of_time positional_1 |> python_of_t

  let randomize () =
    (* t -> Core_kernel__.Percent.t -> t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and percent =
      keyword "percent" param_core_kernel___percent_t ~docstring:"Core_kernel__.Percent.t"
    in
    Core__Core_time_float.Span.randomize positional_1 ~percent |> python_of_t

  let register_module ~module_name =
    let modl = Py_module.create module_name in
    Py_module.set modl "bin_shape_t" (bin_shape_t ());
    Py_module.set modl "t_of_sexp" (t_of_sexp ());
    Py_module.set modl "sexp_of_t" (sexp_of_t ());
    let subm = Parts.register_module ~module_name:"core__core_time_float__span__parts" in
    Py_module.set_value modl "parts" (Caml.Obj.magic subm);
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
    Py_module.set
      modl
      "to_int63_seconds_round_down_exn"
      (to_int63_seconds_round_down_exn ());
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
end

module Zone = struct
  type t = Core__Core_time_float.Zone.t

  let python_of_t, t_of_python =
    let capsule = lazy (Py.Capsule.make "Core__Core_time_float.Zone.t") in
    (fun x -> (Lazy.force capsule |> fst) x), fun x -> (Lazy.force capsule |> snd) x

  let param_t =
    Defunc.Of_python.create ~type_name:"Core__Core_time_float.Zone.t" ~conv:t_of_python

  let input_tz_file () =
    (* string -> string -> t *)
    let%map_open zonename = keyword "zonename" string ~docstring:"string"
    and filename = keyword "filename" string ~docstring:"string" in
    Core__Core_time_float.Zone.input_tz_file ~zonename ~filename |> python_of_t

  let of_utc_offset () =
    (* int -> t *)
    let%map_open hours = keyword "hours" int ~docstring:"int" in
    Core__Core_time_float.Zone.of_utc_offset ~hours |> python_of_t

  let utc () =
    (* t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Zone.utc |> python_of_t)

  let name () =
    (* t -> string *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Zone.name positional_1 |> python_of_string

  let reset_transition_cache () =
    (* t -> unit *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Zone.reset_transition_cache positional_1 |> python_of_unit

  module Index = struct
    type t = Core__Core_time_float.Zone.Index.t

    let python_of_t, t_of_python =
      let capsule = lazy (Py.Capsule.make "Core__Core_time_float.Zone.Index.t") in
      (fun x -> (Lazy.force capsule |> fst) x), fun x -> (Lazy.force capsule |> snd) x

    let param_t =
      Defunc.Of_python.create
        ~type_name:"Core__Core_time_float.Zone.Index.t"
        ~conv:t_of_python

    let next () =
      (* t -> t *)
      let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
      Core__Core_time_float.Zone.Index.next positional_1 |> python_of_t

    let prev () =
      (* t -> t *)
      let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
      Core__Core_time_float.Zone.Index.prev positional_1 |> python_of_t

    let register_module ~module_name =
      let modl = Py_module.create module_name in
      Py_module.set modl "next" (next ());
      Py_module.set modl "prev" (prev ());
      modl
  end

  let index () =
    (* t -> Core__.Import.Time.t -> Index.t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 =
      positional
        "positional_2"
        param_core___import_time_t
        ~docstring:"Core__.Import.Time.t"
    in
    Core__Core_time_float.Zone.index positional_1 positional_2 |> python_of_index_t

  let index_of_date_and_ofday () =
    (* t -> Core__.Import.Time.Date_and_ofday.t -> Index.t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 =
      positional
        "positional_2"
        param_core___import_time_date_and_ofday_t
        ~docstring:"Core__.Import.Time.Date_and_ofday.t"
    in
    Core__Core_time_float.Zone.index_of_date_and_ofday positional_1 positional_2
    |> python_of_index_t

  let index_offset_from_utc_exn () =
    (* t -> Index.t -> Core__.Import.Time.Span.t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_index_t ~docstring:"Index.t" in
    Core__Core_time_float.Zone.index_offset_from_utc_exn positional_1 positional_2
    |> python_of_core___import_time_span_t

  let index_abbreviation_exn () =
    (* t -> Index.t -> string *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_index_t ~docstring:"Index.t" in
    Core__Core_time_float.Zone.index_abbreviation_exn positional_1 positional_2
    |> python_of_string

  let index_has_prev_clock_shift () =
    (* t -> Index.t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_index_t ~docstring:"Index.t" in
    Core__Core_time_float.Zone.index_has_prev_clock_shift positional_1 positional_2
    |> python_of_bool

  let index_prev_clock_shift_time_exn () =
    (* t -> Index.t -> Core__.Import.Time.t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_index_t ~docstring:"Index.t" in
    Core__Core_time_float.Zone.index_prev_clock_shift_time_exn positional_1 positional_2
    |> python_of_core___import_time_t

  let index_prev_clock_shift_amount_exn () =
    (* t -> Index.t -> Core__.Import.Time.Span.t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_index_t ~docstring:"Index.t" in
    Core__Core_time_float.Zone.index_prev_clock_shift_amount_exn positional_1 positional_2
    |> python_of_core___import_time_span_t

  let index_has_next_clock_shift () =
    (* t -> Index.t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_index_t ~docstring:"Index.t" in
    Core__Core_time_float.Zone.index_has_next_clock_shift positional_1 positional_2
    |> python_of_bool

  let index_next_clock_shift_time_exn () =
    (* t -> Index.t -> Core__.Import.Time.t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_index_t ~docstring:"Index.t" in
    Core__Core_time_float.Zone.index_next_clock_shift_time_exn positional_1 positional_2
    |> python_of_core___import_time_t

  let index_next_clock_shift_amount_exn () =
    (* t -> Index.t -> Core__.Import.Time.Span.t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_index_t ~docstring:"Index.t" in
    Core__Core_time_float.Zone.index_next_clock_shift_amount_exn positional_1 positional_2
    |> python_of_core___import_time_span_t

  let abbreviation () =
    (* t -> Core__.Import.Time.t -> string *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 =
      positional
        "positional_2"
        param_core___import_time_t
        ~docstring:"Core__.Import.Time.t"
    in
    Core__Core_time_float.Zone.abbreviation positional_1 positional_2 |> python_of_string

  let absolute_time_of_date_and_ofday () =
    (* t -> Core__.Import.Time.Date_and_ofday.t -> Core__.Import.Time.t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 =
      positional
        "positional_2"
        param_core___import_time_date_and_ofday_t
        ~docstring:"Core__.Import.Time.Date_and_ofday.t"
    in
    Core__Core_time_float.Zone.absolute_time_of_date_and_ofday positional_1 positional_2
    |> python_of_core___import_time_t

  let date_and_ofday_of_absolute_time () =
    (* t -> Core__.Import.Time.t -> Core__.Import.Time.Date_and_ofday.t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 =
      positional
        "positional_2"
        param_core___import_time_t
        ~docstring:"Core__.Import.Time.t"
    in
    Core__Core_time_float.Zone.date_and_ofday_of_absolute_time positional_1 positional_2
    |> python_of_core___import_time_date_and_ofday_t

  let bin_shape_t () =
    (* Bin_prot.Shape.t *)
    Defunc.no_arg (fun () ->
        Core__Core_time_float.Zone.bin_shape_t |> python_of_bin_prot_shape_t)

  let t_of_sexp () =
    (* Sexplib0.Sexp.t -> t *)
    let%map_open positional_1 =
      positional "positional_1" param_sexplib0_sexp_t ~docstring:"Sexplib0.Sexp.t"
    in
    Core__Core_time_float.Zone.t_of_sexp positional_1 |> python_of_t

  let sexp_of_t () =
    (* t -> Ppx_sexp_conv_lib.Sexp.t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Zone.sexp_of_t positional_1
    |> python_of_ppx_sexp_conv_lib_sexp_t

  let of_string () =
    (* string -> t *)
    let%map_open positional_1 = positional "positional_1" string ~docstring:"string" in
    Core__Core_time_float.Zone.of_string positional_1 |> python_of_t

  let to_string () =
    (* t -> string *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Zone.to_string positional_1 |> python_of_string

  let pp () =
    (* Base__.Formatter.t -> t -> unit *)
    let%map_open positional_1 =
      positional "positional_1" param_base___formatter_t ~docstring:"Base__.Formatter.t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Zone.pp positional_1 positional_2 |> python_of_unit

  let greatereq () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Zone.( >= ) positional_1 positional_2 |> python_of_bool

  let lowereq () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Zone.( <= ) positional_1 positional_2 |> python_of_bool

  let eq () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Zone.( = ) positional_1 positional_2 |> python_of_bool

  let greater () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Zone.( > ) positional_1 positional_2 |> python_of_bool

  let lower () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Zone.( < ) positional_1 positional_2 |> python_of_bool

  let neq () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Zone.( <> ) positional_1 positional_2 |> python_of_bool

  let equal () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Zone.equal positional_1 positional_2 |> python_of_bool

  let compare () =
    (* t -> t -> int *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Zone.compare positional_1 positional_2 |> python_of_int

  let min () =
    (* t -> t -> t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Zone.min positional_1 positional_2 |> python_of_t

  let max () =
    (* t -> t -> t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Zone.max positional_1 positional_2 |> python_of_t

  let ascending () =
    (* t -> t -> int *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Zone.ascending positional_1 positional_2 |> python_of_int

  let descending () =
    (* t -> t -> int *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Zone.descending positional_1 positional_2 |> python_of_int

  let between () =
    (* t -> t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and low = keyword "low" param_t ~docstring:"t"
    and high = keyword "high" param_t ~docstring:"t" in
    Core__Core_time_float.Zone.between positional_1 ~low ~high |> python_of_bool

  let clamp_exn () =
    (* t -> t -> t -> t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and min = keyword "min" param_t ~docstring:"t"
    and max = keyword "max" param_t ~docstring:"t" in
    Core__Core_time_float.Zone.clamp_exn positional_1 ~min ~max |> python_of_t

  let hash_fold_t () =
    (* Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state *)
    let%map_open positional_1 =
      positional
        "positional_1"
        param_ppx_hash_lib_std_hash_state
        ~docstring:"Ppx_hash_lib.Std.Hash.state"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Zone.hash_fold_t positional_1 positional_2
    |> python_of_ppx_hash_lib_std_hash_state

  let hash () =
    (* t -> Ppx_hash_lib.Std.Hash.hash_value *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Zone.hash positional_1
    |> python_of_ppx_hash_lib_std_hash_hash_value

  let find_exn () =
    (* string -> t *)
    let%map_open positional_1 = positional "positional_1" string ~docstring:"string" in
    Core__Core_time_float.Zone.find_exn positional_1 |> python_of_t

  let initialized_zones () =
    (* unit -> ((string, t)) list *)
    let%map_open positional_1 = positional "positional_1" param_unit ~docstring:"unit" in
    Core__Core_time_float.Zone.initialized_zones positional_1
    |> python_of_list (fun (t0, t1) ->
           Py.Tuple.of_list [ python_of_string t0; python_of_t t1 ])

  let init () =
    (* unit -> unit *)
    let%map_open positional_1 = positional "positional_1" param_unit ~docstring:"unit" in
    Core__Core_time_float.Zone.init positional_1 |> python_of_unit

  let register_module ~module_name =
    let modl = Py_module.create module_name in
    Py_module.set modl "input_tz_file" (input_tz_file ());
    Py_module.set modl "of_utc_offset" (of_utc_offset ());
    Py_module.set modl "utc" (utc ());
    Py_module.set modl "name" (name ());
    Py_module.set modl "reset_transition_cache" (reset_transition_cache ());
    let subm = Index.register_module ~module_name:"core__core_time_float__zone__index" in
    Py_module.set_value modl "index" (Caml.Obj.magic subm);
    Py_module.set modl "index" (index ());
    Py_module.set modl "index_of_date_and_ofday" (index_of_date_and_ofday ());
    Py_module.set modl "index_offset_from_utc_exn" (index_offset_from_utc_exn ());
    Py_module.set modl "index_abbreviation_exn" (index_abbreviation_exn ());
    Py_module.set modl "index_has_prev_clock_shift" (index_has_prev_clock_shift ());
    Py_module.set
      modl
      "index_prev_clock_shift_time_exn"
      (index_prev_clock_shift_time_exn ());
    Py_module.set
      modl
      "index_prev_clock_shift_amount_exn"
      (index_prev_clock_shift_amount_exn ());
    Py_module.set modl "index_has_next_clock_shift" (index_has_next_clock_shift ());
    Py_module.set
      modl
      "index_next_clock_shift_time_exn"
      (index_next_clock_shift_time_exn ());
    Py_module.set
      modl
      "index_next_clock_shift_amount_exn"
      (index_next_clock_shift_amount_exn ());
    Py_module.set modl "abbreviation" (abbreviation ());
    Py_module.set
      modl
      "absolute_time_of_date_and_ofday"
      (absolute_time_of_date_and_ofday ());
    Py_module.set
      modl
      "date_and_ofday_of_absolute_time"
      (date_and_ofday_of_absolute_time ());
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
end

module Ofday = struct
  type t = Core__Core_time_float.Ofday.t

  let python_of_t, t_of_python =
    let capsule = lazy (Py.Capsule.make "Core__Core_time_float.Ofday.t") in
    (fun x -> (Lazy.force capsule |> fst) x), fun x -> (Lazy.force capsule |> snd) x

  let param_t =
    Defunc.Of_python.create ~type_name:"Core__Core_time_float.Ofday.t" ~conv:t_of_python

  let bin_shape_t () =
    (* Bin_prot.Shape.t *)
    Defunc.no_arg (fun () ->
        Core__Core_time_float.Ofday.bin_shape_t |> python_of_bin_prot_shape_t)

  let t_of_sexp () =
    (* Sexplib0.Sexp.t -> t *)
    let%map_open positional_1 =
      positional "positional_1" param_sexplib0_sexp_t ~docstring:"Sexplib0.Sexp.t"
    in
    Core__Core_time_float.Ofday.t_of_sexp positional_1 |> python_of_t

  let sexp_of_t () =
    (* t -> Sexplib0.Sexp.t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.sexp_of_t positional_1 |> python_of_sexplib0_sexp_t

  let greatereq () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.( >= ) positional_1 positional_2 |> python_of_bool

  let lowereq () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.( <= ) positional_1 positional_2 |> python_of_bool

  let eq () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.( = ) positional_1 positional_2 |> python_of_bool

  let greater () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.( > ) positional_1 positional_2 |> python_of_bool

  let lower () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.( < ) positional_1 positional_2 |> python_of_bool

  let neq () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.( <> ) positional_1 positional_2 |> python_of_bool

  let equal () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.equal positional_1 positional_2 |> python_of_bool

  let compare () =
    (* t -> t -> int *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.compare positional_1 positional_2 |> python_of_int

  let min () =
    (* t -> t -> t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.min positional_1 positional_2 |> python_of_t

  let max () =
    (* t -> t -> t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.max positional_1 positional_2 |> python_of_t

  let ascending () =
    (* t -> t -> int *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.ascending positional_1 positional_2 |> python_of_int

  let descending () =
    (* t -> t -> int *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.descending positional_1 positional_2 |> python_of_int

  let between () =
    (* t -> t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and low = keyword "low" param_t ~docstring:"t"
    and high = keyword "high" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.between positional_1 ~low ~high |> python_of_bool

  let clamp_exn () =
    (* t -> t -> t -> t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and min = keyword "min" param_t ~docstring:"t"
    and max = keyword "max" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.clamp_exn positional_1 ~min ~max |> python_of_t

  let hash_fold_t () =
    (* Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state *)
    let%map_open positional_1 =
      positional
        "positional_1"
        param_ppx_hash_lib_std_hash_state
        ~docstring:"Ppx_hash_lib.Std.Hash.state"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.hash_fold_t positional_1 positional_2
    |> python_of_ppx_hash_lib_std_hash_state

  let hash () =
    (* t -> Ppx_hash_lib.Std.Hash.hash_value *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.hash positional_1
    |> python_of_ppx_hash_lib_std_hash_hash_value

  let pp () =
    (* Base__.Formatter.t -> t -> unit *)
    let%map_open positional_1 =
      positional "positional_1" param_base___formatter_t ~docstring:"Base__.Formatter.t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.pp positional_1 positional_2 |> python_of_unit

  let greatereq_approx () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.( >=. ) positional_1 positional_2 |> python_of_bool

  let lowereq_approx () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.( <=. ) positional_1 positional_2 |> python_of_bool

  let eq_approx () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.( =. ) positional_1 positional_2 |> python_of_bool

  let greater_approx () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.( >. ) positional_1 positional_2 |> python_of_bool

  let lower_approx () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.( <. ) positional_1 positional_2 |> python_of_bool

  let neq_approx () =
    (* t -> t -> bool *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.( <>. ) positional_1 positional_2 |> python_of_bool

  let robustly_compare () =
    (* t -> t -> int *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.robustly_compare positional_1 positional_2
    |> python_of_int

  let of_string () =
    (* string -> t *)
    let%map_open positional_1 = positional "positional_1" string ~docstring:"string" in
    Core__Core_time_float.Ofday.of_string positional_1 |> python_of_t

  let to_string () =
    (* t -> string *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.to_string positional_1 |> python_of_string

  let to_parts () =
    (* t -> Core__.Import.Time.Span.Parts.t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.to_parts positional_1
    |> python_of_core___import_time_span_parts_t

  let start_of_day () =
    (* t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Ofday.start_of_day |> python_of_t)

  let start_of_next_day () =
    (* t *)
    Defunc.no_arg (fun () -> Core__Core_time_float.Ofday.start_of_next_day |> python_of_t)

  let approximate_end_of_day () =
    (* t *)
    Defunc.no_arg (fun () ->
        Core__Core_time_float.Ofday.approximate_end_of_day |> python_of_t)

  let to_span_since_start_of_day () =
    (* t -> Core__.Import.Time.Span.t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.to_span_since_start_of_day positional_1
    |> python_of_core___import_time_span_t

  let of_span_since_start_of_day_exn () =
    (* Core__.Import.Time.Span.t -> t *)
    let%map_open positional_1 =
      positional
        "positional_1"
        param_core___import_time_span_t
        ~docstring:"Core__.Import.Time.Span.t"
    in
    Core__Core_time_float.Ofday.of_span_since_start_of_day_exn positional_1 |> python_of_t

  let of_span_since_start_of_day () =
    (* Core__.Import.Time.Span.t -> t *)
    let%map_open positional_1 =
      positional
        "positional_1"
        param_core___import_time_span_t
        ~docstring:"Core__.Import.Time.Span.t"
    in
    Core__Core_time_float.Ofday.of_span_since_start_of_day positional_1 |> python_of_t

  let diff () =
    (* t -> t -> Core__.Import.Time.Span.t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.diff positional_1 positional_2
    |> python_of_core___import_time_span_t

  let small_diff () =
    (* t -> t -> Core__.Import.Time.Span.t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and positional_2 = positional "positional_2" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.small_diff positional_1 positional_2
    |> python_of_core___import_time_span_t

  let to_string_trimmed () =
    (* t -> string *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.to_string_trimmed positional_1 |> python_of_string

  let to_sec_string () =
    (* t -> string *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.to_sec_string positional_1 |> python_of_string

  let to_millisecond_string () =
    (* t -> string *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.to_millisecond_string positional_1 |> python_of_string

  let to_millisec_string () =
    (* t -> string *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Ofday.to_millisec_string positional_1 |> python_of_string

  module Zoned = struct
    type t = Core__Core_time_float.Ofday.Zoned.t

    let python_of_t, t_of_python =
      let capsule = lazy (Py.Capsule.make "Core__Core_time_float.Ofday.Zoned.t") in
      (fun x -> (Lazy.force capsule |> fst) x), fun x -> (Lazy.force capsule |> snd) x

    let param_t =
      Defunc.Of_python.create
        ~type_name:"Core__Core_time_float.Ofday.Zoned.t"
        ~conv:t_of_python

    let bin_shape_t () =
      (* Bin_prot.Shape.t *)
      Defunc.no_arg (fun () ->
          Core__Core_time_float.Ofday.Zoned.bin_shape_t |> python_of_bin_prot_shape_t)

    let t_of_sexp () =
      (* Sexplib0.Sexp.t -> t *)
      let%map_open positional_1 =
        positional "positional_1" param_sexplib0_sexp_t ~docstring:"Sexplib0.Sexp.t"
      in
      Core__Core_time_float.Ofday.Zoned.t_of_sexp positional_1 |> python_of_t

    let sexp_of_t () =
      (* t -> Sexplib0.Sexp.t *)
      let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
      Core__Core_time_float.Ofday.Zoned.sexp_of_t positional_1
      |> python_of_sexplib0_sexp_t

    let hash_fold_t () =
      (* Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state *)
      let%map_open positional_1 =
        positional
          "positional_1"
          param_ppx_hash_lib_std_hash_state
          ~docstring:"Ppx_hash_lib.Std.Hash.state"
      and positional_2 = positional "positional_2" param_t ~docstring:"t" in
      Core__Core_time_float.Ofday.Zoned.hash_fold_t positional_1 positional_2
      |> python_of_ppx_hash_lib_std_hash_state

    let hash () =
      (* t -> Ppx_hash_lib.Std.Hash.hash_value *)
      let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
      Core__Core_time_float.Ofday.Zoned.hash positional_1
      |> python_of_ppx_hash_lib_std_hash_hash_value

    let pp () =
      (* Base__.Formatter.t -> t -> unit *)
      let%map_open positional_1 =
        positional "positional_1" param_base___formatter_t ~docstring:"Base__.Formatter.t"
      and positional_2 = positional "positional_2" param_t ~docstring:"t" in
      Core__Core_time_float.Ofday.Zoned.pp positional_1 positional_2 |> python_of_unit

    let of_string () =
      (* string -> t *)
      let%map_open positional_1 = positional "positional_1" string ~docstring:"string" in
      Core__Core_time_float.Ofday.Zoned.of_string positional_1 |> python_of_t

    let to_string () =
      (* t -> string *)
      let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
      Core__Core_time_float.Ofday.Zoned.to_string positional_1 |> python_of_string

    let create () =
      (* Core__.Import.Time.Ofday.t -> Zone.t -> t *)
      let%map_open positional_1 =
        positional
          "positional_1"
          param_core___import_time_ofday_t
          ~docstring:"Core__.Import.Time.Ofday.t"
      and positional_2 = positional "positional_2" param_zone_t ~docstring:"Zone.t" in
      Core__Core_time_float.Ofday.Zoned.create positional_1 positional_2 |> python_of_t

    let create_local () =
      (* Core__.Import.Time.Ofday.t -> t *)
      let%map_open positional_1 =
        positional
          "positional_1"
          param_core___import_time_ofday_t
          ~docstring:"Core__.Import.Time.Ofday.t"
      in
      Core__Core_time_float.Ofday.Zoned.create_local positional_1 |> python_of_t

    let ofday () =
      (* t -> Core__.Import.Time.Ofday.t *)
      let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
      Core__Core_time_float.Ofday.Zoned.ofday positional_1
      |> python_of_core___import_time_ofday_t

    let zone () =
      (* t -> Zone.t *)
      let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
      Core__Core_time_float.Ofday.Zoned.zone positional_1 |> python_of_zone_t

    let to_time () =
      (* t -> Core__.Import.Date.t -> Core__.Import.Time.t *)
      let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
      and positional_2 =
        positional
          "positional_2"
          param_core___import_date_t
          ~docstring:"Core__.Import.Date.t"
      in
      Core__Core_time_float.Ofday.Zoned.to_time positional_1 positional_2
      |> python_of_core___import_time_t

    module With_nonchronological_compare = struct
      type t = Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.t

      let python_of_t, t_of_python =
        let capsule =
          lazy
            (Py.Capsule.make
               "Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.t")
        in
        (fun x -> (Lazy.force capsule |> fst) x), fun x -> (Lazy.force capsule |> snd) x

      let param_t =
        Defunc.Of_python.create
          ~type_name:"Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.t"
          ~conv:t_of_python

      let bin_shape_t () =
        (* Bin_prot.Shape.t *)
        Defunc.no_arg (fun () ->
            Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.bin_shape_t
            |> python_of_bin_prot_shape_t)

      let t_of_sexp () =
        (* Sexplib0.Sexp.t -> t *)
        let%map_open positional_1 =
          positional "positional_1" param_sexplib0_sexp_t ~docstring:"Sexplib0.Sexp.t"
        in
        Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.t_of_sexp
          positional_1
        |> python_of_t

      let sexp_of_t () =
        (* t -> Sexplib0.Sexp.t *)
        let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
        Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.sexp_of_t
          positional_1
        |> python_of_sexplib0_sexp_t

      let compare () =
        (* t -> t -> int *)
        let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
        and positional_2 = positional "positional_2" param_t ~docstring:"t" in
        Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.compare
          positional_1
          positional_2
        |> python_of_int

      let hash_fold_t () =
        (* Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state *)
        let%map_open positional_1 =
          positional
            "positional_1"
            param_ppx_hash_lib_std_hash_state
            ~docstring:"Ppx_hash_lib.Std.Hash.state"
        and positional_2 = positional "positional_2" param_t ~docstring:"t" in
        Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.hash_fold_t
          positional_1
          positional_2
        |> python_of_ppx_hash_lib_std_hash_state

      let hash () =
        (* t -> Ppx_hash_lib.Std.Hash.hash_value *)
        let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
        Core__Core_time_float.Ofday.Zoned.With_nonchronological_compare.hash positional_1
        |> python_of_ppx_hash_lib_std_hash_hash_value

      let register_module ~module_name =
        let modl = Py_module.create module_name in
        Py_module.set modl "bin_shape_t" (bin_shape_t ());
        Py_module.set modl "t_of_sexp" (t_of_sexp ());
        Py_module.set modl "sexp_of_t" (sexp_of_t ());
        Py_module.set modl "compare" (compare ());
        Py_module.set modl "hash_fold_t" (hash_fold_t ());
        Py_module.set modl "hash" (hash ());
        modl
    end

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
      let subm =
        With_nonchronological_compare.register_module
          ~module_name:
            "core__core_time_float__ofday__zoned__with_nonchronological_compare"
      in
      Py_module.set_value modl "with_nonchronological_compare" (Caml.Obj.magic subm);
      modl
  end

  let now () =
    (* Zone.t -> t *)
    let%map_open zone = keyword "zone" param_zone_t ~docstring:"Zone.t" in
    Core__Core_time_float.Ofday.now ~zone |> python_of_t

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
    Py_module.set
      modl
      "of_span_since_start_of_day_exn"
      (of_span_since_start_of_day_exn ());
    Py_module.set modl "of_span_since_start_of_day" (of_span_since_start_of_day ());
    Py_module.set modl "diff" (diff ());
    Py_module.set modl "small_diff" (small_diff ());
    Py_module.set modl "to_string_trimmed" (to_string_trimmed ());
    Py_module.set modl "to_sec_string" (to_sec_string ());
    Py_module.set modl "to_millisecond_string" (to_millisecond_string ());
    Py_module.set modl "to_millisec_string" (to_millisec_string ());
    let subm = Zoned.register_module ~module_name:"core__core_time_float__ofday__zoned" in
    Py_module.set_value modl "zoned" (Caml.Obj.magic subm);
    Py_module.set modl "now" (now ());
    modl
end

type t = Core__Core_time_float.t

let python_of_t, t_of_python =
  let capsule = lazy (Py.Capsule.make "Core__Core_time_float.t") in
  (fun x -> (Lazy.force capsule |> fst) x), fun x -> (Lazy.force capsule |> snd) x

let param_t =
  Defunc.Of_python.create ~type_name:"Core__Core_time_float.t" ~conv:t_of_python

type underlying = Core__Core_time_float.underlying

let python_of_underlying, underlying_of_python =
  let capsule = lazy (Py.Capsule.make "Core__Core_time_float.underlying") in
  (fun x -> (Lazy.force capsule |> fst) x), fun x -> (Lazy.force capsule |> snd) x

let param_underlying =
  Defunc.Of_python.create
    ~type_name:"Core__Core_time_float.underlying"
    ~conv:underlying_of_python

let next () =
  (* t -> t *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
  Core__Core_time_float.next positional_1 |> python_of_t

let prev () =
  (* t -> t *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
  Core__Core_time_float.prev positional_1 |> python_of_t

let to_span_since_epoch () =
  (* t -> Core__.Import.Time.Span.t *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
  Core__Core_time_float.to_span_since_epoch positional_1
  |> python_of_core___import_time_span_t

let of_span_since_epoch () =
  (* Core__.Import.Time.Span.t -> t *)
  let%map_open positional_1 =
    positional
      "positional_1"
      param_core___import_time_span_t
      ~docstring:"Core__.Import.Time.Span.t"
  in
  Core__Core_time_float.of_span_since_epoch positional_1 |> python_of_t

module Date_and_ofday = struct
  type t = Core__Core_time_float.Date_and_ofday.t

  let python_of_t, t_of_python =
    let capsule = lazy (Py.Capsule.make "Core__Core_time_float.Date_and_ofday.t") in
    (fun x -> (Lazy.force capsule |> fst) x), fun x -> (Lazy.force capsule |> snd) x

  let param_t =
    Defunc.Of_python.create
      ~type_name:"Core__Core_time_float.Date_and_ofday.t"
      ~conv:t_of_python

  let of_date_ofday () =
    (* Core_kernel__.Date0.t -> Core__.Import.Time.Ofday.t -> t *)
    let%map_open positional_1 =
      positional
        "positional_1"
        param_core_kernel___date0_t
        ~docstring:"Core_kernel__.Date0.t"
    and positional_2 =
      positional
        "positional_2"
        param_core___import_time_ofday_t
        ~docstring:"Core__.Import.Time.Ofday.t"
    in
    Core__Core_time_float.Date_and_ofday.of_date_ofday positional_1 positional_2
    |> python_of_t

  let to_date_ofday () =
    (* t -> (Core_kernel__.Date0.t, Core__.Import.Time.Ofday.t) *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Date_and_ofday.to_date_ofday positional_1
    |> fun (t0, t1) ->
    Py.Tuple.of_list
      [ python_of_core_kernel___date0_t t0; python_of_core___import_time_ofday_t t1 ]

  let to_date () =
    (* t -> Core_kernel__.Date0.t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Date_and_ofday.to_date positional_1
    |> python_of_core_kernel___date0_t

  let to_ofday () =
    (* t -> Core__.Import.Time.Ofday.t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Date_and_ofday.to_ofday positional_1
    |> python_of_core___import_time_ofday_t

  let of_absolute () =
    (* Core__.Import.Time.t -> Core__.Import.Time.Span.t -> t *)
    let%map_open positional_1 =
      positional
        "positional_1"
        param_core___import_time_t
        ~docstring:"Core__.Import.Time.t"
    and offset_from_utc =
      keyword
        "offset_from_utc"
        param_core___import_time_span_t
        ~docstring:"Core__.Import.Time.Span.t"
    in
    Core__Core_time_float.Date_and_ofday.of_absolute positional_1 ~offset_from_utc
    |> python_of_t

  let to_absolute () =
    (* t -> Core__.Import.Time.Span.t -> Core__.Import.Time.t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
    and offset_from_utc =
      keyword
        "offset_from_utc"
        param_core___import_time_span_t
        ~docstring:"Core__.Import.Time.Span.t"
    in
    Core__Core_time_float.Date_and_ofday.to_absolute positional_1 ~offset_from_utc
    |> python_of_core___import_time_t

  let of_synthetic_span_since_epoch () =
    (* Core__.Import.Time.Span.t -> t *)
    let%map_open positional_1 =
      positional
        "positional_1"
        param_core___import_time_span_t
        ~docstring:"Core__.Import.Time.Span.t"
    in
    Core__Core_time_float.Date_and_ofday.of_synthetic_span_since_epoch positional_1
    |> python_of_t

  let to_synthetic_span_since_epoch () =
    (* t -> Core__.Import.Time.Span.t *)
    let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
    Core__Core_time_float.Date_and_ofday.to_synthetic_span_since_epoch positional_1
    |> python_of_core___import_time_span_t

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
end

let now () =
  (* unit -> t *)
  let%map_open positional_1 = positional "positional_1" param_unit ~docstring:"unit" in
  Core__Core_time_float.now positional_1 |> python_of_t

let add () =
  (* t -> Core__.Import.Time.Span.t -> t *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and positional_2 =
    positional
      "positional_2"
      param_core___import_time_span_t
      ~docstring:"Core__.Import.Time.Span.t"
  in
  Core__Core_time_float.add positional_1 positional_2 |> python_of_t

let sub () =
  (* t -> Core__.Import.Time.Span.t -> t *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and positional_2 =
    positional
      "positional_2"
      param_core___import_time_span_t
      ~docstring:"Core__.Import.Time.Span.t"
  in
  Core__Core_time_float.sub positional_1 positional_2 |> python_of_t

let diff () =
  (* t -> t -> Core__.Import.Time.Span.t *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and positional_2 = positional "positional_2" param_t ~docstring:"t" in
  Core__Core_time_float.diff positional_1 positional_2
  |> python_of_core___import_time_span_t

let abs_diff () =
  (* t -> t -> Core__.Import.Time.Span.t *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and positional_2 = positional "positional_2" param_t ~docstring:"t" in
  Core__Core_time_float.abs_diff positional_1 positional_2
  |> python_of_core___import_time_span_t

let is_earlier () =
  (* t -> t -> bool *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and than = keyword "than" param_t ~docstring:"t" in
  Core__Core_time_float.is_earlier positional_1 ~than |> python_of_bool

let is_later () =
  (* t -> t -> bool *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and than = keyword "than" param_t ~docstring:"t" in
  Core__Core_time_float.is_later positional_1 ~than |> python_of_bool

let of_date_ofday () =
  (* Core_kernel__.Zone.t -> Core_kernel__.Time_intf.Date.t -> Core__.Import.Time.Ofday.t -> t *)
  let%map_open zone =
    keyword "zone" param_core_kernel___zone_t ~docstring:"Core_kernel__.Zone.t"
  and positional_2 =
    positional
      "positional_2"
      param_core_kernel___time_intf_date_t
      ~docstring:"Core_kernel__.Time_intf.Date.t"
  and positional_3 =
    positional
      "positional_3"
      param_core___import_time_ofday_t
      ~docstring:"Core__.Import.Time.Ofday.t"
  in
  Core__Core_time_float.of_date_ofday ~zone positional_2 positional_3 |> python_of_t

let to_date_ofday () =
  (* t -> Core_kernel__.Zone.t -> (Core_kernel__.Time_intf.Date.t, Core__.Import.Time.Ofday.t) *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and zone =
    keyword "zone" param_core_kernel___zone_t ~docstring:"Core_kernel__.Zone.t"
  in
  Core__Core_time_float.to_date_ofday positional_1 ~zone
  |> fun (t0, t1) ->
  Py.Tuple.of_list
    [ python_of_core_kernel___time_intf_date_t t0
    ; python_of_core___import_time_ofday_t t1
    ]

let to_date () =
  (* t -> Core_kernel__.Zone.t -> Core_kernel__.Time_intf.Date.t *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and zone =
    keyword "zone" param_core_kernel___zone_t ~docstring:"Core_kernel__.Zone.t"
  in
  Core__Core_time_float.to_date positional_1 ~zone
  |> python_of_core_kernel___time_intf_date_t

let to_ofday () =
  (* t -> Core_kernel__.Zone.t -> Core__.Import.Time.Ofday.t *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and zone =
    keyword "zone" param_core_kernel___zone_t ~docstring:"Core_kernel__.Zone.t"
  in
  Core__Core_time_float.to_ofday positional_1 ~zone
  |> python_of_core___import_time_ofday_t

let reset_date_cache () =
  (* unit -> unit *)
  let%map_open positional_1 = positional "positional_1" param_unit ~docstring:"unit" in
  Core__Core_time_float.reset_date_cache positional_1 |> python_of_unit

let epoch () =
  (* t *)
  Defunc.no_arg (fun () -> Core__Core_time_float.epoch |> python_of_t)

let convert () =
  (* Core_kernel__.Zone.t -> Core_kernel__.Zone.t -> Core_kernel__.Time_intf.Date.t -> Core__.Import.Time.Ofday.t -> (Core_kernel__.Time_intf.Date.t, Core__.Import.Time.Ofday.t) *)
  let%map_open from_tz =
    keyword "from_tz" param_core_kernel___zone_t ~docstring:"Core_kernel__.Zone.t"
  and to_tz = keyword "to_tz" param_core_kernel___zone_t ~docstring:"Core_kernel__.Zone.t"
  and positional_3 =
    positional
      "positional_3"
      param_core_kernel___time_intf_date_t
      ~docstring:"Core_kernel__.Time_intf.Date.t"
  and positional_4 =
    positional
      "positional_4"
      param_core___import_time_ofday_t
      ~docstring:"Core__.Import.Time.Ofday.t"
  in
  Core__Core_time_float.convert ~from_tz ~to_tz positional_3 positional_4
  |> fun (t0, t1) ->
  Py.Tuple.of_list
    [ python_of_core_kernel___time_intf_date_t t0
    ; python_of_core___import_time_ofday_t t1
    ]

let utc_offset () =
  (* t -> Core_kernel__.Zone.t -> Core__.Import.Time.Span.t *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and zone =
    keyword "zone" param_core_kernel___zone_t ~docstring:"Core_kernel__.Zone.t"
  in
  Core__Core_time_float.utc_offset positional_1 ~zone
  |> python_of_core___import_time_span_t

let to_filename_string () =
  (* t -> Core_kernel__.Zone.t -> string *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and zone =
    keyword "zone" param_core_kernel___zone_t ~docstring:"Core_kernel__.Zone.t"
  in
  Core__Core_time_float.to_filename_string positional_1 ~zone |> python_of_string

let of_filename_string () =
  (* string -> Core_kernel__.Zone.t -> t *)
  let%map_open positional_1 = positional "positional_1" string ~docstring:"string"
  and zone =
    keyword "zone" param_core_kernel___zone_t ~docstring:"Core_kernel__.Zone.t"
  in
  Core__Core_time_float.of_filename_string positional_1 ~zone |> python_of_t

let to_string_abs () =
  (* t -> Core_kernel__.Zone.t -> string *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and zone =
    keyword "zone" param_core_kernel___zone_t ~docstring:"Core_kernel__.Zone.t"
  in
  Core__Core_time_float.to_string_abs positional_1 ~zone |> python_of_string

let to_string_abs_trimmed () =
  (* t -> Core_kernel__.Zone.t -> string *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and zone =
    keyword "zone" param_core_kernel___zone_t ~docstring:"Core_kernel__.Zone.t"
  in
  Core__Core_time_float.to_string_abs_trimmed positional_1 ~zone |> python_of_string

let to_string_abs_parts () =
  (* t -> Core_kernel__.Zone.t -> string list *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and zone =
    keyword "zone" param_core_kernel___zone_t ~docstring:"Core_kernel__.Zone.t"
  in
  Core__Core_time_float.to_string_abs_parts positional_1 ~zone
  |> python_of_list python_of_string

let to_string_trimmed () =
  (* t -> Core_kernel__.Zone.t -> string *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and zone =
    keyword "zone" param_core_kernel___zone_t ~docstring:"Core_kernel__.Zone.t"
  in
  Core__Core_time_float.to_string_trimmed positional_1 ~zone |> python_of_string

let to_sec_string () =
  (* t -> Core_kernel__.Zone.t -> string *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and zone =
    keyword "zone" param_core_kernel___zone_t ~docstring:"Core_kernel__.Zone.t"
  in
  Core__Core_time_float.to_sec_string positional_1 ~zone |> python_of_string

let of_localized_string () =
  (* Core_kernel__.Zone.t -> string -> t *)
  let%map_open zone =
    keyword "zone" param_core_kernel___zone_t ~docstring:"Core_kernel__.Zone.t"
  and positional_2 = positional "positional_2" string ~docstring:"string" in
  Core__Core_time_float.of_localized_string ~zone positional_2 |> python_of_t

let to_string_iso8601_basic () =
  (* t -> Core_kernel__.Zone.t -> string *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and zone =
    keyword "zone" param_core_kernel___zone_t ~docstring:"Core_kernel__.Zone.t"
  in
  Core__Core_time_float.to_string_iso8601_basic positional_1 ~zone |> python_of_string

let bin_shape_t () =
  (* Bin_prot.Shape.t *)
  Defunc.no_arg (fun () ->
      Core__Core_time_float.bin_shape_t |> python_of_bin_prot_shape_t)

let t_of_sexp () =
  (* Sexplib0.Sexp.t -> t *)
  let%map_open positional_1 =
    positional "positional_1" param_sexplib0_sexp_t ~docstring:"Sexplib0.Sexp.t"
  in
  Core__Core_time_float.t_of_sexp positional_1 |> python_of_t

let sexp_of_t () =
  (* t -> Ppx_sexp_conv_lib.Sexp.t *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
  Core__Core_time_float.sexp_of_t positional_1 |> python_of_ppx_sexp_conv_lib_sexp_t

let of_string () =
  (* string -> t *)
  let%map_open positional_1 = positional "positional_1" string ~docstring:"string" in
  Core__Core_time_float.of_string positional_1 |> python_of_t

let to_string () =
  (* t -> string *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
  Core__Core_time_float.to_string positional_1 |> python_of_string

let pp () =
  (* Base__.Formatter.t -> t -> unit *)
  let%map_open positional_1 =
    positional "positional_1" param_base___formatter_t ~docstring:"Base__.Formatter.t"
  and positional_2 = positional "positional_2" param_t ~docstring:"t" in
  Core__Core_time_float.pp positional_1 positional_2 |> python_of_unit

let greatereq () =
  (* t -> t -> bool *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and positional_2 = positional "positional_2" param_t ~docstring:"t" in
  Core__Core_time_float.( >= ) positional_1 positional_2 |> python_of_bool

let lowereq () =
  (* t -> t -> bool *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and positional_2 = positional "positional_2" param_t ~docstring:"t" in
  Core__Core_time_float.( <= ) positional_1 positional_2 |> python_of_bool

let eq () =
  (* t -> t -> bool *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and positional_2 = positional "positional_2" param_t ~docstring:"t" in
  Core__Core_time_float.( = ) positional_1 positional_2 |> python_of_bool

let greater () =
  (* t -> t -> bool *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and positional_2 = positional "positional_2" param_t ~docstring:"t" in
  Core__Core_time_float.( > ) positional_1 positional_2 |> python_of_bool

let lower () =
  (* t -> t -> bool *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and positional_2 = positional "positional_2" param_t ~docstring:"t" in
  Core__Core_time_float.( < ) positional_1 positional_2 |> python_of_bool

let neq () =
  (* t -> t -> bool *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and positional_2 = positional "positional_2" param_t ~docstring:"t" in
  Core__Core_time_float.( <> ) positional_1 positional_2 |> python_of_bool

let equal () =
  (* t -> t -> bool *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and positional_2 = positional "positional_2" param_t ~docstring:"t" in
  Core__Core_time_float.equal positional_1 positional_2 |> python_of_bool

let compare () =
  (* t -> t -> int *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and positional_2 = positional "positional_2" param_t ~docstring:"t" in
  Core__Core_time_float.compare positional_1 positional_2 |> python_of_int

let min () =
  (* t -> t -> t *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and positional_2 = positional "positional_2" param_t ~docstring:"t" in
  Core__Core_time_float.min positional_1 positional_2 |> python_of_t

let max () =
  (* t -> t -> t *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and positional_2 = positional "positional_2" param_t ~docstring:"t" in
  Core__Core_time_float.max positional_1 positional_2 |> python_of_t

let ascending () =
  (* t -> t -> int *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and positional_2 = positional "positional_2" param_t ~docstring:"t" in
  Core__Core_time_float.ascending positional_1 positional_2 |> python_of_int

let descending () =
  (* t -> t -> int *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and positional_2 = positional "positional_2" param_t ~docstring:"t" in
  Core__Core_time_float.descending positional_1 positional_2 |> python_of_int

let between () =
  (* t -> t -> t -> bool *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and low = keyword "low" param_t ~docstring:"t"
  and high = keyword "high" param_t ~docstring:"t" in
  Core__Core_time_float.between positional_1 ~low ~high |> python_of_bool

let clamp_exn () =
  (* t -> t -> t -> t *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and min = keyword "min" param_t ~docstring:"t"
  and max = keyword "max" param_t ~docstring:"t" in
  Core__Core_time_float.clamp_exn positional_1 ~min ~max |> python_of_t

let hash_fold_t () =
  (* Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state *)
  let%map_open positional_1 =
    positional
      "positional_1"
      param_ppx_hash_lib_std_hash_state
      ~docstring:"Ppx_hash_lib.Std.Hash.state"
  and positional_2 = positional "positional_2" param_t ~docstring:"t" in
  Core__Core_time_float.hash_fold_t positional_1 positional_2
  |> python_of_ppx_hash_lib_std_hash_state

let hash () =
  (* t -> Ppx_hash_lib.Std.Hash.hash_value *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t" in
  Core__Core_time_float.hash positional_1 |> python_of_ppx_hash_lib_std_hash_hash_value

let get_sexp_zone () =
  (* unit -> Zone.t *)
  let%map_open positional_1 = positional "positional_1" param_unit ~docstring:"unit" in
  Core__Core_time_float.get_sexp_zone positional_1 |> python_of_zone_t

let set_sexp_zone () =
  (* Zone.t -> unit *)
  let%map_open positional_1 =
    positional "positional_1" param_zone_t ~docstring:"Zone.t"
  in
  Core__Core_time_float.set_sexp_zone positional_1 |> python_of_unit

let greatereq_approx () =
  (* t -> t -> bool *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and positional_2 = positional "positional_2" param_t ~docstring:"t" in
  Core__Core_time_float.( >=. ) positional_1 positional_2 |> python_of_bool

let lowereq_approx () =
  (* t -> t -> bool *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and positional_2 = positional "positional_2" param_t ~docstring:"t" in
  Core__Core_time_float.( <=. ) positional_1 positional_2 |> python_of_bool

let eq_approx () =
  (* t -> t -> bool *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and positional_2 = positional "positional_2" param_t ~docstring:"t" in
  Core__Core_time_float.( =. ) positional_1 positional_2 |> python_of_bool

let greater_approx () =
  (* t -> t -> bool *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and positional_2 = positional "positional_2" param_t ~docstring:"t" in
  Core__Core_time_float.( >. ) positional_1 positional_2 |> python_of_bool

let lower_approx () =
  (* t -> t -> bool *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and positional_2 = positional "positional_2" param_t ~docstring:"t" in
  Core__Core_time_float.( <. ) positional_1 positional_2 |> python_of_bool

let neq_approx () =
  (* t -> t -> bool *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and positional_2 = positional "positional_2" param_t ~docstring:"t" in
  Core__Core_time_float.( <>. ) positional_1 positional_2 |> python_of_bool

let robustly_compare () =
  (* t -> t -> int *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and positional_2 = positional "positional_2" param_t ~docstring:"t" in
  Core__Core_time_float.robustly_compare positional_1 positional_2 |> python_of_int

let of_tm () =
  (* Core__.Core_unix.tm -> Zone.t -> t *)
  let%map_open positional_1 =
    positional "positional_1" param_core___core_unix_tm ~docstring:"Core__.Core_unix.tm"
  and zone = keyword "zone" param_zone_t ~docstring:"Zone.t" in
  Core__Core_time_float.of_tm positional_1 ~zone |> python_of_t

let of_string_abs () =
  (* string -> t *)
  let%map_open positional_1 = positional "positional_1" string ~docstring:"string" in
  Core__Core_time_float.of_string_abs positional_1 |> python_of_t

let t_of_sexp_abs () =
  (* Core__.Import.Sexp.t -> t *)
  let%map_open positional_1 =
    positional "positional_1" param_core___import_sexp_t ~docstring:"Core__.Import.Sexp.t"
  in
  Core__Core_time_float.t_of_sexp_abs positional_1 |> python_of_t

let sexp_of_t_abs () =
  (* t -> Zone.t -> Core__.Import.Sexp.t *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and zone = keyword "zone" param_zone_t ~docstring:"Zone.t" in
  Core__Core_time_float.sexp_of_t_abs positional_1 ~zone |> python_of_core___import_sexp_t

let pause () =
  (* Span.t -> unit *)
  let%map_open positional_1 =
    positional "positional_1" param_span_t ~docstring:"Span.t"
  in
  Core__Core_time_float.pause positional_1 |> python_of_unit

let pause_forever () =
  (* unit -> Core__.Import.never_returns *)
  let%map_open positional_1 = positional "positional_1" param_unit ~docstring:"unit" in
  Core__Core_time_float.pause_forever positional_1
  |> python_of_core___import_never_returns

let format () =
  (* t -> string -> Zone.t -> string *)
  let%map_open positional_1 = positional "positional_1" param_t ~docstring:"t"
  and positional_2 = positional "positional_2" string ~docstring:"string"
  and zone = keyword "zone" param_zone_t ~docstring:"Zone.t" in
  Core__Core_time_float.format positional_1 positional_2 ~zone |> python_of_string

let parse () =
  (* string -> string -> Zone.t -> t *)
  let%map_open positional_1 = positional "positional_1" string ~docstring:"string"
  and fmt = keyword "fmt" string ~docstring:"string"
  and zone = keyword "zone" param_zone_t ~docstring:"Zone.t" in
  Core__Core_time_float.parse positional_1 ~fmt ~zone |> python_of_t

module Exposed_for_tests = struct
  let ensure_colon_in_offset () =
    (* string -> string *)
    let%map_open positional_1 = positional "positional_1" string ~docstring:"string" in
    Core__Core_time_float.Exposed_for_tests.ensure_colon_in_offset positional_1
    |> python_of_string

  let register_module ~module_name =
    let modl = Py_module.create module_name in
    Py_module.set modl "ensure_colon_in_offset" (ensure_colon_in_offset ());
    modl
end

let register_module ~module_name =
  let modl = Py_module.create module_name in
  let subm = Span.register_module ~module_name:"core__core_time_float__span" in
  Py_module.set_value modl "span" (Caml.Obj.magic subm);
  let subm = Zone.register_module ~module_name:"core__core_time_float__zone" in
  Py_module.set_value modl "zone" (Caml.Obj.magic subm);
  let subm = Ofday.register_module ~module_name:"core__core_time_float__ofday" in
  Py_module.set_value modl "ofday" (Caml.Obj.magic subm);
  Py_module.set modl "next" (next ());
  Py_module.set modl "prev" (prev ());
  Py_module.set modl "to_span_since_epoch" (to_span_since_epoch ());
  Py_module.set modl "of_span_since_epoch" (of_span_since_epoch ());
  let subm =
    Date_and_ofday.register_module ~module_name:"core__core_time_float__date_and_ofday"
  in
  Py_module.set_value modl "date_and_ofday" (Caml.Obj.magic subm);
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
  let subm =
    Exposed_for_tests.register_module
      ~module_name:"core__core_time_float__exposed_for_tests"
  in
  Py_module.set_value modl "exposed_for_tests" (Obj.magic subm);
  modl
