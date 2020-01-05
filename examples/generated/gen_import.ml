(* Explicitly add the [no_arg] function. *)
module Defunc = struct
  include Python_lib.Defunc

  let no_arg fn = return () |> map ~f:fn
end

module Py_module = struct
  include Python_lib.Py_module

  (* Temporary hack until pythonlib exposes this function. *)
  let to_pyobject (m : t) = (Obj.magic m : Pytypes.pyobject)
end

let python_of_unit () = Py.none
