(* The bindings are generated by the following command:
   dune exec examples/cmt_test.exe \
     ~/tmp/core__Core_time_float.cmti \
     examples/generated/time_bindings.ml
*)
let () = Time_bindings.register ~module_name:"time"
