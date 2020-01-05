# This tries to copy the generated shared library to ocaml.so in the
# current directory so that the import could work.
import os, sys
from ctypes import *

ocaml_library = '_build/default/examples/generated/ocaml.bc.so'
ocaml = PyDLL(ocaml_library, RTLD_GLOBAL)
argv_t = c_char_p * 2
argv = argv_t(ocaml_library.encode('utf-8'), None)
ocaml.caml_startup(argv)

import otime
now = otime.now()
print(otime.to_string(now))
