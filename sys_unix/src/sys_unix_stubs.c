#include "ocaml_utils.h"

CAMLprim value executing_bytecode (value *_vals, int *_nvals) {
  ((void)_vals); ((void)_nvals);
  return Val_true;
}

CAMLprim value not_executing_bytecode (value _v1, value _v2, value _v3,
                                       value _v4, value _v5, value _v6) {
  ((void)_v1); ((void)_v2); ((void)_v3);
  ((void)_v4); ((void)_v5); ((void)_v6);
  return Val_false;
}

CAMLprim value c_int_size (value _unit)
{
  ((void)_unit);
  return Val_int(sizeof(int) * 8);
}
