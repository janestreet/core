#include <caml/mlvalues.h>

CAMLextern void __real_caml_modify(value *fp, value v);

static long count = 0;

void (*replace_caml_modify_hook)(void) = NULL;

CAMLprim void __wrap_caml_modify(value *fp, value v)
{
  count++;
  __real_caml_modify(fp, v);
  if (replace_caml_modify_hook != NULL) replace_caml_modify_hook ();
}

CAMLprim value replace_caml_modify_for_testing_count()
{
  return Val_long(count);
}

CAMLprim value replace_caml_modify_for_testing_reset()
{
  count = 0;
  return Val_unit;
}

CAMLextern void __real_caml_modify_local(value obj, intnat i, value val);

CAMLprim void __wrap_caml_modify_local(value obj, intnat i, value val)
{
  long next_count = count + 1;
  __real_caml_modify_local(obj, i, val);
  count = next_count;
  if (replace_caml_modify_hook != NULL) replace_caml_modify_hook ();
}
