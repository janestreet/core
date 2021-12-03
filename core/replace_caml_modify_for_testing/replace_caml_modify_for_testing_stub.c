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
