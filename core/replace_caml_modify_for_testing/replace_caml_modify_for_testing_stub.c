#define CAML_INTERNALS
#include <caml/mlvalues.h>

CAMLextern void __real_caml_modify(value *fp, value v);
CAMLextern void __real_caml_modify_local(value obj, intnat i, value val);

void (*replace_caml_modify_hook)(void) = NULL;

#ifdef MULTIDOMAIN

#include <caml/startup_aux.h>
#include <caml/domain.h>

typedef struct {
  _Alignas(64) _Atomic long count;
} counter_t;

static counter_t counters[Max_domains_max];

CAMLprim value replace_caml_modify_for_testing_count()
{
  // This assumes the counters are not being updated in parallel. If they are, it will
  // not give a linearizable result.
  long total = 0;
  for(unsigned int i = 0; i < caml_params->max_domains; i++) {
    total += counters[i].count;
  }
  return Val_long(total);
}

CAMLprim value replace_caml_modify_for_testing_reset()
{
  // This assumes the counters are not being updated in parallel. If they are, it may
  // fail to reset all counters.
  for(unsigned int i = 0; i < caml_params->max_domains; i++) {
    counters[i].count = 0;
  }
  return Val_unit;
}

CAMLprim void __wrap_caml_modify(value *fp, value v)
{
  counters[caml_state->id].count++;
  __real_caml_modify(fp, v);
  if (replace_caml_modify_hook != NULL) replace_caml_modify_hook ();
}

CAMLprim void __wrap_caml_modify_local(value obj, intnat i, value val)
{
  long next_count = counters[caml_state->id].count + 1;
  __real_caml_modify_local(obj, i, val);
  counters[caml_state->id].count = next_count;
  if (replace_caml_modify_hook != NULL) replace_caml_modify_hook ();
}

#else /* MULTIDOMAIN */

static long count = 0;

CAMLprim value replace_caml_modify_for_testing_count()
{
  return Val_long(count);
}

CAMLprim value replace_caml_modify_for_testing_reset()
{
  count = 0;
  return Val_unit;
}

CAMLprim void __wrap_caml_modify(value *fp, value v)
{
  count++;
  __real_caml_modify(fp, v);
  if (replace_caml_modify_hook != NULL) replace_caml_modify_hook ();
}

CAMLprim void __wrap_caml_modify_local(value obj, intnat i, value val)
{
  long next_count = count + 1;
  __real_caml_modify_local(obj, i, val);
  count = next_count;
  if (replace_caml_modify_hook != NULL) replace_caml_modify_hook ();
}

#endif /* MULTIDOMAIN */
