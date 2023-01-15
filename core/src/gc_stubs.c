#define CAML_INTERNALS
#include <caml/fail.h>
#include <caml/gc_ctrl.h>
#include <caml/memory.h>
#include <caml/memprof.h>

#if defined(_MSC_VER) && _MSC_VER >= 1500
# define __unused(x) __pragma( warning (push) ) \
    __pragma( warning (disable:4189 ) ) \
    x \
    __pragma( warning (pop))
#else
# define __unused(x) x __attribute__((unused))
#endif

static intnat minor_words(void) {
  return (intnat)(caml_stat_minor_words +
                  (double)(caml_young_end - caml_young_ptr));
}

static intnat promoted_words(void) {
  return ((intnat)caml_stat_promoted_words);
}

CAMLprim value core_gc_minor_words(__unused(value unit)) {
  return Val_long(minor_words());
}

static intnat major_words(void) {
  return (intnat)(caml_stat_major_words + (double)caml_allocated_words);
}

CAMLprim value core_gc_major_words(__unused(value unit)) {
  return Val_long(major_words());
}

CAMLprim value core_gc_promoted_words(__unused(value unit)) {
  return Val_long(promoted_words());
}

CAMLprim value core_gc_minor_collections(__unused(value unit)) {
  return Val_long(caml_stat_minor_collections);
}

CAMLprim value core_gc_major_collections(__unused(value unit)) {
  return Val_long(caml_stat_major_collections);
}

CAMLprim value core_gc_heap_words(__unused(value unit)) {
  return Val_long(caml_stat_heap_wsz);
}

CAMLprim value core_gc_heap_chunks(__unused(value unit)) {
  return Val_long(caml_stat_heap_chunks);
}

CAMLprim value core_gc_compactions(__unused(value unit)) {
  return Val_long(caml_stat_compactions);
}

CAMLprim value core_gc_top_heap_words(__unused(value unit)) {
  return Val_long(caml_stat_top_heap_wsz);
}

CAMLprim value core_gc_major_plus_minor_words(__unused(value unit)) {
  return Val_long(minor_words() + major_words());
}

CAMLprim value core_gc_allocated_words(__unused(value unit)) {
  return Val_long(minor_words() + major_words() - promoted_words());
}

CAMLprim value core_gc_run_memprof_callbacks(__unused(value unit)) {
  value exn = caml_memprof_handle_postponed_exn();
  caml_raise_if_exception(exn);
  return Val_unit;
}
