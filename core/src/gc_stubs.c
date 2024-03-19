#define CAML_INTERNALS
#include <caml/fail.h>
#include <caml/gc_ctrl.h>
#include <caml/memory.h>
#include <caml/memprof.h>
#include <caml/version.h>
#include <stdbool.h>
#include "gc_stubs.h"

#if OCAML_VERSION >= 50100 && (!OCAML_5_MINUS || defined CAML_RUNTIME_5)
#include <caml/shared_heap.h>
#include <caml/minor_gc.h>
#define HAS_OCAML_5_GC
#endif

// These stubs are only available to gc.ml, which branches based on
// %runtime5, so we expect the various [caml_failwith] cases to be
// unreachable.

#ifdef CAML_RUNTIME_5
#define HAS_COMPACTIONS_COUNT
#endif

static intnat minor_words(void) {
  return (intnat)(caml_stat_minor_words +
                  (double)(caml_young_end - caml_young_ptr));
}

static intnat promoted_words(void) {
  return ((intnat)caml_stat_promoted_words);
}

CAMLprim value core_gc_minor_words(value unit) {
  (void)unit;
  return Val_long(minor_words());
}

static intnat major_words(void) {
  return (intnat)(caml_stat_major_words + (double)caml_allocated_words);
}

CAMLprim value core_gc_major_words(value unit) {
  (void)unit;
  return Val_long(major_words());
}

CAMLprim value core_gc_promoted_words(value unit) {
  (void)unit;
  return Val_long(promoted_words());
}

CAMLprim value core_gc_minor_collections(value unit) {
  (void)unit;
  // In OCaml 5.1.0, the number of minor collections is an atomic global state
  // variable.
#ifndef HAS_OCAML_5_GC
  return Val_long(caml_stat_minor_collections);
#else
  return Val_long(atomic_load(&caml_minor_collections_count));
#endif
}

CAMLprim value core_gc_major_collections(value unit) {
  (void)unit;
  return Val_long(caml_stat_major_collections);
}

CAMLprim value core_gc_compactions(value unit) {
  (void)unit;
  // In OCaml 5-trunk, the number of collections is an atomic global state
  // variable.
#ifdef HAS_COMPACTIONS_COUNT
  return Val_long(atomic_load(&caml_compactions_count));
#elif defined HAS_OCAML_5_GC
  caml_failwith("core_gc_compactions: not yet supported in OCaml 5.");
#else
  return Val_long(caml_stat_compactions);
#endif
}

CAMLprim value core_gc_major_plus_minor_words(value unit) {
  (void)unit;
  return Val_long(minor_words() + major_words());
}

CAMLprim value core_gc_allocated_words(value unit) {
  (void)unit;
  return Val_long(minor_words() + major_words() - promoted_words());
}

CAMLprim value core_gc_run_memprof_callbacks(value unit) {
  (void)unit;
// statmemprof isn't implemented on runtime5 yet
#ifndef HAS_OCAML_5_GC
  value res = caml_memprof_handle_postponed_exn();
  if (Is_exception_result(res))
    caml_raise(Extract_exception(res));
#endif
  return Val_unit;
}

CAMLprim value core_gc_heap_words(value unit) {
  (void)unit;
#ifdef HAS_OCAML_5_GC
  caml_failwith("core_gc_heap_words: not supported in OCaml 5.");
#else
  return Val_long(caml_stat_heap_wsz);
#endif
}

CAMLprim value core_gc_heap_chunks(value unit) {
  (void)unit;
#ifdef HAS_OCAML_5_GC
  caml_failwith("core_gc_heap_chunks: not supported in OCaml 5.");
#else
  return Val_long(caml_stat_heap_chunks);
#endif
}

CAMLprim value core_gc_top_heap_words(value unit) {
  (void)unit;
#ifdef HAS_OCAML_5_GC
  caml_failwith("core_gc_top_heap_words: not supported in OCaml 5.");
#else
  return Val_long(caml_stat_top_heap_wsz);
#endif
}
