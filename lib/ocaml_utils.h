#ifndef OCAML_UTILS_H
#define OCAML_UTILS_H

#include <core_config.h>
#include <jane_common.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/unixsupport.h>
#include <stdint.h>

#define XSTR(S) STR(S)
#define STR(S) #S

#ifdef JSC_ARCH_SIXTYFOUR
#  define caml_alloc_int63(v) Val_long(v)
#  define Int63_val(v) Long_val(v)
#else
#  define caml_alloc_int63(v) caml_copy_int64(v)
#  define Int63_val(v) Int64_val(v)
#endif

typedef int64_t int63;

#define DEFINE_INT63_CONSTANT(name,z) \
  CAMLprim value name(value __unused v_unit) { return caml_alloc_int63(z); }

/* [strcmp] is defined as a macro in our current compilation environment.  We use
   [strcmp_not_a_macro] instead so that the text of this macro does not overflow the
   C89 limit on string literal length when used inside [assert]. */

/* defined in ocaml_utils_stubs.c */
extern int strcmp_not_a_macro(const char*, const char*);

extern value getsockopt_int(int *tcpopt, value sock, int level, value option);

extern value setsockopt_int(
  int *tcpopt, value sock, int level, value option, value status);

extern int caml_convert_signal_number(int signo);
extern int caml_rev_convert_signal_number(int signo);

extern void raise_with_two_args(value tag, value arg1, value arg2) Noreturn;

extern value* named_value_exn(const char* n);
extern void* malloc_exn(size_t size);

extern const char* string_ocaml_to_c(value s_v);
extern const char* string_of_ocaml_string_option(value v);
extern int int_of_ocaml_int_option(value v, int* i);

extern const char** array_map(value array, const char* (*f__must_not_allocate)(value));

#endif /* OCAML_UTILS_H */
