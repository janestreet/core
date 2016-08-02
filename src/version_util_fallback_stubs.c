/* This file is always linked into Core. This allows Core to be installed as a
   standalone library, e.g. for use in a toplevel. Our jenga/root.ml, when
   building an executable, generates primitives with real information in
   *.build_info.c and *.hg_version.c. The latter ones have precedence as the
   ones in this file are defined as weak. */


#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

CAMLprim CAMLweakdef value generated_hg_version (value unit __attribute__ ((unused)))
{
  char v[] = "NO_VERSION_UTIL";
  return(caml_copy_string(v));
}

CAMLprim CAMLweakdef value generated_build_info (value unit __attribute__ ((unused)))
{
  char v[] = "("
      "(username \"\")"
      "(hostname \"\")"
      "(kernel   \"\")"
      "(build_date \"1970-01-01\")"
      "(build_time \"00:00:00\")"
      "(x_library_inlining false)"
      "(dynlinkable_code false)"
      "(ocaml_version \"\")"
      "(executable_path \"\")"
      "(build_system \"\")"
    ")";
  return(caml_copy_string(v));
}
