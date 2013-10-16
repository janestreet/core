#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <caml/mlvalues.h>

#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#include <time.h>
#include <sys/time.h>
#include <stdint.h>

#ifdef __MACH__
#include <mach/clock.h>
#include <mach/mach.h>
#endif

#include "ocaml_utils.h"

CAMLprim value tsc_get()
{
#ifdef __x86_64__
  uint32_t hi, lo;
  __asm__ __volatile__ ("rdtsc" : "=a"(lo), "=d"(hi));
  /* Given we're on x86_64, caml_alloc_int63 should expand to Val_long, and hence
   * the stub may be marked noalloc */
  return caml_alloc_int63( ((uint64_t)lo) | (((uint64_t)hi)<<32) );
#else
#define NANOS_PER_SECOND 1000000000
  struct timespec ts;
  if ( clock_gettime( CLOCK_MONOTONIC, &ts ) != 0 )
    unix_error(errno, "clock_gettime", Nothing);
  else
    return caml_alloc_int63(NANOS_PER_SECOND * ts.tv_sec + ts.tv_nsec);
#endif
}

