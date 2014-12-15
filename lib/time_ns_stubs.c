#include <stdio.h>
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
#include <stdint.h>

#include "ocaml_utils.h"
#include "core_config.h"

#define NANOS_PER_SECOND 1000000000

#if defined(JSC_POSIX_TIMERS)

#define clockid_t_val(v_cl) ((clockid_t) Nativeint_val(v_cl))

CAMLprim value core_time_ns_gettime_or_zero()
{
  struct timespec ts;

  if (clock_gettime(CLOCK_REALTIME, &ts) != 0)
    return caml_alloc_int63(0);
  else
    return caml_alloc_int63(NANOS_PER_SECOND * ts.tv_sec + ts.tv_nsec);
}

#else

#include <sys/types.h>
#include <sys/time.h>

CAMLprim value core_time_ns_gettime_or_zero()
{
  struct timeval tp;
  if (gettimeofday(&tp, NULL) == -1)
    return caml_alloc_int63(0);
  else
    return caml_alloc_int63(NANOS_PER_SECOND * tp.tv_sec + tp.tv_usec * 1000);
}

#endif
