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

#define NANOS_PER_SECOND 1000000000

CAMLprim value core_time_ns_clock_rt_gettime_or_zero()
{
  struct timespec ts;

  if (clock_gettime(CLOCK_REALTIME, &ts) != 0)
    return Val_long(0);
  else
    return Val_long(NANOS_PER_SECOND * ts.tv_sec + ts.tv_nsec);
}
