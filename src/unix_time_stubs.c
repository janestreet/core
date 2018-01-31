
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include "ocaml_utils.h"
#include "timespec.h"
#include "time_ns_stubs.h"
/* Improved localtime implementation

   Addresses bug:

   http://caml.inria.fr/mantis/view.php?id=5193
 */

#include <errno.h>
#include <stdio.h>

#include "config.h"

#ifdef JSC_POSIX_TIMERS

clockid_t caml_clockid_t_of_caml (value clock_type) {
  switch (Int_val(clock_type)) {
    case 0: return CLOCK_REALTIME;
    case 1: return CLOCK_MONOTONIC;
    case 2: return CLOCK_PROCESS_CPUTIME_ID;
    case 3: return CLOCK_THREAD_CPUTIME_ID;
  };

  caml_failwith ("invalid Clock.t");
}

value caml_clock_getres (value clock_type) {
  struct timespec tp;
  clock_getres (caml_clockid_t_of_caml (clock_type), &tp);
  return (caml_alloc_int63 (((__int64_t)tp.tv_sec * 1000 * 1000 * 1000) + (__int64_t)tp.tv_nsec));
}

value caml_clock_gettime (value clock_type) {
  struct timespec tp;
  clock_gettime (caml_clockid_t_of_caml (clock_type), &tp);
  return (caml_alloc_int63 (((__int64_t)tp.tv_sec * 1000 * 1000 * 1000) + (__int64_t)tp.tv_nsec));
}

#endif /* JSC_POSIX_TIMERS */

static value alloc_tm(struct tm *tm)
{
  value res;
  res = caml_alloc_small(9, 0);
  Field(res,0) = Val_int(tm->tm_sec);
  Field(res,1) = Val_int(tm->tm_min);
  Field(res,2) = Val_int(tm->tm_hour);
  Field(res,3) = Val_int(tm->tm_mday);
  Field(res,4) = Val_int(tm->tm_mon);
  Field(res,5) = Val_int(tm->tm_year);
  Field(res,6) = Val_int(tm->tm_wday);
  Field(res,7) = Val_int(tm->tm_yday);
  Field(res,8) = tm->tm_isdst ? Val_true : Val_false;
  return res;
}

/*
 * converts a tm structure to a float with the assumption that that the structure
 * defines a gmtime
*/
CAMLprim value core_timegm (value tm_val) {
  struct tm tm;
  time_t res;
  memset(&tm, 0, sizeof(struct tm));
  tm.tm_sec  = Int_val(Field(tm_val,0));
  tm.tm_min  = Int_val(Field(tm_val,1));
  tm.tm_hour = Int_val(Field(tm_val,2));
  tm.tm_mday = Int_val(Field(tm_val,3));
  tm.tm_mon  = Int_val(Field(tm_val,4));
  tm.tm_year = Int_val(Field(tm_val,5));
  tm.tm_wday = Int_val(Field(tm_val,6));
  tm.tm_yday = Int_val(Field(tm_val,7));
  tm.tm_isdst = 0;  /*  tm_isdst is not used by timegm (which sets it to 0) */

  res = timegm(&tm);

  if (res == (time_t) -1) caml_failwith("timegm");

  return caml_copy_double((double) res);
}

CAMLprim value core_time_ns_strftime(value v_tm, value v_fmt)
{
  struct tm tm;
  memset(&tm, 0, sizeof(struct tm));
  tm.tm_sec  = Int_val(Field(v_tm, 0));
  tm.tm_min  = Int_val(Field(v_tm, 1));
  tm.tm_hour = Int_val(Field(v_tm, 2));
  tm.tm_mday = Int_val(Field(v_tm, 3));
  tm.tm_mon  = Int_val(Field(v_tm, 4));
  tm.tm_year = Int_val(Field(v_tm, 5));
  tm.tm_wday = Int_val(Field(v_tm, 6));
  tm.tm_yday = Int_val(Field(v_tm, 7));
  tm.tm_isdst = Bool_val(Field(v_tm, 8));
  return core_kernel_time_ns_format_tm(&tm, v_fmt);
}

CAMLprim value core_time_ns_nanosleep(value v_seconds)
{
  struct timespec req = timespec_of_double(Double_val(v_seconds));
  struct timespec rem;
  int retval;

  caml_enter_blocking_section();
  retval = nanosleep(&req, &rem);
  caml_leave_blocking_section();

  if (retval == 0)
    return caml_copy_double(0.0);
  else if (retval == -1) {
    if (errno == EINTR)
      return caml_copy_double(timespec_to_double(rem));
    else
      uerror("nanosleep", Nothing);
  }
  else
    caml_failwith("core_time_ns_nanosleep: impossible return value from nanosleep(2)");
}

/*
 * These are the same functions as the ones in ocaml except that they call
 * {localtime,gmtime}_r instead of {localtime,gmtime} to avoid setting the
 * global tzname (instead setting the tm_store value that we discard).
 */
#define WRAP_TIME_FUN(NAME, ERROR)                   \
  CAMLprim value core_##NAME (value t)         \
  { \
    time_t clock; \
    struct tm *tm; \
    struct tm tm_store; \
    clock = (time_t) Double_val(t); \
    tm = NAME##_r(&clock, &tm_store); \
    if (tm == NULL) caml_failwith(ERROR); \
    return alloc_tm(tm); \
  }

WRAP_TIME_FUN(localtime, "localtime")
WRAP_TIME_FUN(gmtime, "gmtime")
