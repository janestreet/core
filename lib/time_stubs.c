
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include "ocaml_utils.h"

/* Improved localtime implementation

   Addresses bug:

   http://caml.inria.fr/mantis/view.php?id=5193
 */

#include <time.h>
#include <errno.h>
#include <stdio.h>

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

  tm.tm_sec  = Int_val(Field(tm_val,0));
  tm.tm_min  = Int_val(Field(tm_val,1));
  tm.tm_hour = Int_val(Field(tm_val,2));
  tm.tm_mday = Int_val(Field(tm_val,3));
  tm.tm_mon  = Int_val(Field(tm_val,4));
  tm.tm_year = Int_val(Field(tm_val,5));
  tm.tm_wday = Int_val(Field(tm_val,6));
  tm.tm_yday = Int_val(Field(tm_val,7));
  tm.tm_isdst = 0;  /*  tm_isdst is not used by timegm (which sets it to 0) */
  tm.tm_gmtoff = 0; /* tm_gmtoff is not used by timegm (which sets it to 0) */
  tm.tm_zone = NULL;

  res = timegm(&tm);

  if (res == (time_t) -1) caml_failwith("timegm");

  return caml_copy_double((double) res);
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
