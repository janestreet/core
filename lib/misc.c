
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

/* Fixing 5193 */

/* Fix the broken close_(in/out) function which does not release the
   caml lock. */

#define IO_BUFFER_SIZE 65536

typedef long file_offset;

struct channel {
  int fd;                       /* Unix file descriptor */
  file_offset offset;           /* Absolute position of fd in the file */
  char * end;                   /* Physical end of the buffer */
  char * curr;                  /* Current position in the buffer */
  char * max;                   /* Logical end of the buffer (for input) */
  void * mutex;                 /* Placeholder for mutex (for systhreads) */
  struct channel * next, * prev;/* Double chaining of channels (flush_all) */
  int revealed;                 /* For Cash only */
  int old_revealed;             /* For Cash only */
  int refcount;                 /* For flush_all and for Cash */
  int flags;                    /* Bitfield */
  char buff[IO_BUFFER_SIZE];    /* The buffer itself */
};

#define Channel(v) (*((struct channel **) (Data_custom_val(v))))

CAMLprim value fixed_close_channel(value vchannel)
{
  int result;
  int tmp_fd = -1;
  int tries = 0;
  struct channel *channel = Channel(vchannel);

  if (channel->fd != -1) {
    tmp_fd = channel->fd;
    channel->fd = -1;

    caml_enter_blocking_section();
    do {
      tries++;
      result = close(tmp_fd);
    } while(result == -1 && (errno == EINTR || errno == EAGAIN) && tries < 1000);
    caml_leave_blocking_section();

    if(result == -1) {
      channel->fd = tmp_fd;
      uerror("error closing channel", Nothing);
    } else
      channel->curr = channel->max = channel->end;
  }

  return Val_unit;
}
