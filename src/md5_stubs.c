#include <unistd.h>
#include <errno.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#define CAML_INTERNALS
#pragma GCC diagnostic ignored "-pedantic"
#include <caml/md5.h>
#undef CAML_INTERNALS

/* Contrary to caml_md5_chan, this function releases the runtime lock.

   [fd] must be a file descriptor open for reading and not be
   nonblocking, otherwise the function might fail non-deterministically.
 */
CAMLprim value core_md5_fd(value fd)
{
  CAMLparam1 (fd);
  value res;
  struct MD5Context ctx;
  caml_enter_blocking_section();
  {
    intnat bytes_read;
    char buffer[4096];

    caml_MD5Init(&ctx);
    while (1){
      bytes_read = read (Int_val(fd), buffer, sizeof(buffer));
      if (bytes_read < 0) {
        if (errno == EINTR) continue;
        caml_leave_blocking_section();
        uerror("core_md5_fd", Nothing);
      }
      if (bytes_read == 0) break;
      caml_MD5Update (&ctx, (unsigned char *) buffer, bytes_read);
    }
  }
  caml_leave_blocking_section();
  res = caml_alloc_string(16);
  caml_MD5Final(&Byte_u(res, 0), &ctx);
  CAMLreturn (res);
}
