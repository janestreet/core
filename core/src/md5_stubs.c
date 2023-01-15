#define CAML_INTERNALS
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <core_params.h>
#include <errno.h>
#ifndef _MSC_VER
#include <unistd.h>
#endif

#if __GNUC__ < 8
#ifndef _MSC_VER
#pragma GCC diagnostic ignored "-pedantic"
#endif
#endif
#include <caml/md5.h>
#include <caml/sys.h>
#undef CAML_INTERNALS

/* Contrary to caml_md5_chan, this function releases the runtime lock.

   [fd] must be a file descriptor open for reading and not be
   nonblocking, otherwise the function might fail non-deterministically.
 */
CAMLprim value core_md5_fd(value fd) {
  CAMLparam1(fd);
  value res;
  struct MD5Context ctx;
  caml_enter_blocking_section();
  {
    intnat bytes_read;
    char buffer[4096];

    caml_MD5Init(&ctx);
    while (1) {
      bytes_read = read(Int_val(fd), buffer, sizeof(buffer));
      if (bytes_read < 0) {
        if (errno == EINTR)
          continue;
        caml_leave_blocking_section();
        caml_sys_io_error(NO_ARG);
      }
      if (bytes_read == 0)
        break;
      caml_MD5Update(&ctx, (unsigned char *)buffer, bytes_read);
    }
  }
  caml_leave_blocking_section();
  res = caml_alloc_string(16);
  caml_MD5Final(&Byte_u(res, 0), &ctx);
  CAMLreturn(res);
}

/* Cutoff point at which we need to release the runtime lock. The idea is that
   computing the md5 of a large block is slow so it's worth releasing the
   runtime lock to allow the computation to happen in parallel with OCaml code.

   The divisor is obtained by running the "md5 vs memcpy" benchmarks and
   comparing the results.
*/
#define MD5_CUTOFF (THREAD_IO_CUTOFF / 50)

CAMLprim value core_md5_digest_subbigstring(value buf, value ofs, value vlen,
                                            value res) {
  CAMLparam2(buf, res);
  struct MD5Context ctx;
  unsigned char *data = (unsigned char *)Caml_ba_data_val(buf) + Long_val(ofs);
  size_t len = Long_val(vlen);
  caml_MD5Init(&ctx);

  if (len > MD5_CUTOFF)
    caml_enter_blocking_section();
  caml_MD5Update(&ctx, data, len);
  if (len > MD5_CUTOFF)
    caml_leave_blocking_section();

  caml_MD5Final(&Byte_u(res, 0), &ctx);
  CAMLreturn(Val_unit);
}
