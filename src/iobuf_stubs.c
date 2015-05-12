
#include "iobuf.h"
#include "unix_utils.h"
#include "socketaddr.h"
#include "recvmmsg.h"

#ifdef JSC_RECVMMSG

CAMLprim value iobuf_recvmmsg_assume_fd_is_nonblocking_stub(
  value v_fd, value v_iobufs, value v_count, value v_srcs)
{
  CAMLparam4(v_fd, v_iobufs, v_count, v_srcs);
  CAMLlocal4(v_iobuf, v_lo, v_sockaddrs, v_hi);
  unsigned i;
  int n_read;
  unsigned count;

  count = Long_val(v_count);
  if (count != Long_val(v_count)) {
    caml_invalid_argument("iobuf_recvmmsg_assume_fd_is_nonblocking_stub: "
                          "v_count exceeds unsigned int");
  }
  else if (count > RECVMMSG_MAX_COUNT) {
    caml_invalid_argument("iobuf_recvmmsg_assume_fd_is_nonblocking_stub: "
                          "v_count exceeds RECVMMSG_MAX_COUNT");
  }
  else {
    struct mmsghdr hdrs[RECVMMSG_MAX_COUNT];
    struct iovec iovecs[RECVMMSG_MAX_COUNT];

    for (i = 0; i < count; i++) {
      v_iobuf = Field(v_iobufs, i);
      v_lo = Field(v_iobuf, iobuf_lo);
      v_hi = Field(v_iobuf, iobuf_hi);

      iovecs[i].iov_base = get_bstr(Field(v_iobuf, iobuf_buf), v_lo);
      iovecs[i].iov_len = Long_val(v_hi) - Long_val(v_lo);
    }

    n_read = recvmmsg_assume_fd_is_nonblocking(v_fd, iovecs, count, v_srcs, hdrs);

    for (i = 0; (int) i < n_read; i++) {
      v_iobuf = Field(v_iobufs, i);
      v_lo = Field(v_iobuf, iobuf_lo);

      /* Knowing the structure of an Iobuf record (which we already
       * are dependent on), we can use Field(v_iobuf, iobuf_lo) as an
       * lvalue and skip the caml_modify done by Store_field.
       */
      Field(v_iobuf, iobuf_lo) = Val_long(Long_val(v_lo) + hdrs[i].msg_len);
    }
  }
  CAMLreturn(Val_int(n_read));
}

#endif  /* JSC_RECVMMSG */
