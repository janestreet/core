/* WHEN YOU CHANGE THIS, CHANGE T.t in iobuf.ml AS WELL!!! */
enum iobuf_fields { buf, lo_min, lo, hi, hi_max };

#include "unix_utils.h"
#include "socketaddr.h"
#include "recvmmsg.h"

#ifdef JSC_RECVMMSG

CAMLprim value iobuf_recvmmsg_assume_fd_is_nonblocking_stub(
  value v_fd, value v_iobufs, value v_count, value v_srcs)
{
  CAMLparam4(v_fd, v_iobufs, v_count, v_srcs);
  CAMLlocal3(v_iobuf, v_lo, v_sockaddrs);
  struct mmsghdr hdrs[Int_val(v_count)];
  struct iovec iovecs[Int_val(v_count)];
  int i;
  for (i = 0; i < Int_val(v_count); i++) {
    v_iobuf = Field(v_iobufs, i);
    v_lo = Field(v_iobuf, lo);
    iovecs[i].iov_base = get_bstr(Field(v_iobuf, buf), v_lo);
    iovecs[i].iov_len = Int_val(Field(v_iobuf, hi)) - Int_val(v_lo);
  }
  ssize_t n_read = recvmmsg_assume_fd_is_nonblocking(v_fd, iovecs, v_count, v_srcs, hdrs);
  for (i = 0; i < n_read; i++) {
    v_iobuf = Field(v_iobufs, i);

    /* Knowing the structure of an Iobuf record (which we already are dependent on), we
     * can use Field(v_iobuf, lo) as an lvalue and skip the caml_modify done by Store_field
     */
    Field(v_iobuf, lo) = Val_int(Int_val(Field(v_iobuf, lo)) + hdrs[i].msg_len);
  }
  CAMLreturn(Val_int(n_read));
}

#endif  /* JSC_RECVMMSG */
