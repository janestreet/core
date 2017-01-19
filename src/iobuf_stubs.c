#include "config.h"
#include "iobuf.h"
#include "unix_utils.h"
#include "socketaddr.h"

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/bigarray.h>

#include <errno.h>

#ifdef JSC_RECVMMSG

/* Using the Abstract_tag, expose a version of recvmmsg that allows
   for the reuse of the mmsghdr and associated iovecs. */

typedef struct _recvmmsg_ctx
{
  struct mmsghdr * hdrs;
  struct iovec * iovecs;
} recvmmsg_ctx;

#define Recvmmsg_ctx_ptr(v) ((recvmmsg_ctx *) Data_custom_val(v))

static void finalize_recvmmsg_ctx(value v)
{
  recvmmsg_ctx * ptr = Recvmmsg_ctx_ptr(v);
  if (ptr->hdrs) {
    free(ptr->hdrs);
    ptr->hdrs = NULL;
  }
  if (ptr->iovecs) {
    free(ptr->iovecs);
    ptr->iovecs = NULL;
  }
}

static struct custom_operations recvmmsg_ctx_custom_ops =
  {
    "_recvmmsg_ctx",
    finalize_recvmmsg_ctx,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default
  };

CAMLprim value iobuf_recvmmsg_ctx(value v_iobufs)
{
  CAMLparam1(v_iobufs);
  CAMLlocal4(v_iobuf, v_lo_min, v_hi_max, v_recvmmsg_ctx);
  struct iovec   * iovecs;
  struct mmsghdr * hdrs;
  unsigned i, count;

  count  = Wosize_val(v_iobufs);
  iovecs = (struct iovec   *) malloc(sizeof(struct iovec)   * count);
  hdrs   = (struct mmsghdr *) malloc(sizeof(struct mmsghdr) * count);

  for (i = 0; i<count; ++i) {
    v_iobuf = Field(v_iobufs, i);
    v_lo_min = Field(v_iobuf, iobuf_lo_min);
    v_hi_max = Field(v_iobuf, iobuf_hi_max);

    iovecs[i].iov_base = get_bstr(Field(v_iobuf, iobuf_buf), v_lo_min);
    iovecs[i].iov_len = Long_val(v_hi_max) - Long_val(v_lo_min);

    hdrs[i].msg_hdr.msg_name = 0;
    hdrs[i].msg_hdr.msg_namelen = 0;
    hdrs[i].msg_hdr.msg_iov = &iovecs[i];
    hdrs[i].msg_hdr.msg_iovlen = 1;

    hdrs[i].msg_hdr.msg_control = 0;
    hdrs[i].msg_hdr.msg_controllen = 0;
    hdrs[i].msg_hdr.msg_flags = 0;
    /* We completely ignore msg_flags and ancillary data (msg_control)
       for now.  In the future, users may be interested in this. */
  }

  v_recvmmsg_ctx =
    caml_alloc_custom(&recvmmsg_ctx_custom_ops, sizeof(recvmmsg_ctx), 0, 1);

  Recvmmsg_ctx_ptr(v_recvmmsg_ctx)->iovecs = iovecs;
  Recvmmsg_ctx_ptr(v_recvmmsg_ctx)->hdrs = hdrs;

  CAMLreturn(v_recvmmsg_ctx);
}

CAMLprim value
iobuf_recvmmsg_assume_fd_is_nonblocking_stub
(value v_fd, value v_iobufs, value v_recvmmsg_ctx)
{
  CAMLparam3(v_fd, v_iobufs, v_recvmmsg_ctx);
  CAMLlocal1(v_iobuf);
  value v_lo_min;
  unsigned i;
  int n_read;
  struct mmsghdr * hdrs;

  hdrs   = Recvmmsg_ctx_ptr(v_recvmmsg_ctx)->hdrs;
  n_read = recvmmsg(Int_val(v_fd), hdrs, Wosize_val(v_iobufs), 0, 0);

  for (i = 0; (int) i < n_read; i++) {
    v_iobuf = Field(v_iobufs, i);
    v_lo_min = Field(v_iobuf, iobuf_lo_min);

    /* Here we fail if the user has called set_bounds_and_buffer, replacing the underlying
     * bigstring after we've cached the pointer.
     */
    if (get_bstr(Field(v_iobuf, iobuf_buf), v_lo_min) !=
        hdrs[i].msg_hdr.msg_iov->iov_base) {
      n_read = -1;
      errno  = EINVAL;
    }
    else {
      /* Knowing the structure of an Iobuf record (which we already
       * are dependent on), we can use Field(v_iobuf, iobuf_lo) as an
       * lvalue and skip the caml_modify done by Store_field.
       */
      Field(v_iobuf, iobuf_lo) = v_lo_min;
      Field(v_iobuf, iobuf_hi) = Val_long(Long_val(v_lo_min) + hdrs[i].msg_len);
    }
  }
  if (n_read == -1)
    n_read = -errno;

  CAMLreturn(Val_int(n_read));
}

#endif  /* JSC_RECVMMSG */
