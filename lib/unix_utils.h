#ifndef UNIX_UTILS_H
#define UNIX_UTILS_H

#define _GNU_SOURCE

#include <sys/uio.h>
#include "ocaml_utils.h"

/* Utility definitions */

static inline char * get_bstr(value v_bstr, value v_pos)
{
  return (char *) Caml_ba_data_val(v_bstr) + Long_val(v_pos);
}

static inline struct iovec * copy_iovecs(size_t *total_len, value v_iovecs, int n)
{
  struct iovec *iovecs = caml_stat_alloc(sizeof(struct iovec) * n);
  for (--n; n >= 0; --n) {
    struct iovec *iovec = &iovecs[n];
    value v_iovec = Field(v_iovecs, n);
    value v_iov_base = Field(v_iovec, 0);
    value v_iov_pos = Field(v_iovec, 1);
    size_t iov_len = Long_val(Field(v_iovec, 2));
    iovec->iov_len = iov_len;
    *total_len += iov_len;
    iovec->iov_base = get_bstr(v_iov_base, v_iov_pos);
  }
  return iovecs;
}

/* I/O transaction size after which to release the OCaml-lock */
#define THREAD_IO_CUTOFF 65536

#endif /* UNIX_UTILS_H */
