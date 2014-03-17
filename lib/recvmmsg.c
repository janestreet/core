#define _GNU_SOURCE             /* recvmmsg */

#include <stdio.h>
#include <errno.h>
#include <sys/socket.h>

#include "ocaml_utils.h"
#include "unix_utils.h"
#include "socketaddr.h"
#include "recvmmsg.h"

#ifdef JSC_RECVMMSG

ssize_t recvmmsg_assume_fd_is_nonblocking(
  value v_fd, struct iovec *iovecs, value v_count, value v_srcs, struct mmsghdr *hdrs)
{
  CAMLparam3(v_fd, v_count, v_srcs);
  CAMLlocal1(v_sockaddrs);
  size_t total_len = 0;
  union sock_addr_union addrs[Int_val(v_count)];
  int i;
  for (i = 0; i < Int_val(v_count); i++) {
    hdrs[i].msg_hdr.msg_name = (Is_block(v_srcs) ? &addrs[i].s_gen : 0);
    hdrs[i].msg_hdr.msg_namelen = (Is_block(v_srcs) ? sizeof(addrs[i]) : 0);

#if DEBUG
    fprintf(stderr, "i=%d, count=%d, is_some srcs=%d\n",
            i, Int_val(v_count), Is_block(v_srcs));
#endif
    total_len += iovecs[i].iov_len;

    hdrs[i].msg_hdr.msg_iov = &iovecs[i];
    hdrs[i].msg_hdr.msg_iovlen = 1;

    hdrs[i].msg_hdr.msg_control = 0;
    hdrs[i].msg_hdr.msg_controllen = 0;
    hdrs[i].msg_hdr.msg_flags = 0;
    /* We completely ignore msg_flags and ancillary data (msg_control)
       for now.  In the future, users may be interested in this. */
  }
  ssize_t n_read;
  /* pszilagyi: This is only 64k in unix_utils.h, which we will
     very quickly overrun with recvmmsg and then maybe Jumbo frames.
     bnigito has already observed the Pico feed filling over 32
     recvmmsg buffers in a single call, in a test scenario. */
  if (total_len > THREAD_IO_CUTOFF) {
    caml_enter_blocking_section();
      n_read = recvmmsg(Int_val(v_fd), hdrs, Int_val(v_count), 0, 0);
    caml_leave_blocking_section();
  }
  else n_read = recvmmsg(Int_val(v_fd), hdrs, Int_val(v_count), 0, 0);
  if (n_read == -1) {
    /* bnigito via pszilagyi: This prototype performance tweak saves
       the allocation of an exception in common cases, at the cost of
       conflating reception of an empty message with nothing to do. */
    if (errno == EWOULDBLOCK || errno == EAGAIN)
      n_read = -errno;
    else
      uerror("recvmmsg_assume_fd_is_nonblocking", Nothing);
  }
  else {
    if (Is_block(v_srcs)) {     /* Some */
      v_sockaddrs = Field(v_srcs, 0);
      for (i = 0; (unsigned)i < n_read && (unsigned)i < Wosize_val(v_sockaddrs); i++)
        Store_field(v_sockaddrs, i,
                    alloc_sockaddr(&addrs[i], hdrs[i].msg_hdr.msg_namelen, -1));
    }
  }
  CAMLreturnT(ssize_t, n_read);
}

#endif  /* JSC_RECVMMSG */
