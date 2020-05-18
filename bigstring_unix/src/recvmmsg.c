#define _GNU_SOURCE             /* recvmmsg */

#include <stdio.h>
#include <errno.h>
#include <sys/socket.h>

#include "config.h"
#include "ocaml_utils.h"
#include "unix_utils.h"
#include "socketaddr.h"
#include "recvmmsg.h"

#ifdef JSC_RECVMMSG

int recvmmsg_assume_fd_is_nonblocking(
  value v_fd, struct iovec *iovecs, unsigned count, value v_srcs, struct mmsghdr *hdrs)
{
  CAMLparam2(v_fd, v_srcs);
  CAMLlocal1(v_sockaddrs);
  size_t total_len = 0;
  unsigned i;
  int n_read;
  int save_source_addresses;
  int fd;

  if ((int) count < 0) {
    caml_failwith("recvmmsg_assume_fd_is_nonblocking: apparently negative count");
  }

  {
    union sock_addr_union addrs[RECVMMSG_MAX_COUNT];

    save_source_addresses = Is_block(v_srcs);
    fd = Int_val(v_fd);

    if (count > RECVMMSG_MAX_COUNT) {
      caml_failwith("recvmmsg_assume_fd_is_nonblocking: "
                    "count exceeds RECVMMSG_MAX_COUNT");
    }
    for (i = 0; i < count; i++) {
      hdrs[i].msg_hdr.msg_name = (save_source_addresses ? &addrs[i].s_gen : 0);
      hdrs[i].msg_hdr.msg_namelen = (save_source_addresses ? sizeof(addrs[i]) : 0);

#if DEBUG
      fprintf(stderr, "i=%u, count=%u, save_source_addresses=%d\n",
              i, count, save_source_addresses);
#endif
      total_len += iovecs[i].iov_len;

      hdrs[i].msg_hdr.msg_iov = &iovecs[i];
      hdrs[i].msg_hdr.msg_iovlen = 1;

      hdrs[i].msg_hdr.msg_control = 0;
      hdrs[i].msg_hdr.msg_controllen = 0;
      hdrs[i].msg_hdr.msg_flags = 0;
      /* We completely ignore msg_flags and ancillary data
         (msg_control) for now.  In the future, users may be
         interested in this. */
    }

    /* This is only 64k in unix_utils.h, which we will very quickly
       overrun with recvmmsg and then maybe Jumbo frames.  We have
       already observed an application filling over 32 recvmmsg
       buffers in a single call, in a test scenario. */
    if (total_len > THREAD_IO_CUTOFF) {
      caml_enter_blocking_section();
      n_read = recvmmsg(fd, hdrs, count, 0, 0);
      caml_leave_blocking_section();
    }
    else {
      n_read = recvmmsg(fd, hdrs, count, 0, 0);
    }

    if (n_read > (int) count) {
      caml_failwith("recvmmsg_assume_fd_is_nonblocking: "
                    "recvmmsg unexpectedly returned n_read > count");
    }

    if (n_read == -1)
      n_read = -errno;
    else {
      if (save_source_addresses) {
        v_sockaddrs = Field(v_srcs, 0);
        if (!Is_block(v_sockaddrs)) {
          caml_invalid_argument("recvmmsg_assume_fd_is_nonblocking: "
                                "v_sockaddrs is not an array");
        }
        if (Wosize_val(v_sockaddrs) < count) {
          caml_invalid_argument("recvmmsg_assume_fd_is_nonblocking: "
                                "length v_sockaddrs < count");
        }

        for (i = 0; (int) i < n_read; i++) {
          value addr = alloc_sockaddr(&addrs[i], hdrs[i].msg_hdr.msg_namelen, -1);
          Store_field(v_sockaddrs, i, addr);
        }
      }
    }
  }

  CAMLreturnT(int, n_read);
}

#endif  /* JSC_RECVMMSG */
