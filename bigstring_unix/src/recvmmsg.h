
#define _GNU_SOURCE             /* struct mmsghdr */

#include <sys/socket.h>

#include "ocaml_utils.h"

#ifdef JSC_RECVMMSG

/* [recvmmsg] from [v_fd] into the [v_count] supplied [iovecs].  Save
   the from addresses in [v_srcs], if supplied.  Use [hdrs], in
   particular, fully initializing and saving results there, including
   lengths of data read, flags, etc. */
int recvmmsg_assume_fd_is_nonblocking(
  value v_fd, struct iovec *iovecs, unsigned count, value v_srcs, struct mmsghdr *hdrs);

#define RECVMMSG_MAX_COUNT 64

#endif  /* JSC_RECVMMSG */
