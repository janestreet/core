#define _FILE_OFFSET_BITS 64

#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <netinet/in.h>
#include <assert.h>

#ifdef __GLIBC__
#include <malloc.h>
#endif

#include "ocaml_utils.h"
#include "unix_utils.h"
#include "socketaddr.h"

/* Initialisation */

static value *bigstring_exc_IOError = NULL;
static value *bigstring_exc_End_of_file = NULL;
static value *unix_error_exn = NULL;

CAMLprim value bigstring_init_stub(value __unused v_unit)
{
  bigstring_exc_IOError = caml_named_value("Bigstring.IOError");
  bigstring_exc_End_of_file = caml_named_value("Bigstring.End_of_file");
  unix_error_exn = caml_named_value("Unix.Unix_error");
#ifdef __GLIBC__
  /* GLIBC uses a threshold internally as a cutoff between brk and mmap.
     Sadly, it nowadays employs a heuristic that may change this value
     dynamically.  The call to mallopt suppresses this behavior, which
     made it hard to prevent C-heap fragmentation (e.g. in the writer).
  */
  mallopt(M_MMAP_THRESHOLD, 131072);
#endif
  if (unix_error_exn == NULL)
    caml_invalid_argument(
      "Exception Unix.Unix_error not initialized, please link unix.cma");
  return Val_unit;
}


/* Blitting */

CAMLprim value bigstring_blit_string_bigstring_stub(
  value v_str, value v_src_pos, value v_bstr, value v_dst_pos, value v_len)
{
  char *str = String_val(v_str) + Long_val(v_src_pos);
  char *bstr = get_bstr(v_bstr, v_dst_pos);
  memcpy(bstr, str, Long_val(v_len));
  return Val_unit;
}

CAMLprim value bigstring_blit_bigstring_string_stub(
  value v_bstr, value v_src_pos, value v_str, value v_dst_pos, value v_len)
{
  char *bstr = get_bstr(v_bstr, v_src_pos);
  char *str = String_val(v_str) + Long_val(v_dst_pos);
  memcpy(str, bstr, Long_val(v_len));
  return Val_unit;
}

CAMLprim value bigstring_blit_stub(
  value v_src, value v_src_pos, value v_dst, value v_dst_pos, value v_len)
{
  struct caml_ba_array *ba_src = Caml_ba_array_val(v_src);
  struct caml_ba_array *ba_dst = Caml_ba_array_val(v_dst);
  char *src = (char *) ba_src->data + Long_val(v_src_pos);
  char *dst = (char *) ba_dst->data + Long_val(v_dst_pos);
  size_t len = Long_val(v_len);
  if ((len > THREAD_IO_CUTOFF)
      || (ba_src->flags & CAML_BA_MAPPED_FILE)
      || (ba_dst->flags & CAML_BA_MAPPED_FILE))
  {
    Begin_roots2(v_src, v_dst);
    caml_enter_blocking_section();
      memmove(dst, src, Long_val(v_len));
    caml_leave_blocking_section();
    End_roots();
  }
  else memmove(dst, src, Long_val(v_len));
  return Val_unit;
}


/* Exceptions */

static inline void raise_io_error(value v_n_good, value v_exc)
{
  raise_with_two_args(*bigstring_exc_IOError, v_n_good, v_exc);
}

static inline value mk_unix_error_exn(int errcode, char *cmdname, value cmdarg)
{
  CAMLparam0();
  CAMLlocal3(name, err, arg);
  value res;
  arg = cmdarg == Nothing ? caml_copy_string("") : cmdarg;
  name = caml_copy_string(cmdname);
  err = unix_error_of_code(errcode);
  res = caml_alloc_small(4, 0);
  Field(res, 0) = *unix_error_exn;
  Field(res, 1) = err;
  Field(res, 2) = name;
  Field(res, 3) = arg;
  CAMLreturn(res);
}

static inline value mk_uerror_exn(char *cmdname, value cmdarg)
{
  return mk_unix_error_exn(errno, cmdname, cmdarg);
}

static inline void raise_unix_io_error(
  value v_n_good, char *cmdname, value cmdarg)
{
  value v_uerror = mk_uerror_exn(cmdname, cmdarg);
  raise_io_error(v_n_good, v_uerror);
}

static inline void raise_eof_io_error(value v_n_good)
{
  value v_eof_exn = caml_alloc_small(1, 0);
  Field(v_eof_exn, 0) = *bigstring_exc_End_of_file;
  raise_io_error(v_n_good, v_eof_exn);
}


/* Checking memory-mapping */

CAMLprim value bigstring_is_mmapped_stub(value v_bstr)
{
  return
    Val_bool((Caml_ba_array_val(v_bstr)->flags & CAML_BA_MAPPED_FILE) != 0);
}


/* Input of bigstrings from file descriptors */

CAMLprim value bigstring_read_stub(
  value v_min_len, value v_fd, value v_pos, value v_len, value v_bstr)
{
  CAMLparam1(v_bstr);
    size_t min_len = Long_val(v_min_len);
    int fd = Int_val(v_fd);
    size_t init_len = Long_val(v_len);
    size_t len = init_len;
    ssize_t n_read;
    char *bstr_start = get_bstr(v_bstr, v_pos);
    char *bstr = bstr_start;
    char *bstr_min = bstr_start + min_len;
    caml_enter_blocking_section();
      do {
        n_read = read(fd, bstr, len);
        if (n_read <= 0) {
          value v_n_good = Val_long(bstr - bstr_start);
          caml_leave_blocking_section();
          if (n_read == 0) {
            if (init_len == 0) CAMLreturn(Val_long(0));
            else raise_eof_io_error(v_n_good);
          }
          else raise_unix_io_error(v_n_good, "read", Nothing);
        } else {
          bstr += n_read;
          len -= n_read;
        }
      } while (bstr < bstr_min);
    caml_leave_blocking_section();
  CAMLreturn(Val_long(bstr - bstr_start));
}

CAMLprim value bigstring_read_assume_fd_is_nonblocking_stub(
  value v_fd, value v_pos, value v_len, value v_bstr)
{
  struct caml_ba_array *ba = Caml_ba_array_val(v_bstr);
  char *bstr = (char *) ba->data + Long_val(v_pos);
  size_t len = Long_val(v_len);
  ssize_t n_read;
  if ((len > THREAD_IO_CUTOFF) || (ba->flags & CAML_BA_MAPPED_FILE)) {
    Begin_roots1(v_bstr);
    caml_enter_blocking_section();
      n_read = read(Int_val(v_fd), bstr, len);
    caml_leave_blocking_section();
    End_roots();
  }
  else n_read = read(Int_val(v_fd), bstr, len);
  if (n_read == -1) uerror("bigstring_read_assume_fd_is_nonblocking", Nothing);
  return Val_long(n_read);
}


/* Input of bigstrings from sockets */

CAMLprim value bigstring_really_recv_stub(
  value v_sock, value v_pos, value v_len, value v_bstr)
{
  size_t len = Long_val(v_len);
  if (len == 0) return Val_unit;
  else {
    CAMLparam1(v_bstr);
      char *bstr = get_bstr(v_bstr, v_pos);
      int sock = Int_val(v_sock);
      ssize_t n_read;
      size_t n_total = 0;
      caml_enter_blocking_section();
        while (len > 0) {
          n_read = recv(sock, bstr, len, MSG_WAITALL);
          if (n_read <= 0) {
            value v_n_total = Val_long(n_total);
            caml_leave_blocking_section();
            if (n_read == 0) raise_eof_io_error(v_n_total);
            else raise_unix_io_error(v_n_total, "really_recv", Nothing);
          } else {
            len -= n_read;
            bstr += len;
            n_total += n_read;
          }
        }
      caml_leave_blocking_section();
    CAMLreturn(Val_unit);
  }
}

CAMLprim value bigstring_recvfrom_assume_fd_is_nonblocking_stub(
  value v_sock, value v_pos, value v_len, value v_bstr)
{
  CAMLparam1(v_bstr);
  CAMLlocal1(v_addr);
  struct caml_ba_array *ba = Caml_ba_array_val(v_bstr);
  char *bstr = (char *) ba->data + Long_val(v_pos);
  size_t len = Long_val(v_len);
  ssize_t n_read;
  union sock_addr_union addr;
  socklen_param_type addr_len = sizeof(addr);
  value v_res;
  if (len > THREAD_IO_CUTOFF) {
    caml_enter_blocking_section();
      n_read = recvfrom(Int_val(v_sock), bstr, len, 0, &addr.s_gen, &addr_len);
    caml_leave_blocking_section();
  }
  else n_read = recvfrom(Int_val(v_sock), bstr, len, 0, &addr.s_gen, &addr_len);
  if (n_read == -1)
    uerror("bigstring_recvfrom_assume_fd_is_nonblocking", Nothing);
  v_addr = alloc_sockaddr(&addr, addr_len, -1);
  v_res = caml_alloc_small(2, 0);
  Field(v_res, 0) = Val_long(n_read);
  Field(v_res, 1) = v_addr;
  CAMLreturn(v_res);
}


/* I/O of bigstrings from channels */

typedef off_t file_offset;

#define IO_BUFFER_SIZE 65536

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

CAMLextern void (*caml_channel_mutex_lock) (struct channel *);
CAMLextern void (*caml_channel_mutex_unlock) (struct channel *);

#define Channel(v) (*((struct channel **) (Data_custom_val(v))))

#define Lock(channel) \
    if (caml_channel_mutex_lock != NULL) (*caml_channel_mutex_lock)(channel)

#define Unlock(channel) \
    if (caml_channel_mutex_unlock != NULL) (*caml_channel_mutex_unlock)(channel)

CAMLprim value bigstring_input_stub(
  value v_min_len, value v_chan, value v_pos, value v_len, value v_bstr)
{
  CAMLparam2(v_chan, v_bstr);
    struct channel *chan = Channel(v_chan);
    char *bstr_start = get_bstr(v_bstr, v_pos);
    char *bstr = bstr_start;
    size_t init_bstr_len = Long_val(v_len);
    size_t bstr_len = init_bstr_len;
    size_t min_len = Long_val(v_min_len);
    size_t avail = chan->max - chan->curr;
    Lock(chan);
    if (avail) {
      int got_all = bstr_len <= avail;
      size_t to_write = got_all ? bstr_len : avail;
      memcpy(bstr, chan->curr, to_write);
      if (got_all) {
        chan->curr += to_write;
        Unlock(chan);
        CAMLreturn(v_len);
      }
      else {
        bstr += to_write;
        bstr_len -= to_write;
        min_len -= to_write;
      }
    }
    /* Buffer empty now */
    {
      int fd = chan->fd;
      char *bstr_min = bstr + min_len;
      char *bstr_max = bstr + bstr_len;
      struct iovec iovecs[2];
      struct iovec *bstr_iov = &iovecs[0];
      struct iovec *buff_iov = &iovecs[1];
      ssize_t n_read;
      bstr_iov->iov_base = bstr;
      bstr_iov->iov_len = bstr_len;
      buff_iov->iov_base = chan->buff;
      buff_iov->iov_len = chan->end - chan->buff;
      caml_enter_blocking_section();
      while (1) {
        n_read = readv(fd, iovecs, 2);
        if (n_read <= 0) {
          if (n_read == -1) {
            /* Interrupt and error handling */
            if (errno == EINTR) continue;
            else {
              value v_n_good =
                Val_long((char *) bstr_iov->iov_base - bstr_start);
              /* Set buffer to empty as required */
              chan->curr = chan->max;
              caml_leave_blocking_section();
              Unlock(chan);
              raise_unix_io_error(v_n_good, "input", Nothing);
            }
          }
          else {
            /* Zero-read: set buffer to empty as required */
            assert(n_read == 0);
            chan->curr = chan->max;
            if (init_bstr_len == 0) {
              caml_leave_blocking_section();
              Unlock(chan);
              CAMLreturn(Val_long(0));
            } else {
              /* EOF handling */
              value v_n_good =
                Val_long((char *) bstr_iov->iov_base - bstr_start);
              caml_leave_blocking_section();
              Unlock(chan);
              raise_eof_io_error(v_n_good);
            }
          }
        } else {
          /* Successful read */
          chan->offset += n_read;
          bstr = (char *) bstr_iov->iov_base + n_read;
          if (bstr >= bstr_min) {
            /* Sufficient data read */
            if (bstr > bstr_max) {
              /* Buffer contains extra data */
              chan->max = &chan->buff[bstr - bstr_max];
              chan->curr = chan->buff;
              caml_leave_blocking_section();
              Unlock(chan);
              CAMLreturn(v_len);
            } else {
              /* Buffer empty; set it so */
              chan->curr = chan->max;
              caml_leave_blocking_section();
              Unlock(chan);
              CAMLreturn(Val_long(bstr - bstr_start));
            }
          } else {
            /* Insufficient data */
            bstr_iov->iov_base = bstr;
            bstr_iov->iov_len -= n_read;
          }
        }
      }
    }
}

CAMLprim value bigstring_output_stub(
  value v_min_len, value v_chan, value v_pos, value v_len, value v_bstr)
{
  CAMLparam2(v_chan, v_bstr);
    struct channel *chan = Channel(v_chan);
    char *bstr = get_bstr(v_bstr, v_pos);
    size_t bstr_len = Long_val(v_len);
    Lock(chan);
    if (bstr_len <= (size_t) (chan->end - chan->curr)) {
      /* Buffer can store all data */
      memcpy(chan->curr, bstr, bstr_len);
      chan->curr += bstr_len;
      Unlock(chan);
      CAMLreturn(v_len);
    } else {
      /* Buffer cannot store all data */
      int fd = chan->fd;
      size_t buff_len = chan->curr - chan->buff;
      char *bstr_min = bstr + Long_val(v_min_len);
      struct iovec iovecs[2];
      struct iovec *buff_iov = &iovecs[0];
      struct iovec *bstr_iov = &iovecs[1];
      ssize_t written;
      buff_iov->iov_base = chan->buff;
      buff_iov->iov_len = buff_len;
      bstr_iov->iov_base = bstr;
      bstr_iov->iov_len = bstr_len;
      caml_enter_blocking_section();
      while (1) {
        written = writev(fd, iovecs, 2);
        if (written == -1) {
          /* Interrupt and error handling */
          if (errno == EINTR) continue;
          if ((errno == EAGAIN || errno == EWOULDBLOCK) &&
              buff_iov->iov_len + bstr_iov->iov_len > 1) {
            /* Call might have blocked, try writing a single byte */
            if (buff_len) {
              buff_iov->iov_len = 1;
              bstr_iov->iov_len = 0;
            } else bstr_iov->iov_len = 1;
            continue;
          } else {
            /* Write (maybe of even one byte only) failed */
            value v_n_good = Val_long((char *) bstr_iov->iov_base - bstr);
            chan->curr = chan->buff + buff_len;
            if (buff_len) memmove(chan->buff, buff_iov->iov_base, buff_len);
            caml_leave_blocking_section();
            Unlock(chan);
            raise_unix_io_error(v_n_good, "output", Nothing);
          }
        } else {
          /* Write successful */
          chan->offset += written;
          if (buff_len > (size_t) written) {
            /* Buffer was partially written only; continue */
            buff_iov->iov_base = (char *) buff_iov->iov_base + written;
            buff_len -= written;
            buff_iov->iov_len = buff_len;
          } else {
            /* Buffer is empty now */
            size_t bstr_written = written - buff_len;
            char *new_bstr = (char *) bstr_iov->iov_base + bstr_written;
            if (new_bstr >= bstr_min) {
              /* Sufficient data was sent */
              chan->curr = chan->buff;
              caml_leave_blocking_section();
              Unlock(chan);
              CAMLreturn(Val_long(new_bstr - bstr));
            } else {
              /* Not yet done */
              bstr_iov->iov_base = new_bstr;
              buff_len = 0;
              buff_iov->iov_len = buff_len;
              bstr_len -= bstr_written;
              bstr_iov->iov_len = bstr_len;
            }
          }
        }
      }
    }
}


/* Output macros and functions */

#define MakeReallyOutputFun(NAME, CALL_WRITE) \
  CAMLprim value bigstring_really_##NAME##_stub( \
    value v_fd, value v_pos, value v_len, value v_bstr) \
  { \
    CAMLparam1(v_bstr); \
      int fd = Int_val(v_fd); \
      size_t len = Long_val(v_len); \
      ssize_t written; \
      char *bstr_start = get_bstr(v_bstr, v_pos); \
      char *bstr = bstr_start; \
      char *bstr_max = bstr + len; \
      caml_enter_blocking_section(); \
        do { \
          CALL_WRITE; \
          if (written == -1) { \
            value v_n_good = Val_long(bstr - bstr_start); \
            caml_leave_blocking_section(); \
            raise_unix_io_error(v_n_good, STR(really_##NAME), Nothing); \
          }; \
          len -= written; \
          bstr += written; \
        } while (bstr < bstr_max); \
      caml_leave_blocking_section(); \
    CAMLreturn(Val_unit); \
  }

MakeReallyOutputFun(write, written = write(fd, bstr, len))

CAMLprim value bigstring_write_stub(
  value v_fd, value v_pos, value v_len, value v_bstr)
{
  CAMLparam1(v_bstr);
  char *bstr = get_bstr(v_bstr, v_pos);
  size_t len = Long_val(v_len);
  ssize_t written;
  caml_enter_blocking_section();
    written = write(Int_val(v_fd), bstr, len);
  caml_leave_blocking_section();
  if (written == -1) uerror("write", Nothing);
  CAMLreturn(Val_long(written));
}

CAMLprim value bigstring_write_assume_fd_is_nonblocking_stub(
  value v_fd, value v_pos, value v_len, value v_bstr)
{
  struct caml_ba_array *ba = Caml_ba_array_val(v_bstr);
  char *bstr = (char *) ba->data + Long_val(v_pos);
  size_t len = Long_val(v_len);
  ssize_t written;
  if ((len > THREAD_IO_CUTOFF) || (ba->flags & CAML_BA_MAPPED_FILE)) {
    Begin_roots1(v_bstr);
    caml_enter_blocking_section();
      written = write(Int_val(v_fd), bstr, len);
    caml_leave_blocking_section();
    End_roots();
  }
  else written = write(Int_val(v_fd), bstr, len);
  if (written == -1) uerror("write_assume_fd_is_nonblocking", Nothing);
  return Val_long(written);
}

static inline ssize_t writev_in_blocking_section(
  value v_fd, value v_iovecs, struct iovec *iovecs, int count)
{
  ssize_t ret;
  CAMLparam1(v_iovecs);  /* To protect bigstrings outside of OCaml lock */
  caml_enter_blocking_section();
    ret = writev(Int_val(v_fd), iovecs, count);
    free(iovecs);
  caml_leave_blocking_section();
  CAMLreturn(ret);
}

CAMLprim value bigstring_writev_stub(value v_fd, value v_iovecs, value v_count)
{
  int count = Int_val(v_count);
  size_t total_len = 0;
  struct iovec *iovecs = copy_iovecs(&total_len, v_iovecs, count);
  ssize_t ret = writev_in_blocking_section(v_fd, v_iovecs, iovecs, count);
  if (ret == -1) uerror("writev", Nothing);
  return Val_long(ret);
}

__pure static inline int contains_mmapped(value v_iovecs, int n)
{
  for (--n; n >= 0; --n) {
    value v_iovec = Field(v_iovecs, n);
    int flags = Caml_ba_array_val(Field(v_iovec, 0))->flags;
    if (unlikely(flags & CAML_BA_MAPPED_FILE)) return 1;
  }
  return 0;
}

CAMLprim value bigstring_writev_assume_fd_is_nonblocking_stub(
  value v_fd, value v_iovecs, value v_count)
{
  int count = Int_val(v_count);
  size_t total_len = 0;
  struct iovec *iovecs = copy_iovecs(&total_len, v_iovecs, count);
  ssize_t ret;
  if (total_len > THREAD_IO_CUTOFF || contains_mmapped(v_iovecs, count))
    /* NOTE: writev_in_blocking_section frees iovecs */
    ret = writev_in_blocking_section(v_fd, v_iovecs, iovecs, count);
  else {
    ret = writev(Int_val(v_fd), iovecs, count);
    free(iovecs);
  }
  if (ret == -1) uerror("writev_assume_fd_is_nonblocking", Nothing);
  return Val_long(ret);
}

#ifdef MSG_NOSIGNAL
MakeReallyOutputFun(send_no_sigpipe,
                    written = send(fd, bstr, len, MSG_NOSIGNAL))

static int nonblocking_no_sigpipe_flag = MSG_DONTWAIT | MSG_NOSIGNAL;

CAMLprim value bigstring_send_nonblocking_no_sigpipe_stub(
  value v_fd, value v_pos, value v_len, value v_bstr)
{
  char *bstr = get_bstr(v_bstr, v_pos);
  ssize_t ret =
    send(Int_val(v_fd), bstr, Long_val(v_len), nonblocking_no_sigpipe_flag);
  if (ret == -1 && errno != EAGAIN && errno != EWOULDBLOCK)
    uerror("send_nonblocking_no_sigpipe", Nothing);
  return Val_long(ret);
}

CAMLprim value bigstring_sendto_nonblocking_no_sigpipe_stub(
  value v_fd, value v_pos, value v_len, value v_bstr, value v_addr)
{
  char *bstr = get_bstr(v_bstr, v_pos);
  union sock_addr_union addr;
  socklen_param_type addr_len = sizeof(addr);
  ssize_t ret;
  get_sockaddr(v_addr, &addr, &addr_len);
  ret =
    sendto(
      Int_val(v_fd), bstr, Long_val(v_len),
      nonblocking_no_sigpipe_flag, &addr.s_gen, addr_len);
  if (ret == -1 && errno != EAGAIN && errno != EWOULDBLOCK)
    uerror("sendto_nonblocking_no_sigpipe", Nothing);
  return Val_long(ret);
}

CAMLprim value bigstring_sendmsg_nonblocking_no_sigpipe_stub(
  value v_fd, value v_iovecs, value v_count)
{
  int count = Int_val(v_count);
  size_t total_len = 0;
  struct iovec *iovecs = copy_iovecs(&total_len, v_iovecs, count);
  struct msghdr msghdr = { NULL, 0, NULL, 0, NULL, 0, 0 };
  ssize_t ret;
  if (total_len > THREAD_IO_CUTOFF || contains_mmapped(v_iovecs, count)) {
    Begin_roots1(v_iovecs);
    caml_enter_blocking_section();
      msghdr.msg_iov = iovecs;
      msghdr.msg_iovlen = count;
      ret = sendmsg(Int_val(v_fd), &msghdr, nonblocking_no_sigpipe_flag);
      free(iovecs);
    caml_leave_blocking_section();
    End_roots();
  } else {
    msghdr.msg_iov = iovecs;
    msghdr.msg_iovlen = count;
    ret = sendmsg(Int_val(v_fd), &msghdr, nonblocking_no_sigpipe_flag);
    free(iovecs);
  }
  if (ret == -1 && errno != EAGAIN && errno != EWOULDBLOCK)
    uerror("sendmsg_nonblocking_no_sigpipe", Nothing);
  return Val_long(ret);
}
#else
#warning "MSG_NOSIGNAL not defined; bigstring_send{,msg}_noblocking_no_sigpipe not implemented"
#warning "Try compiling on Linux?"
#endif

/* Search */

CAMLprim value bigstring_find(value v_str, value v_needle,
                              value v_pos, value v_len)
{
  char *start, *r;
  long ret;

  start = get_bstr(v_str, v_pos);
  r = (char*) memchr(start, Int_val(v_needle), Long_val(v_len));

  if (!r) return Val_long(-1);

  ret = Long_val(v_pos) + r - start;
  return Val_long(ret);
}

/* Destruction */

static void check_bigstring_proxy(struct caml_ba_array *b)
{
  if (b->proxy != NULL) caml_failwith("bigstring_destroy: bigstring has proxy");
}

extern void caml_ba_unmap_file(void *addr, uintnat len);

CAMLprim value bigstring_destroy_stub(value v_bstr)
{
  struct caml_ba_array *b = Caml_ba_array_val(v_bstr);
  int i;
  switch (b->flags & CAML_BA_MANAGED_MASK) {
    case CAML_BA_EXTERNAL :
      caml_failwith(
        "bigstring_destroy: bigstring is external or already deallocated");
      break;
    case CAML_BA_MANAGED :
      check_bigstring_proxy(b);
      free(b->data);
      break;
    case CAML_BA_MAPPED_FILE :
      check_bigstring_proxy(b);
      caml_ba_unmap_file(b->data, caml_ba_byte_size(b));
      break;
  }
  b->data = NULL;
  b->flags = CAML_BA_EXTERNAL;
  for (i = 0; i < b->num_dims; ++i) b->dim[i] = 0;
  return Val_unit;
}
