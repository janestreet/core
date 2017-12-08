/* Core_unix support functions written in C. */

#define _GNU_SOURCE

#include <string.h>
#include <pthread.h>
/* Darwin needs this to be included before if.h*/
#if defined(__APPLE__)
#define _POSIX_SOURCE
#include <sys/socket.h>
#elif defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__)
#include <sys/socket.h>
#endif
#include <sys/uio.h>
#include <sys/utsname.h>
#include <sys/file.h>
#include <pwd.h>
#include <dirent.h>
#include <errno.h>
#include <limits.h>
#include <net/if.h>
#include <netinet/in.h>
#include <signal.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <sys/resource.h>
#include <grp.h>
#include <sys/select.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fnmatch.h>
#include <stdio.h>
#include <assert.h>
#include <time.h>
#include <unistd.h>
#include <sched.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <math.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <ifaddrs.h>

#if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__)
#define stat64 stat
#define lstat64 lstat
#define fstat64 fstat
#endif

#include "ocaml_utils.h"
#include "unix_utils.h"
#include "config.h"
#include "timespec.h"
#include "thread_id.h"

#if defined(JSC_WORDEXP)
#include <wordexp.h>
#endif

CAMLprim value core_unix_error_of_code(value code)
{
  return unix_error_of_code(Int_val(code));
}

CAMLprim value core_code_of_unix_error(value error)
{
  return Val_int(code_of_unix_error(error));
}

CAMLprim value unix_error_stub(value v_errcode, value v_cmdname, value cmd_arg)
{
  unix_error(Int_val(v_errcode), String_val(v_cmdname), cmd_arg);
  return Val_unit;
}

#define UNIX_INT63_CONST(CONST) DEFINE_INT63_CONSTANT(unix_##CONST,CONST)

UNIX_INT63_CONST(F_GETFL)
UNIX_INT63_CONST(F_SETFL)

UNIX_INT63_CONST(O_APPEND)
UNIX_INT63_CONST(O_ASYNC)
#ifndef O_CLOEXEC
#define O_CLOEXEC 0
#endif
UNIX_INT63_CONST(O_CLOEXEC)
UNIX_INT63_CONST(O_CREAT)
#ifndef O_DIRECT
#define O_DIRECT 0
#endif
UNIX_INT63_CONST(O_DIRECT)
#ifndef O_DIRECTORY
#define O_DIRECTORY 0
#endif
UNIX_INT63_CONST(O_DIRECTORY)
#ifndef O_DSYNC
#define O_DSYNC 0
#endif
UNIX_INT63_CONST(O_DSYNC)
UNIX_INT63_CONST(O_EXCL)
#ifndef O_NOATIME
#define O_NOATIME 0
#endif
UNIX_INT63_CONST(O_NOATIME)
UNIX_INT63_CONST(O_NOCTTY)
UNIX_INT63_CONST(O_NOFOLLOW)
UNIX_INT63_CONST(O_NONBLOCK)
UNIX_INT63_CONST(O_RDONLY)
UNIX_INT63_CONST(O_RDWR)
#ifndef O_RSYNC
#define O_RSYNC 0
#endif
UNIX_INT63_CONST(O_RSYNC)
UNIX_INT63_CONST(O_SYNC)
UNIX_INT63_CONST(O_TRUNC)
UNIX_INT63_CONST(O_WRONLY)

CAMLprim value unix_fcntl (value fd, value v_cmd, value v_arg) {
  int63 result;
  int cmd  = Int63_val(v_cmd); /* extract before blocking section */
  long arg = Int63_val(v_arg); /* extract before blocking section */
  caml_enter_blocking_section();
  result = fcntl(Int_val(fd), cmd, arg);
  caml_leave_blocking_section();
  if (result == -1) uerror("unix_fcntl", Nothing);
  return caml_alloc_int63(result);
}

int core_unix_close_durably(int fd)
{
  int ret;
  do ret = close(fd);
  while (ret == -1 && errno == EINTR);
  return ret;
}


void close_on_exec(int fd)
{
  int flags;

  flags = fcntl(fd, F_GETFD);
  if (flags == -1) {
    unix_error(errno, "close_on_exec: unable to get flags for descr", Nothing);
  };

  flags |= FD_CLOEXEC;
  if (fcntl(fd, F_SETFD, flags) == -1) {
    unix_error(errno, "close_on_exec: unable to set flags for descr", Nothing);
  };
}


/* Replacement for broken stat functions */

#define Val_file_offset(fofs) caml_copy_int64(fofs)

static inline char * core_copy_to_c_string(value v_str)
{
  asize_t len = caml_string_length(v_str) + 1;
  char *p = caml_stat_alloc(len);
  memcpy(p, String_val(v_str), len);
  return p;
}

CAMLprim value core_setpwent(value v_unit)
{
  CAMLparam1(v_unit);
  caml_enter_blocking_section();
  setpwent();
  caml_leave_blocking_section();
  CAMLreturn(Val_unit);
}

CAMLprim value core_endpwent(value v_unit)
{
  CAMLparam1(v_unit);
  caml_enter_blocking_section();
  endpwent();
  caml_leave_blocking_section();
  CAMLreturn(Val_unit);
}

CAMLprim value core_getpwent(value v_unit)
{
  CAMLparam1(v_unit);
  CAMLlocal1(res);
  struct passwd *entry;

  caml_enter_blocking_section();
  errno = 0;
  entry = getpwent();
  caml_leave_blocking_section();

  if (entry == NULL) {
    if (errno == 0)
      caml_raise_end_of_file();
    else
      unix_error(errno, "getpwent", Nothing);
  }

  res = caml_alloc_tuple(7);
  Store_field(res, 0, caml_copy_string(entry->pw_name));
  Store_field(res, 1, caml_copy_string(entry->pw_passwd));
  Store_field(res, 2, Val_int(entry->pw_uid));
  Store_field(res, 3, Val_int(entry->pw_gid));
  Store_field(res, 4, caml_copy_string(entry->pw_gecos));
  Store_field(res, 5, caml_copy_string(entry->pw_dir));
  Store_field(res, 6, caml_copy_string(entry->pw_shell));

  CAMLreturn(res);
}

#define FLOCK_BUF_LENGTH 80

CAMLprim value core_unix_flock(value v_fd, value v_lock_type)
{
  CAMLparam2(v_fd, v_lock_type);
  int fd = Int_val(v_fd);
  int lock_type = Int_val(v_lock_type);
  int operation;
  int res;
  char error[FLOCK_BUF_LENGTH];

  /* The [lock_type] values are defined in core_unix.ml. */
  switch(lock_type) {
    case 0:
      operation = LOCK_SH;
      break;
    case 1:
      operation = LOCK_EX;
      break;
    case 2:
      operation = LOCK_UN;
      break;
    default:
      snprintf(error, FLOCK_BUF_LENGTH,
               "bug in flock C stub: unknown lock type: %d", lock_type);
      caml_invalid_argument(error);
  };

  /* always try a non-blocking lock */
  operation = operation | LOCK_NB;

  caml_enter_blocking_section();
  res = flock(fd, operation);
  caml_leave_blocking_section();

  if (res) {
    switch(errno) {
      case EWOULDBLOCK:
        CAMLreturn(Val_false);
      default:
        unix_error(errno, "core_unix_flock", Nothing);
    };
  };

  CAMLreturn(Val_true);
}

/* Filesystem functions */

CAMLprim value unix_mknod_stub(
  value v_pathname, value v_mode, value v_perm, value v_major, value v_minor)
{
  CAMLparam1(v_pathname);

  int ret, len;
  char *pathname;
  mode_t mode = Int_val(v_perm);
  dev_t dev = 0;

  switch (Int_val(v_mode)) {
    case 0 : mode |= S_IFREG; break;
    case 2 :
      mode |= S_IFCHR;
      dev = makedev(Int_val(v_major), Int_val(v_minor));
      break;
    case 3 :
      mode |= S_IFBLK;
      dev = makedev(Int_val(v_major), Int_val(v_minor));
      break;
    case 5 : mode |= S_IFIFO; break;
    case 6 : mode |= S_IFSOCK; break;
    default : caml_invalid_argument("mknod");
  }

  len = caml_string_length(v_pathname) + 1;
  pathname = caml_stat_alloc(len);
  memcpy(pathname, String_val(v_pathname), len);

  caml_enter_blocking_section();
    ret = mknod(pathname, mode, dev);
    caml_stat_free(pathname);
  caml_leave_blocking_section();

  if (ret == -1) uerror("mknod", v_pathname);

  CAMLreturn(Val_unit);
}


/* I/O functions */

typedef struct dirent directory_entry;

CAMLprim value unix_sync(value v_unit)
{
  caml_enter_blocking_section();
    sync();
  caml_leave_blocking_section();
  return v_unit;
}

CAMLprim value unix_fsync(value v_fd)
{
  int ret;
  caml_enter_blocking_section();
    ret = fsync(Int_val(v_fd));
  caml_leave_blocking_section();
  if (ret == -1) uerror("fsync", Nothing);
  return Val_unit;
}

#if defined(_POSIX_SYNCHRONIZED_IO) && (_POSIX_SYNCHRONIZED_IO > 0)
CAMLprim value unix_fdatasync(value v_fd)
{
  int ret;
  caml_enter_blocking_section();
    ret = fdatasync(Int_val(v_fd));
  caml_leave_blocking_section();
  if (ret == -1) uerror("fdatasync", Nothing);
  return Val_unit;
}
#else
#warning "_POSIX_SYNCHRONIZED_IO undefined or <= 0; aliasing unix_fdatasync to unix_fsync"
CAMLprim value unix_fdatasync(value v_fd)
{
  return unix_fsync(v_fd);
}
#endif

CAMLprim value unix_dirfd(value v_dh)
{
  int ret = 0;
  if (DIR_Val(v_dh) == NULL)
    caml_invalid_argument("dirfd: NULL directory handle (probably closed)");
  ret = dirfd(DIR_Val(v_dh));
  if (ret == -1) uerror("dirfd", Nothing);
  return Val_int(ret);
}

CAMLprim value unix_readdir_ino_stub(value v_dh)
{
  DIR *d;
  directory_entry * e;
  d = DIR_Val(v_dh);
  if (d == (DIR *) NULL) unix_error(EBADF, "readdir_ino", Nothing);
  caml_enter_blocking_section();
    e = readdir((DIR *) d);
  caml_leave_blocking_section();
  if (e == (directory_entry *) NULL) caml_raise_end_of_file();
  else {
    CAMLparam0();
    CAMLlocal2(v_name, v_ino);
    value v_res;
    v_name = caml_copy_string(e->d_name);
    v_ino = caml_copy_nativeint(e->d_ino);
    v_res = caml_alloc_small(2, 0);
    Field(v_res, 0) = v_name;
    Field(v_res, 1) = v_ino;
    CAMLreturn(v_res);
  }
}

CAMLprim value unix_read_assume_fd_is_nonblocking_stub(
  value v_fd, value v_buf, value v_pos, value v_len)
{
  char *buf = String_val(v_buf) + Long_val(v_pos);
  ssize_t ret = read(Int_val(v_fd), buf, Long_val(v_len));
  if (ret == -1) uerror("unix_read_assume_fd_is_nonblocking", Nothing);
  return Val_long(ret);
}

CAMLprim value unix_write_assume_fd_is_nonblocking_stub(
  value v_fd, value v_buf, value v_pos, value v_len)
{
  char *buf = String_val(v_buf) + Long_val(v_pos);
  ssize_t ret = write(Int_val(v_fd), buf, Long_val(v_len));
  if (ret == -1) uerror("unix_write_assume_fd_is_nonblocking", Nothing);
  return Val_long(ret);
}

CAMLprim value unix_writev_assume_fd_is_nonblocking_stub(
  value v_fd, value v_iovecs, value v_count)
{
  int count = Int_val(v_count);
  ssize_t ret;
  struct iovec *iovecs = caml_stat_alloc(sizeof(struct iovec) * count);
  int i = count - 1;
  for (; i >= 0; --i) {
    struct iovec *iovec = &iovecs[i];
    value v_iovec = Field(v_iovecs, i);
    value v_iov_base = Field(v_iovec, 0);
    value v_iov_pos = Field(v_iovec, 1);
    value v_iov_len = Field(v_iovec, 2);
    iovec->iov_base = String_val(v_iov_base) + Long_val(v_iov_pos);
    iovec->iov_len = Long_val(v_iov_len);
  }
  ret = jane_writev(Int_val(v_fd), iovecs, count);
  caml_stat_free(iovecs);
  if (ret == -1) uerror("unix_writev_assume_fd_is_nonblocking", Nothing);
  return Val_long(ret);
}

CAMLprim value unix_writev_stub(value v_fd, value v_iovecs, value v_count)
{
  int i, count = Int_val(v_count), len = 0;
  ssize_t ret;
  char *buf, *dst;
  for (i = count - 1; i >= 0; --i) {
    value v_iovec = Field(v_iovecs, i);
    value v_iov_len = Field(v_iovec, 2);
    len += Long_val(v_iov_len);
  }
  buf = caml_stat_alloc(len);
  dst = buf + len;
  for (i = count - 1; i >= 0; --i) {
    value v_iovec = Field(v_iovecs, i);
    value v_iov_base = Field(v_iovec, 0);
    value v_iov_pos = Field(v_iovec, 1);
    value v_iov_len = Field(v_iovec, 2);
    size_t iov_len = Long_val(v_iov_len);
    dst -= iov_len;
    /* We need to copy all the strings because as soon as we release
       the lock the GC may move them */
    memcpy(dst, String_val(v_iov_base) + Long_val(v_iov_pos), iov_len);
  }
  caml_enter_blocking_section();
    ret = write(Int_val(v_fd), buf, len);
    caml_stat_free(buf);
  caml_leave_blocking_section();
  if (ret == -1) uerror("unix_writev", Nothing);
  return Val_long(ret);
}


/* pselect */

typedef fd_set file_descr_set;

static inline void fdlist_to_fdset(value fdlist, fd_set *fdset, int *maxfd)
{
  value l;
  FD_ZERO(fdset);
  for (l = fdlist; l != Val_int(0); l = Field(l, 1)) {
    int fd = Int_val(Field(l, 0));
    FD_SET(fd, fdset);
    if (fd > *maxfd) *maxfd = fd;
  }
}

static inline value fdset_to_fdlist(value fdlist, fd_set *fdset)
{
  value l;
  value res = Val_int(0);

  Begin_roots2(l, res);
    for (l = fdlist; l != Val_int(0); l = Field(l, 1)) {
      int fd = Int_val(Field(l, 0));
      if (FD_ISSET(fd, fdset)) {
        value newres = caml_alloc_small(2, 0);
        Field(newres, 0) = Val_int(fd);
        Field(newres, 1) = res;
        res = newres;
      }
    }
  End_roots();
  return res;
}

static inline void decode_sigset(value vset, sigset_t * set)
{
  sigemptyset(set);
  while (vset != Val_int(0)) {
    int sig = caml_convert_signal_number(Int_val(Field(vset, 0)));
    sigaddset(set, sig);
    vset = Field(vset, 1);
  }
}

CAMLprim value unix_pselect_stub(
  value v_rfds, value v_wfds, value v_efds, value v_timeout, value v_sigmask)
{
  fd_set rfds, wfds, efds;
  double tm = Double_val(v_timeout);
  struct timespec ts;
  struct timespec *tsp;
  int maxfd = -1, ret;
  value v_res;
  sigset_t sigmask;

  decode_sigset(v_sigmask, &sigmask);

  Begin_roots3(v_rfds, v_wfds, v_efds);
    fdlist_to_fdset(v_rfds, &rfds, &maxfd);
    fdlist_to_fdset(v_wfds, &wfds, &maxfd);
    fdlist_to_fdset(v_efds, &efds, &maxfd);

    if (tm < 0.0) tsp = (struct timespec *) NULL;
    else {
      ts = timespec_of_double(tm);
      tsp = &ts;
    }

    caml_enter_blocking_section();
      ret = pselect(maxfd + 1, &rfds, &wfds, &efds, tsp, &sigmask);
    caml_leave_blocking_section();

    if (ret == -1) uerror("pselect", Nothing);

    v_rfds = fdset_to_fdlist(v_rfds, &rfds);
    v_wfds = fdset_to_fdlist(v_wfds, &wfds);
    v_efds = fdset_to_fdlist(v_efds, &efds);
    v_res = caml_alloc_small(3, 0);
    Field(v_res, 0) = v_rfds;
    Field(v_res, 1) = v_wfds;
    Field(v_res, 2) = v_efds;
  End_roots();

  return v_res;
}


/* Clock functions */

#ifdef JSC_POSIX_TIMERS
#define clockid_t_val(v_cl) ((clockid_t) Nativeint_val(v_cl))

CAMLprim value unix_clock_gettime(value v_cl)
{
  struct timespec ts;
  if (clock_gettime(clockid_t_val(v_cl), &ts))
    uerror("clock_gettime", Nothing);
  return caml_copy_double(timespec_to_double(ts));
}

CAMLprim value unix_clock_settime(value v_cl, value v_t)
{
  struct timespec ts = timespec_of_double(Double_val(v_t));
  if (clock_settime(clockid_t_val(v_cl), &ts))
    uerror("clock_settime", Nothing);
  return Val_unit;
}

CAMLprim value unix_clock_getres(value v_cl)
{
  struct timespec ts;
  if (clock_getres(clockid_t_val(v_cl), &ts))
    uerror("clock_getres", Nothing);
  return caml_copy_double(timespec_to_double(ts));
}

/* Unfortunately, it is currently not possible to
   extract the POSIX thread id given the OCaml-thread id due to lack of
   support for this feature in the OCaml-runtime.  The below function
   clearly does not do what is intended in the general case, but will
   probably usually do the right thing.
*/
static inline pthread_t pthread_t_val(value __unused v_tid)
{
  return pthread_self();
}

#if defined(JSC_THREAD_CPUTIME)
CAMLprim value unix_pthread_getcpuclockid(value v_tid)
{
  clockid_t c;
  if (pthread_getcpuclockid(pthread_t_val(v_tid), &c))
    uerror("pthread_getcpuclockid", Nothing);
  return caml_copy_nativeint(c);
}
#endif

#if defined(CLOCK_PROCESS_CPUTIME_ID)
#define CLOCK CLOCK_PROCESS_CPUTIME_ID
#elif defined(CLOCK_PROF)
#define CLOCK CLOCK_PROF
#else
#define CLOCK CLOCK_REALTIME
#endif

CAMLprim value unix_clock_process_cputime_id_stub(value __unused v_unit)
{
  return caml_copy_nativeint(CLOCK);
}

CAMLprim value unix_clock_thread_cputime_id_stub(value __unused v_unit)
{
  return caml_copy_nativeint(CLOCK);
}

#undef CLOCK

#else
#warning "posix timers not present; clock functions undefined"
#endif

/* Resource limits */

static inline int resource_val(value v_resource)
{
  int resource;
  switch (Int_val(v_resource)) {
    case 0 : resource = RLIMIT_CORE; break;
    case 1 : resource = RLIMIT_CPU; break;
    case 2 : resource = RLIMIT_DATA; break;
    case 3 : resource = RLIMIT_FSIZE; break;
    case 4 : resource = RLIMIT_NOFILE; break;
    case 5 : resource = RLIMIT_STACK; break;
#ifdef RLIMIT_AS
    case 6 : resource = RLIMIT_AS; break;
#endif
#ifdef RLIMIT_NICE
    case 7 : resource = RLIMIT_NICE; break;
#endif
    default :
      /* impossible */
      caml_failwith("resource_val: unknown sum tag");
      break;
  }
  return resource;
}

static inline rlim_t rlim_t_val(value v_lim)
{
  return
    Is_block(v_lim)
    ? (rlim_t) Int64_val(Field(v_lim, 0))
    : RLIM_INFINITY;
}

static value Val_rlim_t(rlim_t lim)
{
  value v_rl;
  if (lim == RLIM_INFINITY) v_rl = Val_int(0);
  else {
    value v_arg = caml_copy_int64(lim);
    Begin_roots1(v_arg);
      v_rl = caml_alloc_small(1, 0);
    End_roots();
    Field(v_rl, 0) = v_arg;
  }
  return v_rl;
}

CAMLprim value unix_getrlimit(value v_resource)
{
  CAMLparam0();
  CAMLlocal2(v_cur, v_max);
    int resource = resource_val(v_resource);
    value v_limits;
    struct rlimit rl;
    if (getrlimit(resource, &rl)) uerror("getrlimit", Nothing);
    v_cur = Val_rlim_t(rl.rlim_cur);
    v_max = Val_rlim_t(rl.rlim_max);
    v_limits = caml_alloc_small(2, 0);
    Field(v_limits, 0) = v_cur;
    Field(v_limits, 1) = v_max;
  CAMLreturn(v_limits);
}

CAMLprim value unix_setrlimit(value v_resource, value v_limits)
{
  struct rlimit rl;
  int resource = resource_val(v_resource);
  value v_cur = Field(v_limits, 0), v_max = Field(v_limits, 1);
  rl.rlim_cur = rlim_t_val(v_cur);
  rl.rlim_max = rlim_t_val(v_max);
  if (setrlimit(resource, &rl)) uerror("setrlimit", Nothing);
  return Val_unit;
}

/* Populating the ifreq structure is wiley and we do it in a couple
   of places, so lets factor it out here for safety. */
static struct ifreq build_ifaddr_request(const char *interface)
{
  struct ifreq ifr;
  memset(&ifr, 0, sizeof(ifr));
  ifr.ifr_addr.sa_family = AF_INET;
  strncpy(ifr.ifr_name, interface, sizeof(ifr.ifr_name));

  assert(sizeof(ifr.ifr_name) == IFNAMSIZ);
  if (ifr.ifr_name[IFNAMSIZ-1] != '\0')
    caml_failwith("build_ifaddr_request: interface name string too long");

  return ifr;
}

/* return a simple [in_addr] as the address */
struct in_addr core_unix_get_in_addr_for_interface(value v_interface)
{
  int fd = -1;
  char* error = NULL;
  struct ifreq ifr = build_ifaddr_request(String_val(v_interface));

  /* Note that [v_interface] is invalid past this point. */
  caml_enter_blocking_section();

  fd = socket(AF_INET, SOCK_DGRAM, 0);

  if (fd == -1)
    error = "linux_get_ipv4_address_for_interface: couldn't allocate socket";
  else {
    if (ioctl(fd, SIOCGIFADDR, &ifr) < 0)
      error = "linux_get_ipv4_address_for_interface: ioctl(fd, SIOCGIFADDR, ...) failed";

    (void) core_unix_close_durably(fd);
  }

  caml_leave_blocking_section();

  if (error == NULL) {
    /* This is weird but doing the usual casting causes errors when using
     * the new gcc on CentOS 6.  This solution was picked up on Red Hat's
     * bugzilla or something.  It also works to memcpy a sockaddr into
     * a sockaddr_in.  This is faster hopefully.
     */
    union {
      struct sockaddr sa;
      struct sockaddr_in sain;
    } u;
    u.sa = ifr.ifr_addr;
    return u.sain.sin_addr;
  }

  uerror(error, Nothing);
  assert(0);  /* [uerror] should never return. */
}


/* Resource usage */

CAMLprim value unix_getrusage(value v_who)
{
  CAMLparam0();
  CAMLlocal1(v_usage);
    int who = (Int_val(v_who) == 0) ? RUSAGE_SELF : RUSAGE_CHILDREN;
    struct rusage ru;
    if (getrusage(who, &ru)) uerror("getrusage", Nothing);
    v_usage = caml_alloc(16, 0);
    Store_field(v_usage, 0,
                caml_copy_double((double) ru.ru_utime.tv_sec +
                                 (double) ru.ru_utime.tv_usec / 1e6));
    Store_field(v_usage, 1,
                caml_copy_double((double) ru.ru_stime.tv_sec +
                                 (double) ru.ru_stime.tv_usec / 1e6));
    Store_field(v_usage, 2, caml_copy_int64(ru.ru_maxrss));
    Store_field(v_usage, 3, caml_copy_int64(ru.ru_ixrss));
    Store_field(v_usage, 4, caml_copy_int64(ru.ru_idrss));
    Store_field(v_usage, 5, caml_copy_int64(ru.ru_isrss));
    Store_field(v_usage, 6, caml_copy_int64(ru.ru_minflt));
    Store_field(v_usage, 7, caml_copy_int64(ru.ru_majflt));
    Store_field(v_usage, 8, caml_copy_int64(ru.ru_nswap));
    Store_field(v_usage, 9, caml_copy_int64(ru.ru_inblock));
    Store_field(v_usage, 10, caml_copy_int64(ru.ru_oublock));
    Store_field(v_usage, 11, caml_copy_int64(ru.ru_msgsnd));
    Store_field(v_usage, 12, caml_copy_int64(ru.ru_msgrcv));
    Store_field(v_usage, 13, caml_copy_int64(ru.ru_nsignals));
    Store_field(v_usage, 14, caml_copy_int64(ru.ru_nvcsw));
    Store_field(v_usage, 15, caml_copy_int64(ru.ru_nivcsw));
  CAMLreturn(v_usage);
}

/* System configuration */

CAMLprim value unix_sysconf(value v_name)
{
  int name;
  long ret;
  switch (Int_val(v_name)) {
    case 0 : name = _SC_ARG_MAX; break;
    case 1 : name = _SC_CHILD_MAX; break;
    case 2 : name = _SC_HOST_NAME_MAX; break;
    case 3 : name = _SC_LOGIN_NAME_MAX; break;
    case 4 : name = _SC_OPEN_MAX; break;
    case 5 : name = _SC_PAGESIZE; break;
    case 6 : name = _SC_RE_DUP_MAX; break;
    case 7 : name = _SC_STREAM_MAX; break;
    case 8 : name = _SC_SYMLOOP_MAX; break;
    case 9 : name = _SC_TTY_NAME_MAX; break;
    case 10 : name = _SC_TZNAME_MAX; break;
    case 11 : name = _SC_VERSION; break;
    /* We think this might work on Solaris, too, but don't have any boxes
       around to test it with. */
#if defined(__linux__)
    case 12 : name = _SC_PHYS_PAGES; break;
    case 13 : name = _SC_AVPHYS_PAGES; break;
#endif
    case 14 : name = _SC_IOV_MAX; break;
    default :
      /* impossible */
      caml_failwith("unix_sysconf: unknown sum tag");
      break;
  }
  errno = 0;
  ret = sysconf(name);
  if (ret == -1) {
    if (errno == 0) {
      return core_Val_none;
    }
    else {
      uerror("sysconf", Nothing);
    }
  }
  return core_Val_some(caml_copy_int64(ret));
}


/* POSIX thread functions */

#define Mutex_val(v) (* ((pthread_mutex_t **) Data_custom_val(v)))
#define Condition_val(v) (* ((pthread_cond_t **) Data_custom_val(v)))

static void caml_pthread_check(int retcode, char *msg)
{
#define err_buf_len 100
  char err_buf[err_buf_len];
  char *err;
  size_t errlen, msglen;
  value str;

  if (retcode == 0) return;

#ifdef __GLIBC__
  err = strerror_r(retcode, err_buf, err_buf_len);
#else
  if (strerror_r(retcode, err_buf, err_buf_len) == -1)
    uerror("strerror_r", Nothing);
  err = err_buf;
#endif

  msglen = strlen(msg);
  errlen = strlen(err);
  str = caml_alloc_string(msglen + 2 + errlen);
  memmove(&Byte(str, 0), msg, msglen);
  memmove(&Byte(str, msglen), ": ", 2);
  memmove(&Byte(str, msglen + 2), err, errlen);
  caml_raise_sys_error(str);
#undef err_buf_len
}

#if defined(_POSIX_TIMEOUTS) && (_POSIX_TIMEOUTS > 0)
CAMLprim value unix_mutex_timedlock(value v_mtx, value v_timeo)
{
  int ret;
  pthread_mutex_t *mtx = Mutex_val(v_mtx);
  ret = pthread_mutex_trylock(mtx);
  if (ret == EBUSY) {
    struct timespec ts = timespec_of_double(Double_val(v_timeo));
    Begin_roots1(v_mtx);
    caml_enter_blocking_section();
      ret = pthread_mutex_timedlock(mtx, &ts);
    caml_leave_blocking_section();
    End_roots();
    if (ret == ETIMEDOUT) return Val_false;
  }
  caml_pthread_check(ret, "Mutex.timedlock");
  return Val_true;
}
#else
#warning "POSIX TMO not present; unix_mutex_timedlock undefined"
#endif

CAMLprim value unix_condition_timedwait(value v_cnd, value v_mtx, value v_timeo)
{
  CAMLparam2(v_cnd, v_mtx);
    int ret;
    pthread_cond_t *cnd = Condition_val(v_cnd);
    pthread_mutex_t *mtx = Mutex_val(v_mtx);
    struct timespec ts = timespec_of_double(Double_val(v_timeo));
    caml_enter_blocking_section();
      ret = pthread_cond_timedwait(cnd, mtx, &ts);
    caml_leave_blocking_section();
    if (ret == ETIMEDOUT) CAMLreturn(Val_false);
    caml_pthread_check(ret, "Condition.timedwait");
  CAMLreturn(Val_true);
}

static void caml_mutex_finalize(value v_mtx)
{
  pthread_mutex_t *mtx = Mutex_val(v_mtx);
  pthread_mutex_destroy(mtx);
  caml_stat_free(mtx);
}

static int caml_mutex_condition_compare(value v_mtx1, value v_mtx2)
{
  pthread_mutex_t *mtx1 = Mutex_val(v_mtx1);
  pthread_mutex_t *mtx2 = Mutex_val(v_mtx2);
  return mtx1 == mtx2 ? 0 : mtx1 < mtx2 ? -1 : 1;
}

static struct custom_operations caml_mutex_ops = {
  "_mutex",
  caml_mutex_finalize,
  caml_mutex_condition_compare,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
# ifdef custom_compare_ext_default
  custom_compare_ext_default
#endif

};

#if defined(_POSIX_THREADS) && _POSIX_THREADS >= 200112L
CAMLprim value unix_create_error_checking_mutex(value __unused v_unit)
{
  pthread_mutex_t *mtx;
  pthread_mutexattr_t attrs;
  value v_res;
  pthread_mutexattr_init(&attrs);
  pthread_mutexattr_settype(&attrs, PTHREAD_MUTEX_ERRORCHECK);
  mtx = caml_stat_alloc(sizeof(pthread_mutex_t));
  caml_pthread_check(
    pthread_mutex_init(mtx, &attrs), "Mutex.create_error_checking");
  pthread_mutexattr_destroy(&attrs);
  v_res =
    caml_alloc_custom(&caml_mutex_ops, sizeof(pthread_mutex_t *), 1, 1000);
  Mutex_val(v_res) = mtx;
  return v_res;
}
#else
#warning "_POSIX_THREADS not defined or < 200112; unix_create_error_checking_mutex not available"
#endif

/* Pathname resolution */

/* Seems like a sane approach to getting a reasonable bound for the
   maximum path length */
#ifdef PATH_MAX
#define JANE_PATH_MAX ((PATH_MAX <= 0 || PATH_MAX > 65536) ? 65536 : PATH_MAX)
#else
#define JANE_PATH_MAX (65536)
#endif

#ifdef __GLIBC__
CAMLprim value unix_realpath(value v_path)
{
  char *path = String_val(v_path);
  char *res = realpath(path, NULL);
  if (res == NULL) uerror("realpath", v_path);
  else {
    value v_res = caml_copy_string(res);
    free(res);
    return v_res;
  }
}
#else
CAMLprim value unix_realpath(value v_path)
{
  char *path = String_val(v_path);
  /* [realpath] is inherently broken without GNU-extension, and this
     seems like a reasonable thing to do if we do not build against
     GLIBC. */
  char resolved_path[JANE_PATH_MAX];
  if (realpath(path, resolved_path) == NULL) uerror("realpath", v_path);
  return caml_copy_string(resolved_path);
}
#endif


/* Temporary file and directory creation */

static inline void init_mktemp(char *loc, char *buf, value v_path)
{
  int i, len = caml_string_length(v_path);
  if (len > JANE_PATH_MAX - 12) caml_invalid_argument(loc);
  memcpy(buf, String_val(v_path), len);
  i = len;
  buf[i++] = '.';
  buf[i++] = 't';
  buf[i++] = 'm';
  buf[i++] = 'p';
  buf[i++] = '.';
  while (i < len + 11) buf[i++] = 'X';
  buf[i++] = '\0';
}

CAMLprim value unix_mkstemp(value v_path)
{
  CAMLparam1(v_path);
  CAMLlocal1(v_res_path);
  char *loc = "mkstemp";
  char buf[JANE_PATH_MAX];
  int fd;
  value v_res;
  init_mktemp(loc, buf, v_path);
  caml_enter_blocking_section();
#if defined (JSC_MKOSTEMP)
  fd = mkostemp(buf, O_CLOEXEC);
#else
  fd = mkstemp(buf);
  if (fd != -1) {
    int flags = fcntl(fd, F_GETFD);
    if (flags == -1 || fcntl(fd, F_SETFD, flags | O_CLOEXEC) == -1) {
      close(fd);
      fd = -1;
    }
  }
#endif
  caml_leave_blocking_section();
  if (fd == -1) uerror(loc, v_path);
  v_res_path = caml_copy_string(buf);
  v_res = caml_alloc_small(2, 0);
  Field(v_res, 0) = v_res_path;
  Field(v_res, 1) = Val_int(fd);
  CAMLreturn(v_res);
}

CAMLprim value unix_mkdtemp(value v_path)
{
  CAMLparam1(v_path);
  char *loc = "mkdtemp";
  char *path;
  char buf[JANE_PATH_MAX];
  init_mktemp(loc, buf, v_path);
  caml_enter_blocking_section();
    path = mkdtemp(buf);
  caml_leave_blocking_section();
  if (path == NULL) uerror(loc, v_path);
  CAMLreturn(caml_copy_string(buf));
}


/* Signal handling */

CAMLprim value unix_abort(value v_unit)
{
  abort();
  return v_unit;
}


/* User id, group id management */

CAMLprim value unix_initgroups(value v_user, value v_group)
{
  int ret, user_len = caml_string_length(v_user) + 1;
  char *c_user = caml_stat_alloc(user_len);
  gid_t group = Long_val(v_group);
  memcpy(c_user, String_val(v_user), user_len);
  caml_enter_blocking_section();
    ret = initgroups(c_user, group);
    caml_stat_free(c_user);
  caml_leave_blocking_section();
  if (ret == -1) uerror("initgroups", Nothing);
  return Val_unit;
}

CAMLprim value unix_getgrouplist(value v_user, value v_group)
{
  int n;
  int ngroups = NGROUPS_MAX;
  gid_t groups[NGROUPS_MAX];
  value ret;
  char *c_user;

  assert(Is_block(v_user) && Tag_val(v_user) == String_tag);
  assert(!Is_block(v_group));

  c_user = strdup(String_val(v_user));

  caml_enter_blocking_section();
    n = getgrouplist(c_user, Long_val(v_group), groups, &ngroups);
    free(c_user);
  caml_leave_blocking_section();

  if (n == -1) uerror ("getgrouplist", Nothing);

  ret = caml_alloc(n, 0);
  for (n = n - 1; n >= 0; n--) {
    Store_field(ret, n, Val_long(groups[n]));
  }

  return ret;
}

/* Globbing and shell string expansion */

CAMLprim value unix_fnmatch_make_flags(value v_flags)
{
  int flags = 0, i = Wosize_val(v_flags);
  while (--i >= 0) {
    switch (Int_val(Field(v_flags, i))) {
      case 0 : flags |= FNM_NOESCAPE; break;
      case 1 : flags |= FNM_PATHNAME; break;
      case 2 : flags |= FNM_PERIOD; break;
      case 3 : flags |= FNM_PATHNAME; break;
      case 4 : flags |= FNM_LEADING_DIR; break;
      default : flags |= FNM_CASEFOLD; break;
    }
  }
  return caml_copy_int32(flags);
}

CAMLprim value unix_fnmatch(value v_flags, value v_glob, value v_str)
{
  int flags = Int32_val(v_flags);
  char *glob = String_val(v_glob);
  char *str = String_val(v_str);
  int ret = fnmatch(glob, str, flags);
  switch (ret) {
    case 0 : return Val_true;
    case FNM_NOMATCH : return Val_false;
    default : caml_failwith("fnmatch");
  }
}

#if defined(JSC_WORDEXP)

CAMLprim value unix_wordexp_make_flags(value v_flags)
{
  int flags = 0, i = Wosize_val(v_flags);
  while (--i >= 0) {
    switch (Int_val(Field(v_flags, i))) {
      case 0 : flags |= WRDE_NOCMD; break;
      case 1 : flags |= WRDE_SHOWERR; break;
      default : flags |= WRDE_UNDEF; break;
    }
  }
  return caml_copy_int32(flags);
}

CAMLprim value unix_wordexp(value v_flags, value v_str)
{
  CAMLparam0();
  CAMLlocal1(v_res);
  int flags = Int32_val(v_flags);
  unsigned int i, len = caml_string_length(v_str) + 1;
  int ret;
  char *buf = caml_stat_alloc(len);
  char **w;
  wordexp_t p;
  memcpy(buf, String_val(v_str), len);
  caml_enter_blocking_section();
    ret = wordexp(buf, &p, flags);
    caml_stat_free(buf);
  caml_leave_blocking_section();
  switch (ret) {
    case 0 :
      v_res = caml_alloc(p.we_wordc, 0);
      w = p.we_wordv;
      for (i = 0; i < p.we_wordc; ++i)
        Store_field(v_res, i, caml_copy_string(w[i]));
      wordfree(&p);
      CAMLreturn(v_res);
    case WRDE_BADCHAR : caml_failwith("wordexp: bad char");
    case WRDE_BADVAL : caml_failwith("wordexp: undefined shell variable");
    case WRDE_CMDSUB : caml_failwith("wordexp: unwanted command substitution");
    case WRDE_NOSPACE : caml_failwith("wordexp: out of memory");
    case WRDE_SYNTAX : caml_failwith("wordexp: syntax error");
    default : caml_failwith("wordexp: impossible");
  }
}

#endif /* defined(JSC_WORDEXP) */

/* System information */

CAMLprim value unix_uname(value v_unit __unused)
{
  CAMLparam0();
  CAMLlocal1(v_utsname);
    struct utsname u;
    if (uname(&u)) uerror("uname", Nothing);
    v_utsname = caml_alloc(5, 0);
    Store_field(v_utsname, 0, caml_copy_string(u.sysname));
    Store_field(v_utsname, 1, caml_copy_string(u.nodename));
    Store_field(v_utsname, 2, caml_copy_string(u.release));
    Store_field(v_utsname, 3, caml_copy_string(u.version));
    Store_field(v_utsname, 4, caml_copy_string(u.machine));
  CAMLreturn(v_utsname);
}


/* Additional IP functionality */

CAMLprim value unix_if_indextoname(value v_index)
{
  char name[IF_NAMESIZE];
  if (if_indextoname((unsigned int) Int_val(v_index), name) == NULL)
    uerror("if_indextoname", Nothing);
  else return caml_copy_string(name);
}

#include "socketaddr.h"

/* Keep this in sync with the type Core_unix.Mcast_action.t */
#define VAL_MCAST_ACTION_ADD  (Val_int(0))
#define VAL_MCAST_ACTION_DROP (Val_int(1))

CAMLprim value core_unix_mcast_modify (value v_action,
                                       value v_ifname_opt,
                                       value v_source_opt,
                                       value v_fd,
                                       value v_sa)
{
  int ret, fd = Int_val(v_fd);
  union sock_addr_union sau;
  struct sockaddr *sa = &sau.s_gen;
  socklen_param_type sa_len;

  get_sockaddr(v_sa, &sau, &sa_len);

  switch (sa->sa_family) {
    case AF_INET: {
      struct ip_mreq mreq;

      memcpy(&mreq.imr_multiaddr,
             &((struct sockaddr_in *) sa)->sin_addr,
             sizeof(struct in_addr));

      if (Is_block(v_ifname_opt)) {
        struct ifreq ifreq;

        assert(Tag_val(v_ifname_opt) == 0 && Wosize_val(v_ifname_opt) == 1);
        ifreq = build_ifaddr_request(String_val(Field(v_ifname_opt,0)));

        if (ioctl(fd, SIOCGIFADDR, &ifreq) < 0)
          uerror("core_unix_mcast_modify: ioctl", Nothing);

        memcpy(&mreq.imr_interface,
               &((struct sockaddr_in *) &ifreq.ifr_addr)->sin_addr,
               sizeof(struct in_addr));
      } else {
        assert(v_ifname_opt == Val_long(0) /* None */);
        mreq.imr_interface.s_addr = htonl(INADDR_ANY);
      }

      if (Is_block(v_source_opt)) {
#if defined(__APPLE__) || defined(__OpenBSD__) || defined(__NetBSD__)
        caml_failwith("core_unix_mcast_modify: ~source is not supported on MacOS, OpenBSD or NetBSD");
#else
        struct ip_mreq_source mreq_source;

        assert(v_action == VAL_MCAST_ACTION_ADD);
        assert(Tag_val(v_source_opt) == 0 && Wosize_val(v_source_opt) == 1);

        mreq_source.imr_multiaddr  = mreq.imr_multiaddr;
        mreq_source.imr_interface  = mreq.imr_interface;
        mreq_source.imr_sourceaddr = GET_INET_ADDR(Field(v_source_opt, 0));

        ret = setsockopt(fd, IPPROTO_IP, IP_ADD_SOURCE_MEMBERSHIP, &mreq_source,
                         sizeof(mreq_source));
#endif
      } else {
        int optname;

        assert(v_source_opt == Val_long(0) /* None */);

        switch (v_action) {
          case VAL_MCAST_ACTION_ADD:
            optname = IP_ADD_MEMBERSHIP;
            break;

          case VAL_MCAST_ACTION_DROP:
            optname = IP_DROP_MEMBERSHIP;
            break;

          default:
            caml_failwith("core_unix_mcast_modify: invalid action");
        }

        ret = setsockopt(fd, IPPROTO_IP, optname, &mreq, sizeof(mreq));
      }

      if (ret == -1) uerror("core_unix_mcast_modify: setsockopt", Nothing);
      return Val_unit;
    }

    default:
      unix_error(EPROTONOSUPPORT, "core_unix_mcast_modify", Nothing);
  }
}

/* Similar to it's use in linux_ext, these are unfortunately not exported presently. It seems we
   should either get the functions exported, or have all portable ip level options (such as
   IP_ADD_MEMBERSHIP, IP_DROP_MEMBERSHIP, IP_MULTICAST_TTL, IP_MULTICAST_LOOP, and
   IP_MULTICAST_IF) added to the stdlib. */
enum option_type {
  TYPE_BOOL = 0,
  TYPE_INT = 1,
  TYPE_LINGER = 2,
  TYPE_TIMEVAL = 3,
  TYPE_UNIX_ERROR = 4
};

extern value unix_getsockopt_aux(
  char *name,
  enum option_type ty, int level, int option,
  value v_socket);
extern value unix_setsockopt_aux(
  char *name,
  enum option_type ty, int level, int option,
  value v_socket, value v_status);

CAMLprim value unix_mcast_get_ttl(value v_socket)
{
  return
    unix_getsockopt_aux("getsockopt", TYPE_INT, IPPROTO_IP, IP_MULTICAST_TTL, v_socket);
}

CAMLprim value unix_mcast_set_ttl(value v_socket, value v_ttl)
{
  return
    unix_setsockopt_aux( "setsockopt", TYPE_INT, IPPROTO_IP, IP_MULTICAST_TTL, v_socket, v_ttl);
}

CAMLprim value unix_mcast_set_ifname(value v_socket, value v_ifname)
{
  struct in_addr addr;

  assert(!Is_block(v_socket));

  /* Here is the IPv4 address of the ethernet interface. */
  addr = core_unix_get_in_addr_for_interface(v_ifname);

  /* Now setsockopt to publish on the interface using the address. */
  return
    unix_setsockopt_aux("setsockopt",
                        TYPE_INT,
                        IPPROTO_IP, IP_MULTICAST_IF,
                        v_socket,
                        Val_int(addr.s_addr));
}

CAMLprim value unix_mcast_get_loop(value v_socket)
{
  return
    unix_getsockopt_aux("getsockopt", TYPE_BOOL, IPPROTO_IP, IP_MULTICAST_LOOP, v_socket);
}

CAMLprim value unix_mcast_set_loop(value v_socket, value v_loop)
{
  return
    unix_setsockopt_aux( "setsockopt", TYPE_BOOL, IPPROTO_IP, IP_MULTICAST_LOOP, v_socket, v_loop);
}

/* Scheduling */

#if defined(_POSIX_PRIORITY_SCHEDULING) && (_POSIX_PRIORITY_SCHEDULING+0 > 0)
static int sched_policy_table[] = { SCHED_FIFO, SCHED_RR, SCHED_OTHER };

CAMLprim value unix_sched_setscheduler(
  value v_pid, value v_policy, value v_priority)
{
  struct sched_param sched_param;
  int pid = Int_val(v_pid);
  int policy = sched_policy_table[Int_val(v_policy)];
  int priority = Int_val(v_priority);

  if (sched_getparam(pid, &sched_param) != 0) uerror("sched_getparam", Nothing);
  sched_param.sched_priority = priority;

  if (sched_setscheduler(pid, policy, &sched_param) != 0)
    uerror("sched_setscheduler", Nothing);

  return Val_unit;
}
#else
#warning "_POSIX_PRIORITY_SCHEDULING not present; sched_setscheduler undefined"
CAMLprim value unix_sched_setscheduler(
  value __unused v_pid, value __unused v_policy, value __unused v_priority)
{  invalid_argument("sched_setscheduler unimplemented"); }
#endif


/* Priority */

CAMLprim value unix_nice(value v_inc)
{
  int new_nice;
  errno = 0;
  new_nice = nice(Int_val(v_inc));
  if (new_nice == -1 && errno) uerror("nice", Nothing);
  else return Val_int(new_nice);
}

CAMLprim value unix_unsetenv(value var)
{
  if (unsetenv(String_val(var)) != 0) uerror("unsetenv", var);
  return Val_unit;
}


static int mman_mcl_flags_table[] = { MCL_CURRENT, MCL_FUTURE };

CAMLprim value unix_mlockall(value v_flags)
{
  CAMLparam1(v_flags);
  size_t i, mask;

  for (i = 0, mask = 0; i < Wosize_val(v_flags); i++)
    mask |= mman_mcl_flags_table[Int_val(Field(v_flags, i))];

  if (mlockall(mask) < 0)
    uerror("mlockall", Nothing);

  CAMLreturn(Val_unit);
}

CAMLprim value unix_munlockall()
{
  if (munlockall() < 0)
    uerror("munlockall", Nothing);
  return Val_unit;
}

static value alloc_tm(struct tm *tm)
{
  value res;
  res = caml_alloc_small(9, 0);
  Field(res,0) = Val_int(tm->tm_sec);
  Field(res,1) = Val_int(tm->tm_min);
  Field(res,2) = Val_int(tm->tm_hour);
  Field(res,3) = Val_int(tm->tm_mday);
  Field(res,4) = Val_int(tm->tm_mon);
  Field(res,5) = Val_int(tm->tm_year);
  Field(res,6) = Val_int(tm->tm_wday);
  Field(res,7) = Val_int(tm->tm_yday);
  Field(res,8) = tm->tm_isdst ? Val_true : Val_false;
  return res;
}

CAMLprim value unix_strptime(value v_fmt, value v_s)
{
  CAMLparam2(v_s, v_fmt);

  struct tm tm;
  tm.tm_sec  = 0;
  tm.tm_min  = 0;
  tm.tm_hour = 0;
  tm.tm_mday = 0;
  tm.tm_mon  = 0;
  tm.tm_year = 0;
  tm.tm_wday = 0;
  tm.tm_yday = 0;
  tm.tm_isdst = 0;

  if (strptime(String_val(v_s), String_val(v_fmt), &tm) == NULL)
    caml_failwith("unix_strptime: match failed");

  CAMLreturn(alloc_tm(&tm));
}

CAMLprim value core_unix_remove(value v_path)
{
  CAMLparam1(v_path);
  int retval;
  char *path = core_copy_to_c_string(v_path);

  caml_enter_blocking_section();
  retval = remove(path);
  caml_stat_free(path);
  caml_leave_blocking_section();

  if (retval)
    uerror("remove", v_path);

  CAMLreturn(Val_unit);
}

#if defined(GET_THREAD_ID)

CAMLprim value unix_gettid(value v_unit __unused)
{
  return Val_long(GET_THREAD_ID);
}

#endif

static value
sockaddr_to_caml_string_of_octets(struct sockaddr* sa, int family)
{
  CAMLparam0();
  CAMLlocal1(caml_addr);
  struct sockaddr_in* sin;
  struct sockaddr_in6* sin6;
  char* addr;
  int i, addrlen;
  char *addr_string;

  /* It is possible and reasonable for addresses other than ifa_addr to be NULL */
  if (sa == NULL) CAMLreturn(caml_alloc_string(0));

  if (family != sa->sa_family)
    caml_failwith("Not all addresses provided by getifaddrs have matching families.");

  switch (sa->sa_family) {
  case AF_INET:
    sin = (struct sockaddr_in *)sa;
    addr = (char *)&sin->sin_addr;
    addrlen = sizeof(struct in_addr);
    break;
  case AF_INET6:
    sin6 = (struct sockaddr_in6 *)sa;
    addr = (char *)&sin6->sin6_addr;
    addrlen = sizeof(struct in6_addr);
    break;
#if defined(JSC_LINUX_EXT)
  case AF_PACKET:
    addrlen = 0;
    break;
#endif
  default:
    /* Unknown AFs are filtered out below, before this function is called. */
    caml_failwith("Unexpected address family received from getifaddrs.");
  }

  caml_addr = caml_alloc_string(addrlen);
  addr_string = String_val(caml_addr);
  for (i = 0; i < addrlen; i++) {
    addr_string[i] = addr[i];
  }
  CAMLreturn(caml_addr);
}

static value
alloc_ifaddrs(struct ifaddrs* ifap, value family_variant)
{
  CAMLparam1(family_variant);
  CAMLlocal1(res);
  int family = ifap->ifa_addr->sa_family;

  res = caml_alloc(7, 0);

  /* THE ORDER AND NUMBER OF THESE IS IMPORTANT, SEE core_unix.ml!!! */
  Store_field(res, 0, caml_copy_string(ifap->ifa_name));
  Store_field(res, 1, family_variant);
  Store_field(res, 2, Val_int(ifap->ifa_flags));
  Store_field(res, 3, sockaddr_to_caml_string_of_octets(ifap->ifa_addr,      family));
  Store_field(res, 4, sockaddr_to_caml_string_of_octets(ifap->ifa_netmask,   family));
  /* Including both may be the most portable thing to do. */
  Store_field(res, 5, sockaddr_to_caml_string_of_octets(ifap->ifa_broadaddr, family));
  Store_field(res, 6, sockaddr_to_caml_string_of_octets(ifap->ifa_dstaddr,   family));

  CAMLreturn(res);
}

#if !defined(IFF_ALLMULTI)
#  define IFF_ALLMULTI 0
#endif
#if !defined(IFF_AUTOMEDIA)
#  define IFF_AUTOMEDIA 0
#endif
#if !defined(IFF_BROADCAST)
#  define IFF_BROADCAST 0
#endif
#if !defined(IFF_DEBUG)
#  define IFF_DEBUG 0
#endif
#if !defined(IFF_DYNAMIC)
#  define IFF_DYNAMIC 0
#endif
#if !defined(IFF_LOOPBACK)
#  define IFF_LOOPBACK 0
#endif
#if !defined(IFF_MASTER)
#  define IFF_MASTER 0
#endif
#if !defined(IFF_MULTICAST)
#  define IFF_MULTICAST 0
#endif
#if !defined(IFF_NOARP)
#  define IFF_NOARP 0
#endif
#if !defined(IFF_NOTRAILERS)
#  define IFF_NOTRAILERS 0
#endif
#if !defined(IFF_POINTOPOINT)
#  define IFF_POINTOPOINT 0
#endif
#if !defined(IFF_PORTSEL)
#  define IFF_PORTSEL 0
#endif
#if !defined(IFF_PROMISC)
#  define IFF_PROMISC 0
#endif
#if !defined(IFF_RUNNING)
#  define IFF_RUNNING 0
#endif
#if !defined(IFF_SLAVE)
#  define IFF_SLAVE 0
#endif
#if !defined(IFF_UP)
#  define IFF_UP 0
#endif

/* THE ORDERING OF THESE CONSTANTS MUST MATCH core_unix.ml!!! */
static uint32_t iff_table [] = {
  IFF_ALLMULTI,
  IFF_AUTOMEDIA,
  IFF_BROADCAST,
  IFF_DEBUG,
  IFF_DYNAMIC,
  IFF_LOOPBACK,
  IFF_MASTER,
  IFF_MULTICAST,
  IFF_NOARP,
  IFF_NOTRAILERS,
  IFF_POINTOPOINT,
  IFF_PORTSEL,
  IFF_PROMISC,
  IFF_RUNNING,
  IFF_SLAVE,
  IFF_UP
};

CAMLprim value
core_unix_iff_to_int(value v_iff)
{
  CAMLparam1(v_iff);
  int tsize = sizeof(iff_table) / sizeof(int);
  int i = Int_val(v_iff);

  if (i < 0 || i > tsize - 1) caml_failwith("iff value out of range");

  CAMLreturn(Val_int(iff_table[i]));
}

CAMLprim value
core_unix_getifaddrs(value v_unit)
{
  CAMLparam1(v_unit);
  CAMLlocal4(head, next, caml_ifap, family_variant);
  struct ifaddrs* ifap_orig;
  struct ifaddrs* ifap;
  int retval;

  caml_release_runtime_system ();
  retval = getifaddrs(&ifap_orig);
  caml_acquire_runtime_system ();

  if (retval) uerror("getifaddrs", Nothing);

  /* THE ORDER OF THESE IS IMPORTANT, SEE core_unix.ml!!! */
#define CORE_PACKET 0
#define CORE_INET4  1
#define CORE_INET6  2

  head = Val_int(0);
  for (ifap = ifap_orig; ifap != NULL; ifap = ifap->ifa_next) {
    if (ifap->ifa_addr == NULL) continue;

    switch (ifap->ifa_addr->sa_family) {
#if defined(JSC_LINUX_EXT)
    case AF_PACKET:
      family_variant = Val_int(CORE_PACKET);
      break;
#endif
    case AF_INET:
      family_variant = Val_int(CORE_INET4);
      break;
    case AF_INET6:
      family_variant = Val_int(CORE_INET6);
      break;
    default:
      continue;
    }

    caml_ifap = alloc_ifaddrs(ifap, family_variant);
    next = caml_alloc(2, 0);
    Store_field(next, 0, caml_ifap);
    Store_field(next, 1, head);
    head = next;
  }

  caml_release_runtime_system ();
  freeifaddrs(ifap_orig);
  caml_acquire_runtime_system ();

  CAMLreturn(head);
}

#include <arpa/inet.h>

CAMLprim value core_unix_inet4_addr_of_int32(value v) {
  CAMLparam1(v);

  struct in_addr addr;
  addr.s_addr = ntohl(Int32_val(v));

  CAMLreturn(alloc_inet_addr(&addr));
}

CAMLprim value core_unix_inet4_addr_to_int32_exn(value v) {
  CAMLparam1(v);
  struct in_addr addr;

  if (caml_string_length(v) != 4) {
    caml_invalid_argument("not a valid IPv4 address");
  }

  addr = GET_INET_ADDR(v);
  CAMLreturn(caml_copy_int32(htonl(addr.s_addr)));
}

CAMLprim value core_unix_inet4_addr_of_int63(value v) {
  CAMLparam1(v);

  struct in_addr addr;
  addr.s_addr = ntohl(Int63_val(v));

  CAMLreturn(alloc_inet_addr(&addr));
}

CAMLprim value core_unix_inet4_addr_to_int63_exn(value v) {
  CAMLparam1(v);
  struct in_addr addr;

  if (caml_string_length(v) != 4) {
    caml_invalid_argument("not a valid IPv4 address");
  }

  addr = GET_INET_ADDR(v);
  CAMLreturn(caml_alloc_int63(htonl(addr.s_addr)));
}
