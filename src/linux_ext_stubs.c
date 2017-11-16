#include "config.h"
#ifdef JSC_LINUX_EXT
#define _FILE_OFFSET_BITS 64
#define _GNU_SOURCE

#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <unistd.h>
#include <sys/prctl.h>
#include <errno.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <net/if.h>
#include <time.h>
#include <sched.h>
#include <termios.h>
#include <sys/ioctl.h>
#include <sys/syscall.h>
#include <sys/sendfile.h>
#include <sys/epoll.h>
#include <sys/resource.h>
#include <arpa/inet.h>
#include <assert.h>

#include <sys/sysinfo.h>

#include "ocaml_utils.h"
#include "unix_utils.h"

/* Bytes_val is only available from 4.06 */
#ifndef Bytes_val
#define Bytes_val String_val
#endif

extern int core_unix_close_durably(int fd);
extern struct in_addr core_unix_get_in_addr_for_interface(value v_interface);

CAMLprim value
linux_sendfile_stub(value v_sock, value v_fd, value v_pos, value v_len)
{
  loff_t pos = Long_val(v_pos);
  ssize_t ret;

  caml_enter_blocking_section();
    ret = sendfile(Int_val(v_sock), Int_val(v_fd), &pos, Long_val(v_len));
  caml_leave_blocking_section();

  if (ret == -1) uerror("sendfile", Nothing);

  return Val_long(ret);
}

CAMLprim value linux_sysinfo(value __unused v_unit)
{
  struct sysinfo s_info;
  int ret = sysinfo(&s_info);
  if (ret == -1) uerror("sysinfo", Nothing);
  else {
    value v_res = caml_alloc_small(14, 0);
    Field(v_res, 0) = Val_long(s_info.uptime);
    Field(v_res, 1) = Val_long(s_info.loads[0]);
    Field(v_res, 2) = Val_long(s_info.loads[1]);
    Field(v_res, 3) = Val_long(s_info.loads[2]);
    Field(v_res, 4) = Val_long(s_info.totalram);
    Field(v_res, 5) = Val_long(s_info.freeram);
    Field(v_res, 6) = Val_long(s_info.sharedram);
    Field(v_res, 7) = Val_long(s_info.bufferram);
    Field(v_res, 8) = Val_long(s_info.totalswap);
    Field(v_res, 9) = Val_long(s_info.freeswap);
    Field(v_res, 10) = Val_int(s_info.procs);
    Field(v_res, 11) = Val_long(s_info.totalhigh);
    Field(v_res, 12) = Val_long(s_info.freehigh);
    Field(v_res, 13) = Val_int(s_info.mem_unit);
    return v_res;
  }
}

/**/

static int linux_tcpopt_bool[] = { TCP_CORK, TCP_QUICKACK };

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

CAMLprim value linux_gettcpopt_bool_stub(value v_socket, value v_option)
{
  int option = linux_tcpopt_bool[Int_val(v_option)];
  return
    unix_getsockopt_aux("getsockopt", TYPE_BOOL, SOL_TCP, option, v_socket);
}

CAMLprim value
linux_settcpopt_bool_stub(value v_socket, value v_option, value v_status)
{
  int option = linux_tcpopt_bool[Int_val(v_option)];
  return
    unix_setsockopt_aux(
      "setsockopt", TYPE_BOOL, SOL_TCP, option, v_socket, v_status);
}

/**/

static int nonblocking_no_sigpipe_flag = MSG_DONTWAIT | MSG_NOSIGNAL;

CAMLprim value linux_send_nonblocking_no_sigpipe_stub(
  value v_fd, value v_pos, value v_len, value v_buf)
{
  char *buf = Bytes_val(v_buf) + Long_val(v_pos);
  ssize_t ret =
    send(Int_val(v_fd), buf, Long_val(v_len), nonblocking_no_sigpipe_flag);
  if (ret == -1 && errno != EAGAIN && errno != EWOULDBLOCK)
    uerror("send_nonblocking_no_sigpipe", Nothing);
  return Val_long(ret);
}

CAMLprim value linux_send_no_sigpipe_stub(
  value v_fd, value v_pos, value v_len, value v_buf)
{
  char *buf = Bytes_val(v_buf) + Long_val(v_pos);
  ssize_t ret = send(Int_val(v_fd), buf, Long_val(v_len), MSG_NOSIGNAL);
  if (ret == -1) uerror("send_no_sigpipe", Nothing);
  return Val_long(ret);
}

CAMLprim value linux_sendmsg_nonblocking_no_sigpipe_stub(
  value v_fd, value v_iovecs, value v_count)
{
  int count = Int_val(v_count);
  ssize_t ret;
  struct iovec *iovecs = caml_stat_alloc(sizeof(struct iovec) * count);
  struct msghdr msghdr = { NULL, 0, NULL, 0, NULL, 0, 0 };
  msghdr.msg_iov = iovecs;
  msghdr.msg_iovlen = count;
  for (--count; count >= 0; --count) {
    struct iovec *iovec = &iovecs[count];
    value v_iovec = Field(v_iovecs, count);
    value v_iov_base = Field(v_iovec, 0);
    value v_iov_pos = Field(v_iovec, 1);
    value v_iov_len = Field(v_iovec, 2);
    iovec->iov_base = String_val(v_iov_base) + Long_val(v_iov_pos);
    iovec->iov_len = Long_val(v_iov_len);
  }
  ret = sendmsg(Int_val(v_fd), &msghdr, nonblocking_no_sigpipe_flag);
  caml_stat_free(iovecs);
  if (ret == -1 && errno != EAGAIN && errno != EWOULDBLOCK)
    uerror("sendmsg_nonblocking_no_sigpipe", Nothing);
  return Val_long(ret);
}

CAMLprim value linux_pr_set_pdeathsig_stub(value v_sig)
{
  int sig = caml_convert_signal_number(Int_val(v_sig));
  if (prctl(PR_SET_PDEATHSIG, sig) == -1) uerror("pr_set_pdeathsig", Nothing);
  return Val_unit;
}

CAMLprim value linux_pr_get_pdeathsig_stub(value __unused v_unit)
{
  int sig;
  if (prctl(PR_GET_PDEATHSIG, &sig) == -1) uerror("pr_get_pdeathsig", Nothing);
  return Val_int(caml_rev_convert_signal_number(sig));
}

static void cpulist_to_cpuset(value cpulist, cpu_set_t * cpuset)
{
  value l;
  CPU_ZERO(cpuset);
  for (l = cpulist; l != Val_int(0); l = Field(l, 1)) {
    int cpu = Int_val(Field(l, 0));
    CPU_SET(cpu, cpuset);
  }
}

CAMLprim value linux_sched_setaffinity(value v_pid, value cpulist)
{
  cpu_set_t set;
  pid_t pid;
  pid = Int_val(v_pid);
  cpulist_to_cpuset(cpulist, &set);
  if (sched_setaffinity(pid, sizeof(cpu_set_t), &set) != 0)
    uerror("setaffinity", Nothing);
  return Val_unit;
}

CAMLprim value linux_pr_set_name(value v_name)
{
  char *buf = String_val(v_name);
  if (prctl(PR_SET_NAME, (unsigned long) buf) == -1)
    uerror("pr_set_name", Nothing);
  return Val_unit;
}

CAMLprim value linux_pr_get_name(value __unused v_unit)
{
  char buf[17];

  buf[16] = 0;
  if (prctl(PR_GET_NAME, (unsigned long) buf) == -1)
    uerror("pr_get_name", Nothing);
  return caml_copy_string(buf);
}

CAMLprim value linux_setpriority(value v_priority)
{
  int tid;

  assert(!Is_block(v_priority));

  tid = syscall(SYS_gettid);
  if (setpriority(PRIO_PROCESS, tid, Long_val(v_priority)) == -1)
    uerror("setpriority", Nothing);

  return Val_unit;
}

CAMLprim value linux_getpriority(value v_unit)
{
  int tid;
  int old_errno;
  int priority;

  assert(v_unit == Val_unit);

  tid = syscall(SYS_gettid);

  old_errno = errno;
  errno = 0;
  priority = getpriority(PRIO_PROCESS, tid);
  if (errno != 0) {
    errno = old_errno;
    uerror("getpriority", Nothing);
  }
  errno = old_errno;

  return Val_long(priority);
}

CAMLprim value linux_get_terminal_size(value __unused v_unit)
{
  int fd;
  struct winsize ws;
  int ret;
  value v_res;

  caml_enter_blocking_section();

  fd = open("/dev/tty", O_RDWR);
  if (fd == -1) {
    caml_leave_blocking_section();
    uerror("get_terminal_size__open", Nothing);
  }
  ret = ioctl(fd, TIOCGWINSZ, &ws);
  if (ret == -1) {
    int old_errno = errno;
    (void)core_unix_close_durably(fd);
    caml_leave_blocking_section();
    if (ret == -1) {
      errno = old_errno;
      uerror("get_terminal_size__ioctl_close", Nothing);
    } else {
      errno = old_errno;
      uerror("get_terminal_size__ioctl", Nothing);
    }
  }
  ret = core_unix_close_durably(fd);
  caml_leave_blocking_section();
  if (ret == -1) uerror("get_terminal_size__close", Nothing);

  v_res = caml_alloc_small(2, 0);
  Field(v_res, 0) = Val_int(ws.ws_row);
  Field(v_res, 1) = Val_int(ws.ws_col);

  return v_res;
}

/* Construct an OCaml string as the IP */
CAMLprim value linux_get_ipv4_address_for_interface(value v_interface)
{
  struct in_addr addr = core_unix_get_in_addr_for_interface(v_interface);
  return caml_copy_string(inet_ntoa(addr));
}

/*
 * This linux specific socket option is in use for applications that require it
 * for security reasons. Taking a string argument, it does not fit the sockopt stubs used
 * for other socket options.
 */
CAMLprim value linux_bind_to_interface(value v_fd, value v_ifname)
{
  int ret, fd, ifname_len;
  char *ifname;

  assert(!Is_block(v_fd));
  assert(Is_block(v_ifname) && Tag_val(v_ifname) == String_tag);

  fd = Int_val(v_fd);
  ifname = String_val(v_ifname);

  ifname_len = caml_string_length(v_ifname) + 1;
  if (ifname_len > IFNAMSIZ) {
    caml_failwith("linux_bind_to_interface: ifname string too long");
  }

  ret = setsockopt(fd, SOL_SOCKET, SO_BINDTODEVICE, (void*)ifname, ifname_len);
  if (ret < 0) {
    uerror("bind_to_interface", Nothing);
  }

  return Val_unit;
}

/** Core epoll methods **/

#define EPOLL_FLAG(FLAG) DEFINE_INT63_CONSTANT (linux_epoll_##FLAG##_flag, FLAG)

EPOLL_FLAG(EPOLLIN)
EPOLL_FLAG(EPOLLOUT)
/* EPOLL_FLAG(EPOLLRDHUP) */
EPOLL_FLAG(EPOLLPRI)
EPOLL_FLAG(EPOLLERR)
EPOLL_FLAG(EPOLLHUP)
EPOLL_FLAG(EPOLLET)
EPOLL_FLAG(EPOLLONESHOT)

CAMLprim value linux_epoll_sizeof_epoll_event(value __unused v_unit)
{
  return Val_long(sizeof(struct epoll_event));
}

CAMLprim value linux_epoll_create(value __unused v_unit)
{
  int retcode;

  retcode = epoll_create1(EPOLL_CLOEXEC);
  if (retcode == -1) uerror("epoll_create", Nothing);

  return Val_long(retcode);
}

static value linux_epoll_ctl(value v_epfd, value v_fd, value v_flags, int operation)
{
  struct epoll_event evt;

  evt.data.ptr = NULL;
  evt.events = Int63_val(v_flags);
  evt.data.fd = Long_val(v_fd);

  if (epoll_ctl(Long_val(v_epfd), operation, Long_val(v_fd), &evt) == -1)
    uerror("epoll_ctl", Nothing);

  return Val_unit;
}

/*
 * Add and modify seem somewhat duplicative, I'm unsure the result of
 * adding an fd to a set a second time to change the event flags. Use
 * mod()...
 */
CAMLprim value linux_epoll_ctl_add(value v_epfd, value v_fd, value v_flags)
{
  return linux_epoll_ctl(v_epfd, v_fd, v_flags, EPOLL_CTL_ADD);
}

CAMLprim value linux_epoll_ctl_mod(value v_epfd, value v_fd, value v_flags)
{
  return linux_epoll_ctl(v_epfd, v_fd, v_flags, EPOLL_CTL_MOD);
}

/*
 * Some implementations ignore errors in delete, as they occur commonly when
 * an fd is closed prior to the del() call. close() removes an fd from an
 * epoll set automatically, so the del() call will fail.
 */
CAMLprim value linux_epoll_ctl_del(value v_epfd, value v_fd)
{
  if (epoll_ctl(Long_val(v_epfd), EPOLL_CTL_DEL, Long_val(v_fd), NULL) == -1)
    uerror("epoll_ctl", Nothing);

  return Val_unit;
}

CAMLprim value linux_epoll_wait(value v_epfd, value v_array, value v_timeout)
{
  struct epoll_event * evt;
  int retcode, maxevents;
  int timeout = Long_val(v_timeout);

  /* [CAMLparam1] is needed here to ensure that the bigstring does not get finalized
     during the period when we release the Caml lock, below.
  */
  CAMLparam1(v_array);

  evt = (struct epoll_event *) Caml_ba_data_val(v_array);
  maxevents = Caml_ba_array_val(v_array)->dim[0] / sizeof(struct epoll_event);

  /*
   * timeout, in milliseconds returns immediately if 0 is given, waits
   * forever with -1.
   */
  if (timeout == 0)
  {
    /* returns immediately, skip enter()/leave() pair */
    retcode = epoll_wait(Long_val(v_epfd), evt, maxevents, timeout);
  }
  else
  {
    caml_enter_blocking_section();
    retcode = epoll_wait(Long_val(v_epfd), evt, maxevents, timeout);
    caml_leave_blocking_section();
  }

  if (retcode == -1) uerror("epoll_wait", Nothing);

  CAMLreturn(Val_long(retcode));
}


/** Offsets and sizes of the resulting ready events array. */

CAMLprim value linux_epoll_offsetof_readyfd(value __unused v_unit)
{
  return Val_int( offsetof(struct epoll_event, data.fd));
}

CAMLprim value linux_epoll_offsetof_readyflags(value __unused v_unit)
{
  return Val_int( offsetof(struct epoll_event, events));
}

#ifdef JSC_TIMERFD

/** timerfd bindings **/

#include <sys/timerfd.h>

/* These values are from timerfd.h. They are not defined in Linux
   2.6.26 or earlier. */
#if !defined(TFD_NONBLOCK)
#  define TFD_NONBLOCK 04000
#endif
#if !defined(TFD_CLOEXEC)
#  define TFD_CLOEXEC 02000000
#endif

#define TIMERFD_INT63(X)                                  \
  CAMLprim value linux_timerfd_##X(value __unused v_unit) \
  { return caml_alloc_int63(X); }

TIMERFD_INT63(TFD_NONBLOCK)
TIMERFD_INT63(TFD_CLOEXEC)
TIMERFD_INT63(CLOCK_REALTIME)
TIMERFD_INT63(CLOCK_MONOTONIC)

CAMLprim value linux_timerfd_create(value v_clock_id, value v_flags)
{
  int retcode;

  retcode = timerfd_create(Int63_val(v_clock_id), Int63_val(v_flags));

  if (retcode == -1) uerror("timerfd_create", Nothing);

  return Val_int(retcode);
}

#define NANOS_PER_SECOND 1000000000

static inline void set_timespec(struct timespec *ts, value v)
{
  uint64_t d = Int63_val(v);
  ts->tv_sec = (time_t) (d / NANOS_PER_SECOND);
  ts->tv_nsec = (long) (d - (ts->tv_sec * NANOS_PER_SECOND));
}

CAMLprim value linux_timerfd_settime(value v_fd, value v_absolute,
                                     value v_initial, value v_interval)
{
  int retcode;
  struct itimerspec old, new;

  set_timespec(&new.it_value, v_initial);
  set_timespec(&new.it_interval, v_interval);

  retcode = timerfd_settime(Int_val(v_fd),
                            Bool_val(v_absolute) ? TFD_TIMER_ABSTIME : 0,
                            &new, &old);

  return retcode < 0 ? Val_int(-errno) : Val_unit;
}

#define Int63_ns_of_timespec(ts) \
  caml_alloc_int63((uint64_t)ts.tv_sec * NANOS_PER_SECOND + (uint64_t)ts.tv_nsec)

static value alloc_spec(struct itimerspec *spec)
{
  CAMLparam0();
  CAMLlocal1(v_spec);
  v_spec = caml_alloc_tuple(2);
  Store_field(v_spec, 0, Int63_ns_of_timespec(spec->it_value));
  Store_field(v_spec, 1, Int63_ns_of_timespec(spec->it_interval));
  CAMLreturn(v_spec);
}

CAMLprim value linux_timerfd_gettime(value v_fd)
{
  int retcode;
  struct itimerspec cur;

  retcode = timerfd_gettime(Int_val(v_fd), &cur);

  if (retcode == -1) uerror("timerfd_gettime", Nothing);

  return alloc_spec(&cur);
}

#endif /* JSC_TIMERFD */

#ifdef JSC_EVENTFD

#include <sys/eventfd.h>

#define EVENTFD_INT63(X) DEFINE_INT63_CONSTANT(linux_eventfd_##X, X)

EVENTFD_INT63(EFD_CLOEXEC)
EVENTFD_INT63(EFD_NONBLOCK)
EVENTFD_INT63(EFD_SEMAPHORE)

CAMLprim value
linux_eventfd(value v_initval, value v_flags)
{
  CAMLparam2(v_initval, v_flags);

  int initval, retval;

  initval = Int32_val(v_initval);
  if (initval < 0) caml_failwith("eventfd: initial value cannot be negative");

  retval = eventfd(initval, Int63_val(v_flags));
  if (retval < 0) uerror("eventfd", Nothing);

  CAMLreturn(Val_int(retval));
}

CAMLprim value
linux_eventfd_read(value v_fd)
{
  CAMLparam1(v_fd);

  int fd = Int_val(v_fd);
  uint64_t val;
  int retval;

  caml_enter_blocking_section();
  retval = read(fd, &val, sizeof(uint64_t));
  caml_leave_blocking_section();
  if (retval < 0) uerror("eventfd_read", Nothing);

  CAMLreturn(caml_copy_int64(val));
}

CAMLprim value
linux_eventfd_write(value v_fd, value v_val)
{
  CAMLparam2(v_fd, v_val);

  int fd = Int_val(v_fd);
  uint64_t val = Int64_val(v_val);
  int retval;

  caml_enter_blocking_section();
  retval = write(fd, &val, sizeof(uint64_t));
  caml_leave_blocking_section();
  if (retval < 0) uerror("eventfd_write", Nothing);

  CAMLreturn(Val_unit);
}

#endif /* JSC_EVENTFD */

#endif /* JSC_LINUX_EXT */
