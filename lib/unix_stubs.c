/* Core_unix support functions written in C. */

#define _GNU_SOURCE

#include <string.h>
#include <pthread.h>
/* Darwin needs this to be included before if.h*/
#ifdef __APPLE__
#define _POSIX_SOURCE
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
#include <signal.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <sys/resource.h>
#include <grp.h>
#include <sys/select.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fnmatch.h>
#include <wordexp.h>
#include <stdio.h>
#include <assert.h>
#include <time.h>
#include <unistd.h>
#include <sched.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <math.h>

#include "ocaml_utils.h"
#include "config.h"

CAMLprim value unix_error_stub(value v_errcode, value v_cmdname, value cmd_arg)
{
  unix_error(Int_val(v_errcode), String_val(v_cmdname), cmd_arg);
  return Val_unit;
}

#define MAX_ERROR_LEN 4096

extern char **environ;

static void report_error(int fd, const char* str)
{
  char buf[MAX_ERROR_LEN];
  char buf2[MAX_ERROR_LEN];
#ifdef __GLIBC__
  snprintf(buf2, MAX_ERROR_LEN, "%s (%s)\n", str,
           strerror_r(errno, buf, MAX_ERROR_LEN));
#else
  if (strerror_r(errno, buf, MAX_ERROR_LEN) == -1)
    snprintf(buf, MAX_ERROR_LEN, "Unknown error %d", errno);
  snprintf(buf2, MAX_ERROR_LEN, "%s (%s)\n", str, buf);
#endif
  buf2[MAX_ERROR_LEN - 1] = '\0';
  write(fd, buf2, strlen(buf2));
}

/* Maximum number of arguments plus one (for terminating NULL) that may
   be passed to execv/execvp.  4096 is the minimum that POSIX allows,
   but note that it is not specified how much argument space is used
   up by variables in the user's environment.  This makes it hard to
   predict whether the system call can fail due to too many (implicit)
   arguments. */
/* Note: the ARG_MAX defined in sys/limits.h can be huge */
#define ML_ARG_MAX (4096 + 1)

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

/* Close function that handles signals correctly by retrying the close
   after EINTR.

   NOTE: we should never see EIO when closing pipes.  If so, it is
   reasonable to see this as a kernel bug, and it's pretty useless trying
   to catch/work around potential kernel bugs.  We assume that it works.
   An EBADF would be bad when closing successfully opened pipes, too, but
   in that case the pipe should be guaranteed to be closed anyway (unlike
   EIO).  This covers all errors that close could potentially return.
*/
static inline int safe_close(int fd)
{
  int ret;
  while ((ret = close(fd)) == -1 && errno == EINTR) /* empty loop */ ;
  return ret;
}

/* Idempotent version of safe_close: doesn't flag EBADF as an error. */
static inline int safe_close_idem(int fd)
{
  int ret = safe_close(fd);
  return (ret == -1 && errno == EBADF) ? 0 : ret;
}

/* Given v_prog, an O'Caml string value specifying a program name,
   v_args, an O'Caml array specifying program arguments (not
   including the program name), and v_search_path, an O'Caml boolean
   value specifying whether to search the PATH, fork a child process
   that executes the specified program.  Return the child's pid together
   with fds connected via pipes to the stdin, stdout and stderr of the
   program such that if the fds are closed the pipes are broken.

   Beware!
   A great deal of work has gone into making this subtle function thoroughly
   robust and, hopefully, correct.  Changes should not be undertaken lightly.
*/

CAMLprim value ml_create_process(value v_working_dir, value v_prog, value v_args,
                                 value v_env, value v_search_path)
{
  /* No need to protect the arguments or other values: we never release
     the O'Caml lock, and we never use O'Caml values after other values
     get allocated in the O'Caml heap. */
  typedef enum { READ_END = 0, WRITE_END = 1 } pipe_end_t;
  value v_res;
  int stdin_pfds[2];
  int stdout_pfds[2];
  int stderr_pfds[2];
  int child_pid;
  int my_errno;

  /* It's ok to hold pointers into the O'Caml heap, since the memory
     space gets duplicated upon the fork, during which we keep the
     O'Caml lock. */
  char *prog = String_val(v_prog);
  int search_path = Bool_val(v_search_path);

  /* We use a statically allocated, fixed-size array for performance
     reasons.  It is reasonable to assume that the array can never become
     too big for stack allocations anyway. */
  char *args[ML_ARG_MAX];
  int n_args = Wosize_val(v_args);
  int n_env  = Wosize_val(v_env);

  char *working_dir = NULL;

  /* Note that the executable name also counts as an argument, and we
     also have to subtract one more for the terminating NULL! */
  if (n_args >= ML_ARG_MAX - 1)
    caml_failwith("too many arguments for Unix.create_process");

  args[0] = prog;
  args[n_args + 1] = NULL;

  while (n_args) {
    args[n_args] = String_val(Field(v_args, n_args - 1));
    --n_args;
  }

  if (pipe(stdin_pfds) == -1)
    uerror("create_process: parent->stdin pipe creation failed", Nothing);

  if (pipe(stdout_pfds) == -1) {
    my_errno = errno;
    safe_close(stdin_pfds[READ_END]);
    safe_close(stdin_pfds[WRITE_END]);
    unix_error(my_errno,
               "create_process: stdout->parent pipe creation failed", Nothing);
  }

  if (pipe(stderr_pfds) == -1) {
    my_errno = errno;
    safe_close(stdin_pfds[READ_END]);
    safe_close(stdin_pfds[WRITE_END]);
    safe_close(stdout_pfds[READ_END]);
    safe_close(stdout_pfds[WRITE_END]);
    unix_error(my_errno,
               "create_process: stderr->parent pipe creation failed", Nothing);
  }

  /* This function deliberately doesn't release the O'Caml lock (i.e. it
     doesn't call caml_enter_blocking_section) during the fork.  This is
     because we hold pointers into the ML heap across a fork, and
     releasing the lock immediately before the fork could theoretically
     cause the GC to run and move blocks before the fork duplicates the
     memory space.

     If the parent process has threads that turn out to suffer from too
     much latency during this fork, we may want to rewrite this function
     to copy the O'Caml values into the C heap before the fork and release
     the O'Caml lock.  It seems unlikely that forks will ever take so
     long that people care.  In Linux 2.6 forks are practically constant
     time even in the presence of ridiculous amounts of processes, and
     are reported to always have less than 500us latency.  Maybe the
     kernel does not even schedule threads during forks anyway.  */
  if ((child_pid = fork()) == 0) {
    /* Child process. */

    /* Just in case any of the pipes' file descriptors are 0, 1 or 2
       (not inconceivable, especially when running as a daemon),
       duplicate all three descriptors we need in the child to fresh
       descriptors before duplicating them onto stdin, stdout and stderr.

       It is in fact the case that none of [temp_stdin], [temp_stdout] and
       [temp_stderr] will never be 0, 1, or 2.  That this is so follows from
       the following three properties:

       1. The kernel always allocates the lowest-numbered unused fd when
          asked for a new one.

       2. We allocated more than two fds during the calls to [pipe] above.

       3. We have not closed any fds between the calls to [pipe] above and
          this point.  */
    int temp_stdin = dup(stdin_pfds[READ_END]);
    int temp_stdout = dup(stdout_pfds[WRITE_END]);
    int temp_stderr = dup(stderr_pfds[WRITE_END]);
    if (temp_stdin == -1 || temp_stdout == -1 || temp_stderr == -1) {
      /* Errors here and below are sent back to the parent on the
         stderr pipe. */
      report_error(stderr_pfds[WRITE_END],
                   "could not dup fds in child process");
      /* The open fds will be cleaned up by exit(); likewise below.
         We use 254 to avoid any clash with ssh returning 255. */
      exit(254);
    }

    /* We are going to replace stdin, stdout, and stderr for this child
       process so we close the existing descriptors now.  They may be
       closed already so we ignore EBADF from close. */
    if (safe_close_idem(0) == -1
        || safe_close_idem(1) == -1
        || safe_close_idem(2) == -1) {
      report_error(temp_stderr,
                   "could not close standard descriptors in child process");
      exit(254);
    }

    /* All pipe fds propagated from parent to child via fork() must be
       closed, otherwise the reference counts on those fds won't drop
       to zero (and cause the pipe to be broken) when the parent closes
       them. */
    safe_close(stdin_pfds[READ_END]);
    safe_close(stdin_pfds[WRITE_END]);
    safe_close(stdout_pfds[READ_END]);
    safe_close(stdout_pfds[WRITE_END]);
    safe_close(stderr_pfds[READ_END]);
    safe_close(stderr_pfds[WRITE_END]);

    /* We must dup2 after closing the pfds, because the latter might
       have been standard descriptors. */
    if (dup2(temp_stdin, 0 /* stdin */) == -1
        || dup2(temp_stdout, 1 /* stdout */) == -1
        || dup2(temp_stderr, 2 /* stderr */) == -1) {
      report_error(temp_stderr, "could not dup2 fds in child process");
      exit(254);
    }

    safe_close(temp_stdin);
    safe_close(temp_stdout);
    safe_close(temp_stderr);

    environ = NULL;
    while (n_env) putenv(String_val(Field(v_env, --n_env)));

    if (Is_block(v_working_dir))
      working_dir = String_val(Field(v_working_dir, 0));
    if (working_dir && chdir(working_dir) == -1) {
      report_error(2 /* stderr */, "chdir failed in child process");
      exit(254);
    }

    if ((search_path ? execvp : execv)(prog, args) == -1) {
      report_error(2 /* stderr */, "execvp/execv failed in child process");
      exit(254);
    }
  }

  my_errno = errno;

  /* Parent process. */

  /* Close the ends of the pipes that we [the parent] aren't going to use. */
  safe_close(stdin_pfds[READ_END]);
  safe_close(stdout_pfds[WRITE_END]);
  safe_close(stderr_pfds[WRITE_END]);

  /* Set the ends we are going to use to close on exec, so the next child we
   * fork doesn't get an unwanted inheritance. */
  close_on_exec(stdin_pfds[WRITE_END]);
  close_on_exec(stdout_pfds[READ_END]);
  close_on_exec(stderr_pfds[READ_END]);

  /* If the fork failed, cause the pipes to be destroyed and fail. */
  if (child_pid == -1) {
    safe_close(stdin_pfds[WRITE_END]);
    safe_close(stdout_pfds[READ_END]);
    safe_close(stderr_pfds[READ_END]);
    unix_error(my_errno, "create_process: failed to fork", Nothing);
  }

  /* Must use Field as an lvalue after caml_alloc_small -- not Store_field. */
  v_res = caml_alloc_small(4, 0);
  Field(v_res, 0) = Val_int(child_pid);
  Field(v_res, 1) = Val_int(stdin_pfds[WRITE_END]);
  Field(v_res, 2) = Val_int(stdout_pfds[READ_END]);
  Field(v_res, 3) = Val_int(stderr_pfds[READ_END]);

  return v_res;
}


/* Replacement for broken stat functions */

static int file_kind_table[] = {
  S_IFREG, S_IFDIR, S_IFCHR, S_IFBLK, S_IFLNK, S_IFIFO, S_IFSOCK
};

#define Val_file_offset(fofs) caml_copy_int64(fofs)

static value cst_to_constr(int n, int *tbl, int size, int deflt)
{
  int i;
  for (i = 0; i < size; i++)
    if (n == tbl[i]) return Val_int(i);
  return Val_int(deflt);
}

static value core_stat_aux_64(struct stat64 *buf)
{
  CAMLparam0();
  CAMLlocal5(atime, mtime, ctime, offset, v);

  atime = caml_copy_double((double) buf->st_atime);
  mtime = caml_copy_double((double) buf->st_mtime);
  ctime = caml_copy_double((double) buf->st_ctime);
  offset = Val_file_offset(buf->st_size);
  v = caml_alloc_small(12, 0);
  Field (v, 0) = Val_int (buf->st_dev);
  Field (v, 1) = Val_int (buf->st_ino);
  Field (v, 2) = cst_to_constr(buf->st_mode & S_IFMT, file_kind_table,
                               sizeof(file_kind_table) / sizeof(int), 0);
  Field (v, 3) = Val_int (buf->st_mode & 07777);
  Field (v, 4) = Val_int (buf->st_nlink);
  Field (v, 5) = Val_int (buf->st_uid);
  Field (v, 6) = Val_int (buf->st_gid);
  Field (v, 7) = Val_int (buf->st_rdev);
  Field (v, 8) = offset;
  Field (v, 9) = atime;
  Field (v, 10) = mtime;
  Field (v, 11) = ctime;
  CAMLreturn(v);
}

static inline char * core_copy_to_c_string(value v_str)
{
  asize_t len = caml_string_length(v_str) + 1;
  char *p = caml_stat_alloc(len);
  memcpy(p, String_val(v_str), len);
  return p;
}

CAMLprim value core_unix_stat_64(value path)
{
  CAMLparam1(path);
  int ret;
  struct stat64 buf;
  char *p = core_copy_to_c_string(path);
  caml_enter_blocking_section();
  ret = stat64(p, &buf);
  caml_stat_free(p);
  caml_leave_blocking_section();
  if (ret == -1) uerror("stat", path);
  CAMLreturn(core_stat_aux_64(&buf));
}

CAMLprim value core_unix_lstat_64(value path)
{
  CAMLparam1(path);
  int ret;
  struct stat64 buf;
  char *p = core_copy_to_c_string(path);
  caml_enter_blocking_section();
  ret = lstat64(p, &buf);
  caml_stat_free(p);
  caml_leave_blocking_section();
  if (ret == -1) uerror("lstat", path);
  CAMLreturn(core_stat_aux_64(&buf));
}

CAMLprim value core_unix_fstat_64(value fd)
{
  int ret;
  struct stat64 buf;
  caml_enter_blocking_section();
  ret = fstat64(Int_val(fd), &buf);
  caml_leave_blocking_section();
  if (ret == -1) uerror("fstat", Nothing);
  return core_stat_aux_64(&buf);
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

#define DIR_Val(v) *((DIR **) &Field(v, 0))
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
  ret = writev(Int_val(v_fd), iovecs, count);
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
      ts.tv_sec = (int) tm;
      ts.tv_nsec = (int) (1e9 * (tm - ts.tv_sec));
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


/* Unfortunately, it is currently not possible to
   extract the POSIX thread id given the OCaml-thread id due to lack of
   support for this feature in the OCaml-runtime.  The below function
   clearly does not do what is intended in the general case, but will
   probably usually do the right thing.

   mshinwell: I'll see about trying to fix the runtime.
*/
static inline pthread_t pthread_t_val(value __unused v_tid)
{
  return pthread_self();
}

/* Clock functions */

#ifdef JSC_POSIX_TIMERS
#define clockid_t_val(v_cl) ((clockid_t) Nativeint_val(v_cl))

CAMLprim value unix_pthread_getcpuclockid(value v_tid)
{
  clockid_t c;
  if (pthread_getcpuclockid(pthread_t_val(v_tid), &c))
    uerror("pthread_getcpuclockid", Nothing);
  return caml_copy_nativeint(c);
}

CAMLprim value unix_clock_process_cputime_id_stub(value __unused v_unit)
{
  return caml_copy_nativeint(CLOCK_PROCESS_CPUTIME_ID);
}

CAMLprim value unix_clock_thread_cputime_id_stub(value __unused v_unit)
{
  return caml_copy_nativeint(CLOCK_THREAD_CPUTIME_ID);
}

CAMLprim value unix_clock_gettime(value v_cl)
{
  struct timespec ts;
  if (clock_gettime(clockid_t_val(v_cl), &ts))
    uerror("clock_gettime", Nothing);
  return caml_copy_double((double) ts.tv_sec + (double) ts.tv_nsec / 1e9);
}

CAMLprim value unix_clock_settime(value v_cl, value v_t)
{
  double t = Double_val(v_t);
  struct timespec ts;
  ts.tv_sec = t;
  ts.tv_nsec = (t - ts.tv_sec) * 1e9;
  if (clock_settime(clockid_t_val(v_cl), &ts))
    uerror("clock_settime", Nothing);
  return Val_unit;
}

CAMLprim value unix_clock_getres(value v_cl)
{
  struct timespec ts;
  if (clock_getres(clockid_t_val(v_cl), &ts))
    uerror("clock_getres", Nothing);
  return caml_copy_double((double) ts.tv_sec + (double) ts.tv_nsec / 1e9);
}
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
    case 6 : resource = RLIMIT_AS; break;
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
  ret = sysconf(name);
  if (ret == -1) uerror("sysconf", Nothing);
  return (caml_copy_int64(ret));
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
    double timeo = Double_val(v_timeo);
    struct timespec ts;
    ts.tv_sec = timeo;
    ts.tv_nsec = (timeo - ts.tv_sec) * 1e9;
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
    double timeo = Double_val(v_timeo);
    struct timespec ts;
    ts.tv_sec = timeo;
    ts.tv_nsec = (timeo - ts.tv_sec) * 1e9;
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

#if defined(_XOPEN_UNIX) && (_XOPEN_UNIX > 0)
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
#warn "XOPEN_UNIX not defined or = 0; unix_create_error_checking_mutex not available"
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
  if (len > JANE_PATH_MAX - 7) caml_invalid_argument(loc);
  memcpy(buf, String_val(v_path), len);
  for (i = len; i < len + 6; ++i) buf[i] = 'X';
  buf[len + 6] = '\0';
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
    fd = mkstemp(buf);
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


/* Globbing and shell string expansion */

CAMLprim value unix_fnmatch_make_flags(value v_flags)
{
  int flags = 0, i = Wosize_val(v_flags);
  while (--i >= 0) {
    switch (Int_val(Field(v_flags, i))) {
      case 0 : flags |= FNM_NOESCAPE; break;
      case 1 : flags |= FNM_PATHNAME; break;
      case 2 : flags |= FNM_PERIOD; break;
      case 3 : flags |= FNM_FILE_NAME; break;
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

#define MK_MCAST(NAME, OP) \
  CAMLprim value unix_mcast_##NAME(value v_ifname_opt, value v_fd, value v_sa) \
  { \
    int ret, fd = Int_val(v_fd); \
    union sock_addr_union sau; \
    struct sockaddr *sa = &sau.s_gen; \
    socklen_param_type sa_len; \
    get_sockaddr(v_sa, &sau, &sa_len); \
    switch (sa->sa_family) { \
      case AF_INET: { \
        struct ip_mreq mreq; \
        struct ifreq ifreq; \
        memcpy(&mreq.imr_multiaddr, \
               &((struct sockaddr_in *) sa)->sin_addr, \
               sizeof(struct in_addr)); \
        if (v_ifname_opt != Val_int(0)) { \
          value v_ifname = Field(v_ifname_opt, 0); \
          char *ifname = String_val(v_ifname); \
          int ifname_len = caml_string_length(v_ifname) + 1; \
          if (ifname_len > IFNAMSIZ) \
            caml_failwith("mcast_" STR(NAME) ": ifname string too long"); \
          strncpy(ifreq.ifr_name, ifname, IFNAMSIZ); \
          if (ioctl(fd, SIOCGIFADDR, &ifreq) < 0) \
            uerror("mcast_" STR(NAME), Nothing); \
          memcpy(&mreq.imr_interface, \
                 &((struct sockaddr_in *) &ifreq.ifr_addr)->sin_addr, \
                 sizeof(struct in_addr)); \
        } else mreq.imr_interface.s_addr = htonl(INADDR_ANY); \
        ret = \
          setsockopt(fd, IPPROTO_IP, IP_##OP##_MEMBERSHIP, \
                     &mreq, sizeof(mreq)); \
        if (ret == -1) uerror("mcast_" STR(NAME), Nothing); \
        return Val_unit; \
      } \
      default : \
        errno = EPROTONOSUPPORT; \
        uerror("mcast_" STR(NAME), Nothing); \
    } \
  }

MK_MCAST(join, ADD)
MK_MCAST(leave, DROP)


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


CAMLprim value unix_strftime(value v_tm, value v_fmt)
{
  struct tm tm;
  size_t len;
  char* buf;
  int buf_len;
  value v_str;

  buf_len = 128*1024 + caml_string_length(v_fmt);
  buf = malloc(buf_len);
  if (!buf) caml_failwith("unix_strftime: malloc failed");

  tm.tm_sec  = Int_val(Field(v_tm, 0));
  tm.tm_min  = Int_val(Field(v_tm, 1));
  tm.tm_hour = Int_val(Field(v_tm, 2));
  tm.tm_mday = Int_val(Field(v_tm, 3));
  tm.tm_mon  = Int_val(Field(v_tm, 4));
  tm.tm_year = Int_val(Field(v_tm, 5));
  tm.tm_wday = Int_val(Field(v_tm, 6));
  tm.tm_yday = Int_val(Field(v_tm, 7));
  tm.tm_isdst = Bool_val(Field(v_tm, 8));
#ifdef __USE_BSD
  tm.tm_gmtoff = 0;  /* GNU extension, may not be visible everywhere */
  tm.tm_zone = NULL; /* GNU extension, may not be visible everywhere */
#endif

  len = strftime(buf, buf_len, String_val(v_fmt), &tm);

  if (len == 0) {
    /* From the man page:
         "Note that the return value 0 does not necessarily indicate an error;
          for example, in many locales %p yields an empty string."
       Given how large our buffer is we just assume that 0 always indicates
       an empty string. */
    v_str = caml_copy_string("");
    free(buf);
    return v_str;
  }

  v_str = caml_copy_string(buf);  /* [strftime] always null terminates the string */
  free(buf);
  return v_str;
}


static struct timespec timespec_of_double(double seconds)
{
  struct timespec ts;

  ts.tv_sec = (time_t) floor(seconds);
  ts.tv_nsec = (long) (1e9 * (seconds - ts.tv_sec));

  return ts;
}

static double double_of_timespec(struct timespec ts)
{
  return (double) ts.tv_sec + ((double) ts.tv_nsec / 1e9);
}

CAMLprim value unix_nanosleep(value v_seconds)
{
  struct timespec req = timespec_of_double(Double_val(v_seconds));
  struct timespec rem;
  int retval;

  caml_enter_blocking_section();
  retval = nanosleep(&req, &rem);
  caml_leave_blocking_section();

  if (retval == 0)
    return caml_copy_double(0.0);
  else if (retval == -1) {
    if (errno == EINTR)
      return caml_copy_double(double_of_timespec(rem));
    else
      uerror("nanosleep", Nothing);
  }
  else
    caml_failwith("unix_nanosleep: impossible return value from nanosleep(2)");
}
