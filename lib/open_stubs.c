#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <pthread.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/signals.h>

extern void caml_sys_error(value arg);

#ifndef O_BINARY
#define O_BINARY 0
#endif
#ifndef O_TEXT
#define O_TEXT 0
#endif
#ifndef O_NONBLOCK
#ifdef O_NDELAY
#define O_NONBLOCK O_NDELAY
#else
#define O_NONBLOCK 0
#endif
#endif

static int sys_open_flags[] = {
  O_RDONLY, O_WRONLY, O_APPEND | O_WRONLY, O_CREAT, O_TRUNC, O_EXCL,
  O_BINARY, O_TEXT, O_NONBLOCK
};

CAMLprim value core_sys_open(value path, value vflags, value vperm)
{
  CAMLparam3(path, vflags, vperm);
  int fd, flags, perm;
  char * p;

  p = caml_stat_alloc(caml_string_length(path) + 1);
  strcpy(p, String_val(path));
  flags = caml_convert_flag_list(vflags, sys_open_flags);
  perm = Int_val(vperm);
  /* open on a named FIFO can block (PR#1533) */
  caml_enter_blocking_section();
  fd = open(p, flags, perm);
  caml_stat_free(p);
  if (fd == -1) {
    caml_leave_blocking_section();
    caml_sys_error(path);
  }
#if defined(F_SETFD) && defined(FD_CLOEXEC)
  fcntl(fd, F_SETFD, FD_CLOEXEC);
#endif
  caml_leave_blocking_section ();
  CAMLreturn(Val_long(fd));
}
