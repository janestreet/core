#include "config_ext.h"

#ifdef JSC_UNIX_PTY

#define _GNU_SOURCE
#define _XOPEN_SOURCE 600

#ifndef __USE_BSD
#define __USE_BSD
#endif

#include <fcntl.h>
#include <stdlib.h>
#include <termios.h>

#include <caml/alloc.h>
#include <unix_utils.h>

#if defined(__sun)
#define NO_POSIX_OPENPT
#endif

#ifdef NO_POSIX_OPENPT
int posix_openpt(int flags)
{
  return open("/dev/ptmx", flags);
}
#endif

static int posix_openpt_flag_table[2] = { O_RDWR, O_NOCTTY };

CAMLprim value unix_posix_openpt(value flags)
{
  CAMLparam1(flags);
  int fd, cv_flags;

  cv_flags = caml_convert_flag_list(flags, posix_openpt_flag_table);
  caml_enter_blocking_section();
  fd = posix_openpt(cv_flags);
  caml_leave_blocking_section();

  if (fd == -1) uerror("posix_openpt", Nothing);

  CAMLreturn (Val_int(fd));
}

CAMLprim value unix_grantpt(value mlfd)
{
  CAMLparam1(mlfd);
  int fd, err;

  fd = Int_val(mlfd);
  caml_enter_blocking_section();
  err = grantpt(fd);
  caml_leave_blocking_section();

  if (err == -1) uerror("grantpt", Nothing);

  CAMLreturn (Val_int(0));
}

CAMLprim value unix_unlockpt(value mlfd)
{
  CAMLparam1(mlfd);
  int fd, err;

  fd = Int_val(mlfd);
  caml_enter_blocking_section();
  err = unlockpt(fd);
  caml_leave_blocking_section();

  if (err == -1) uerror("unlockpt", Nothing);

  CAMLreturn (Val_int(0));
}

CAMLprim value unix_ptsname(value mlfd)
{
  CAMLparam1(mlfd);
  CAMLlocal1(mlname);
  int fd;
  char *name;

  fd = Int_val(mlfd);
  caml_enter_blocking_section();
  name = ptsname(fd);
  caml_leave_blocking_section();

  if (name == NULL) uerror("ptsname", Nothing);
  else mlname = caml_copy_string(name);

  CAMLreturn (mlname);
}

#endif /* JSC_UNIX_PTY */
