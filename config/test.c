/* This file is just preprocessed.  Lines of the form "OUT:XXX" are
   kept and replaced by XXX in the output to produce
   src/config.mlh. */

#include <caml/mlvalues.h>

#include <stdio.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/resource.h>

#if defined(LINUX_EXT)
"OUT:#let JSC_LINUX_EXT = true"
#else
"OUT:#let JSC_LINUX_EXT = false"
#endif

#if defined(LINUX_EXT) || defined(__OpenBSD__)
"OUT:#let JSC_THREAD_ID = true"
#else
"OUT:#let JSC_THREAD_ID = false"
#endif

#if defined(POSIX_TIMERS)
"OUT:#let JSC_POSIX_TIMERS = true"
#else
"OUT:#let JSC_POSIX_TIMERS = false"
#endif

#if defined(RLIMIT_NICE)
"OUT:#let JSC_RLIMIT_NICE = true"
#else
"OUT:#let JSC_RLIMIT_NICE = false"
#endif

#if defined(RLIMIT_AS)
"OUT:#let JSC_RLIMIT_AS = true"
#else
"OUT:#let JSC_RLIMIT_AS = false"
#endif

/* Defined in <caml/mlvalues.h> */
#if defined(ARCH_SIXTYFOUR)
"OUT:#let JSC_ARCH_SIXTYFOUR = true"
#else
"OUT:#let JSC_ARCH_SIXTYFOUR = false"
#endif

#if defined MSG_NOSIGNAL
"OUT:#let JSC_MSG_NOSIGNAL = true"
#else
"OUT:#let JSC_MSG_NOSIGNAL = false"
#endif

#if defined(_POSIX_TIMEOUTS) && (_POSIX_TIMEOUTS > 0)
"OUT:#let JSC_MUTEX_TIMED_LOCK = true"
#else
"OUT:#let JSC_MUTEX_TIMED_LOCK = false"
#endif

#if defined(_POSIX_SYNCHRONIZED_IO) && _POSIX_SYNCHRONIZED_IO > 0
"OUT:#let JSC_FDATASYNC = true"
#else
"OUT:#let JSC_FDATASYNC = false"
#endif

#if defined(_POSIX_THREAD_CPUTIME)
"OUT:#let JSC_THREAD_CPUTIME = true"
#else
"OUT:#let JSC_THREAD_CPUTIME = false"
#endif
