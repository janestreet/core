/* This file is just preprocessed.  Lines of the form "OUT:XXX" are
   kept and replaced by XXX in the output to produce
   src/config.h. */

#include <caml/mlvalues.h>

#include <stdio.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/resource.h>

#if defined(LINUX_EXT)
"OUT:#define JSC_LINUX_EXT"
#else
"OUT:#undef JSC_LINUX_EXT"
#endif

#if defined(LINUX_EXT) || defined(__OpenBSD__)
"OUT:#define JSC_THREAD_ID"
#else
"OUT:#undef JSC_THREAD_ID"
#endif

#if defined(POSIX_TIMERS)
"OUT:#define JSC_POSIX_TIMERS"
#else
"OUT:#undef JSC_POSIX_TIMERS"
#endif

#if defined(RLIMIT_NICE)
"OUT:#define JSC_RLIMIT_NICE"
#else
"OUT:#undef JSC_RLIMIT_NICE"
#endif

#if defined(RLIMIT_AS)
"OUT:#define JSC_RLIMIT_AS"
#else
"OUT:#undef JSC_RLIMIT_AS"
#endif

/* Defined in <caml/mlvalues.h> */
#if defined(ARCH_SIXTYFOUR)
"OUT:#define JSC_ARCH_SIXTYFOUR"
#else
"OUT:#undef JSC_ARCH_SIXTYFOUR"
#endif

#if defined(MSG_NOSIGNAL)
"OUT:#define JSC_MSG_NOSIGNAL"
#else
"OUT:#undef JSC_MSG_NOSIGNAL"
#endif

#if defined(SO_NOSIGPIPE)
"OUT:#define JSC_SO_NOSIGPIPE"
#else
"OUT:#undef JSC_SO_NOSIGPIPE"
#endif

#if defined(_POSIX_TIMEOUTS) && (_POSIX_TIMEOUTS > 0)
"OUT:#define JSC_MUTEX_TIMED_LOCK"
#else
"OUT:#undef JSC_MUTEX_TIMED_LOCK"
#endif

#if defined(_POSIX_SYNCHRONIZED_IO) && _POSIX_SYNCHRONIZED_IO > 0
"OUT:#define JSC_FDATASYNC"
#else
"OUT:#undef JSC_FDATASYNC"
#endif

#if defined(_POSIX_THREAD_CPUTIME)
"OUT:#define JSC_THREAD_CPUTIME"
#else
"OUT:#undef JSC_THREAD_CPUTIME"
#endif
