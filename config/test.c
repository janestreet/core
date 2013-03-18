/* This file is just preprocessed.  Lines of the form "OUT:XXX" are
   kept and replaced by XXX in the output to produce
   lib/config.mlh. */

#include <caml/mlvalues.h>

#include <stdio.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/resource.h>

#if defined(LINUX_EXT)
"OUT:DEFINE LINUX_EXT"
#else
#  warning "cpp test --defined(LINUX_EXT)-- was false"
#  warning "Feature LINUX_EXT will not be availlable"
#endif

#if defined(POSIX_TIMERS)
"OUT:DEFINE POSIX_TIMERS"
#else
#  warning "cpp test --defined(POSIX_TIMERS)-- was false"
#  warning "Feature POSIX_TIMERS will not be availlable"
#endif

#if defined(RLIMIT_NICE)
"OUT:DEFINE RLIMIT_NICE"
#else
#  warning "cpp test --defined(RLIMIT_NICE)-- was false"
#  warning "Feature RLIMIT_NICE will not be availlable"
#endif

  /* Defined in <caml/mlvalues.h> */
#if defined(ARCH_SIXTYFOUR)
"OUT:DEFINE ARCH_SIXTYFOUR"
#endif

#if defined MSG_NOSIGNAL
"OUT:DEFINE MSG_NOSIGNAL"
#else
#  warning "cpp test --defined MSG_NOSIGNAL-- was false"
#  warning "Bigstring.(unsafe_|really_)?send(to)?(_noblocking)?_no_sigpipe will not be availlable"
#endif

#if defined(_POSIX_TIMEOUTS) && (_POSIX_TIMEOUTS > 0)
"OUT:DEFINE MUTEX_TIMED_LOCK"
#else
#  warning "cpp test --defined(_POSIX_TIMEOUTS) && (_POSIX_TIMEOUTS > 0)-- was false"
#  warning "Feature MUTEX_TIMED_LOCK will not be availlable"
#endif

#if defined(_POSIX_SYNCHRONIZED_IO) && _POSIX_SYNCHRONIZED_IO > 0
"OUT:DEFINE FDATASYNC"
#else
#  warning "cpp test --defined(_POSIX_SYNCHRONIZED_IO) && _POSIX_SYNCHRONIZED_IO > 0-- was false"
#  warning "Feature FDATASYNC will not be availlable"
#endif

#if defined(_POSIX_THREAD_CPUTIME)
"OUT:DEFINE THREAD_CPUTIME"
#else
#  warning "cpp test --defined(_POSIX_THREAD_CPUTIME)-- was false"
#  warning "Feature THREAD_CPUTIME will not be availlable"
#endif
