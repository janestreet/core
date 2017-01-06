/* This file is used by both unix_stubs.c and config/discover.ml */

#if !defined(JSC_THREAD_ID_METHOD)
#warning "JSC_THREAD_ID_METHOD not defined"
#endif

#if JSC_THREAD_ID_METHOD == 1

#include <sys/syscall.h>

#define GET_THREAD_ID syscall(SYS_gettid)

#elif JSC_THREAD_ID_METHOD == 2

#include <sys/types.h>

/* Advice from Philip Guenther on the ocaml-core mailing list is that we need to prototype
 * this ourselves :(
 * See: https://groups.google.com/forum/#!topic/ocaml-core/51knlnuJ8MM */
extern pid_t getthrid(void);

#define GET_THREAD_ID getthrid()

#elif !(JSC_THREAD_ID_METHOD == -1)
#warning "Unknown JSC_THREAD_ID_METHOD value"
#endif
