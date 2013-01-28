#ifndef JANE_COMMON_H
#define JANE_COMMON_H

#if __GNUC__ >= 3
# ifndef inline
#   define inline inline __attribute__ ((always_inline))
# endif
# ifndef __pure
#   define __pure __attribute__ ((pure))
# endif
# ifndef __const
#   define __const __attribute__ ((const))
# endif
# ifndef __malloc
#   define __malloc __attribute__ ((malloc))
# endif
# ifndef __unused
#   define __unused __attribute__ ((unused))
# endif
# ifndef __likely
#   define likely(x) __builtin_expect (!!(x), 1)
# endif
# ifndef __unlikely
#   define unlikely(x) __builtin_expect (!!(x), 0)
# endif
#else
# ifndef inline
#   define inline
# endif
# ifndef __pure
#   define __pure
# endif
# ifndef  __const
#   define __const
# endif
# ifndef  __malloc
#   define __malloc
# endif
# ifndef  __unused
#   define __unused
# endif
# ifndef  __likely
#   define likely(x) (x)
# endif
# ifndef  __unlikely
#   define unlikely(x) (x)
# endif
#endif

#endif /* JANE_COMMON_H */
