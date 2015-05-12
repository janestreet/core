#if JSC_STAT_NANOSEC_METHOD == 1
#  define NSEC(field) st_##field##tim.tv_nsec
#elif JSC_STAT_NANOSEC_METHOD == 2
#  define NSEC(field) st_##field##timespec.tv_nsec
#elif JSC_STAT_NANOSEC_METHOD == 3
#  define NSEC(field) st_##field##timensec
#else
#  error "JSC_STAT_NANOSEC_METHOD must be defined to 1, 2 or 3!"
#endif
