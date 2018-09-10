#define _GNU_SOURCE

#include "config.h"
#include "ocaml_utils.h"

#ifdef JSC_PTHREAD_NP

#include <string.h>
#include <pthread.h>
#include <assert.h>

#if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__) || defined(__APPLE__)
#include <pthread_np.h>
#include <sys/cpuset.h>
#else
#include <sched.h>
#endif

CAMLprim value pthread_np_setaffinity_self(value cpuids)
{
  int result;
  mlsize_t length, i;
  cpu_set_t cpuset;
  CPU_ZERO(&cpuset);

  length = Wosize_val(cpuids);
  for (i = 0; i < length; i++) 
  {
    CPU_SET(Int_val(Field(cpuids, i)), &cpuset);
  }

  result = pthread_setaffinity_np(pthread_self(), sizeof(cpu_set_t), &cpuset);
  if (result < 0) 
  {
    uerror("pthread_setaffinity_np", Nothing);
  }
  return Val_unit;
}

CAMLprim value pthread_np_getaffinity_self()
{
  CAMLparam0();
  CAMLlocal1(v_cpus);
  int result;
  mlsize_t cpu_count, i;
  cpu_set_t cpuset;

  CPU_ZERO(&cpuset);

  result = pthread_getaffinity_np(pthread_self(), sizeof(cpu_set_t), &cpuset);
  if (result < 0)
  {
    uerror("pthread_getaffinity_np", Nothing);
  }

  cpu_count = CPU_COUNT(&cpuset);
  v_cpus = caml_alloc_tuple(cpu_count);

  for (i = 0; i < CPU_SETSIZE; i++)
  {
    if (CPU_ISSET(i, &cpuset))
    {
      assert(cpu_count >= 1);
      Store_field(v_cpus, --cpu_count, Val_long(i));
    }
  }
  CAMLreturn(v_cpus);
}
#endif /* JSC_PTHREAD_NP */
