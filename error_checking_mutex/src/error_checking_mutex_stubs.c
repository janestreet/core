#define _GNU_SOURCE

#include <string.h>
#include <pthread.h>

#include "ocaml_utils.h"

#define Mutex_val(v) (* ((pthread_mutex_t **) Data_custom_val(v)))

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
  custom_compare_ext_default,
#endif
#ifdef custom_fixed_length_default
  custom_fixed_length_default,
#endif
};

#if defined(_POSIX_THREADS) && _POSIX_THREADS >= 200112L
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
#warning "_POSIX_THREADS not defined or < 200112; unix_create_error_checking_mutex not available"
#endif
