#define _FILE_OFFSET_BITS 64

#define _GNU_SOURCE             /* recvmmsg */

/* For pread/pwrite */
#define _XOPEN_SOURCE 500

/* For OpenBSD `swap` functions */
#ifdef __OpenBSD__
#define _BSD_SOURCE
#endif

#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <assert.h>
#include <stdint.h>

#ifdef __APPLE__
#include <libkern/OSByteOrder.h>
#define bswap_16 OSSwapInt16
#define bswap_32 OSSwapInt32
#define bswap_64 OSSwapInt64
#elif __GLIBC__
#include <byteswap.h>
#include <malloc.h>
#elif __OpenBSD__
#include <sys/types.h>
#define bswap_16 swap16
#define bswap_32 swap32
#define bswap_64 swap64
#elif __CYGWIN__
#include <endian.h>
#else
#include <sys/types.h>
#if defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__)
#include <sys/endian.h>
#else
#include <endian.h>
#endif
#define __BYTE_ORDER    _BYTE_ORDER
#define __LITTLE_ENDIAN _LITTLE_ENDIAN
#define __BIG_ENDIAN    _BIG_ENDIAN
#define bswap_16 bswap16
#define bswap_32 bswap32
#define bswap_64 bswap64
#endif

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <core_params.h>
#include "core_bigstring.h"
#include "internalhash.h"

/* Bytes_val is only available from 4.06 */
#ifndef Bytes_val
#define Bytes_val String_val
#endif

CAMLprim value
bigstring_realloc (value v_bstr, value v_size)
{
  CAMLparam2(v_bstr, v_size);
  CAMLlocal1(v_bstr2);
  struct caml_ba_array *ba = Caml_ba_array_val(v_bstr);
  intnat size = Long_val(v_size);
  int i;

  struct caml_ba_array *ba2;
  void *data;
  switch (ba->flags & CAML_BA_MANAGED_MASK) {
    case CAML_BA_EXTERNAL :
      caml_failwith("bigstring_realloc: bigstring is external or deallocated");
      break;
    case CAML_BA_MANAGED :
      if (ba->proxy != NULL) caml_failwith("bigstring_realloc: bigstring has proxy");
      break;
    case CAML_BA_MAPPED_FILE :
      caml_failwith("bigstring_realloc: bigstring is backed by memory map");
      break;
  }

  data = realloc(ba->data, sizeof(char) * size);
  /* realloc is equivalent to free when size is equal to zero, and may return NULL. */
  if (NULL == data && size != 0) caml_raise_out_of_memory ();

  v_bstr2 = caml_ba_alloc(ba->flags, ba->num_dims, data, ba->dim);
  ba2 = Caml_ba_array_val(v_bstr2);
  ba2->dim[0] = size;

  /* ba is a pointer into the OCaml heap, hence may have been invalidated by the
   * call to [caml_ba_alloc]. */
  ba = Caml_ba_array_val(v_bstr);
  ba->data = NULL;
  ba->flags = CAML_BA_EXTERNAL;
  for (i = 0; i < ba->num_dims; ++i) ba->dim[i] = 0;

  CAMLreturn(v_bstr2);
}

/* Destruction */

static void check_bigstring_proxy(struct caml_ba_array *b)
{
  if (b->proxy != NULL) caml_failwith("bigstring_destroy: bigstring has proxy");
}

void core_bigstring_destroy(value v, int flags)
{
  int i;
  struct caml_ba_array *b = Caml_ba_array_val(v);
  struct custom_operations* ops = Custom_ops_val(v);
  switch (b->flags & CAML_BA_MANAGED_MASK) {
    case CAML_BA_EXTERNAL :
      if ((flags & CORE_BIGSTRING_DESTROY_ALLOW_EXTERNAL)
           != CORE_BIGSTRING_DESTROY_ALLOW_EXTERNAL)
        caml_failwith("bigstring_destroy: bigstring is external or already deallocated");
      break;
    case CAML_BA_MANAGED :
      check_bigstring_proxy(b);
      free(b->data);
      break;
    case CAML_BA_MAPPED_FILE :
      check_bigstring_proxy(b);
      /* This call to finalize is actually a call to caml_ba_mapped_finalize
         (the finalize function for *mapped* bigarrays), which will unmap the
         array. (note: this is compatible with OCaml 4.06+) */
      if ((flags & CORE_BIGSTRING_DESTROY_DO_NOT_UNMAP)
          != CORE_BIGSTRING_DESTROY_DO_NOT_UNMAP) {
        if (ops->finalize != NULL) {
          ops->finalize(v);
        }
      }
      break;
  }
  b->data = NULL;
  b->flags = CAML_BA_EXTERNAL;
  for (i = 0; i < b->num_dims; ++i) b->dim[i] = 0;
}

CAMLprim value bigstring_destroy_stub(value v_bstr)
{
  core_bigstring_destroy(v_bstr, 0);
  return Val_unit;
}
