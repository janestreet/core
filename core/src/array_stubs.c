#include <assert.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdio.h>
#include <string.h>

CAMLprim value core_array_unsafe_int_blit(value src, value src_pos, value dst,
                                          value dst_pos, value len) {
  /* On 32bit boxes ocaml values are 32bits long. On 64bit boxes OCaml
     values are 64bits long. The value type will change its size
     accordingly and hence the following macro works.
   */

  /* Value casts here remove the volatile quantifier, because
     it can't apply to the entire address range and we need to use memmove.

     This code is safe in 4.14 because this stub only operates
     on int arrays, which can be overwritten without caml_modify. Blitting
     arrays of non-int non-float types calls caml_modify on each field
     (which has now been updated for the 5.0 memory model).

     The memmove here will be UB in 5.0 if another domain
     concurrently uses this memory range, but 1) this stub is already
     unsafe and 2) torn/inconsistent writes of integers will never
     cause the GC to mistakenly traverse a field, since writes at least
     occur in word-size atomic chunks.

     See [https://github.com/ocaml/ocaml/pull/11255].
   */
  memmove((value *)&Field(dst, Long_val(dst_pos)),
          (value *)&Field(src, Long_val(src_pos)),
          Long_val(len) * sizeof(value));

  return Val_unit;
}

CAMLprim value core_array_unsafe_float_blit(value src, value src_pos, value dst,
                                            value dst_pos, value len) {
  /* On both 32bit and 64bit boxes, floats are 64bits long and type
     casting the pointer to double achieves this.
  */
  memmove((double *)dst + Long_val(dst_pos), (double *)src + Long_val(src_pos),
          Long_val(len) * sizeof(double));

  return Val_unit;
}
