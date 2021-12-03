#include <assert.h>
#include <string.h>
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>

CAMLprim value core_array_unsafe_int_blit(value src, value src_pos,
                                          value dst, value dst_pos, value len)
{
  /* On 32bit boxes ocaml values are 32bits long. On 64bit boxes OCaml
     values are 64bits long. The value type will change its size
     accordingly and hence the following macro works.
   */
  memmove(&Field(dst, Long_val(dst_pos)),
          &Field(src, Long_val(src_pos)),
          Long_val(len) * sizeof(value));

  return Val_unit;
}

CAMLprim value core_array_unsafe_float_blit(value src, value src_pos,
                                            value dst, value dst_pos, value len)
{
  /* On both 32bit and 64bit boxes, floats are 64bits long and type
     casting the pointer to double achieves this.
  */
  memmove((double *)dst + Long_val(dst_pos),
          (double *)src + Long_val(src_pos),
          Long_val(len) * sizeof(double));

  return Val_unit;
}
