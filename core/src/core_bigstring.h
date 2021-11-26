#ifndef __CORE_BIGSTRING_H
#define __CORE_BIGSTRING_H

#include <caml/bigarray.h>

/* Bigarray flags for creating a [Bigstring.t] */
#define CORE_BIGSTRING_FLAGS (CAML_BA_CHAR | CAML_BA_C_LAYOUT)

/* Do not call [unmap] for bigstrings with kind [CAML_BA_MAPPED_FILE] */
#define CORE_BIGSTRING_DESTROY_DO_NOT_UNMAP   1

/* Don't fail on bigstring with kind [CAML_BA_EXTERNAL] */
#define CORE_BIGSTRING_DESTROY_ALLOW_EXTERNAL 2

/* Destroy a bigstring:

   - free the memory with [free] if it is managed by ocaml
   - reset all its dimmensions to 0
   - [unmap] if it is a memory-map
   - set its kind to [CAML_BA_EXTERNAL]
*/
void core_bigstring_destroy(value v, int flags);

#endif /* __CORE_BIGSTRING_H */
