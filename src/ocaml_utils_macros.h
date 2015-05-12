
#ifndef ocaml_utils_macros_h
#define ocaml_utils_macros_h

#include <caml/mlvalues.h>

#define UNUSED __attribute__((unused))

#define Is_string(v) (Is_block(v) && Tag_val(v) == String_tag)

#define Is_none(v) (Is_long(v) && Long_val(v) == 0)
#define Is_some(v) (Is_block(v) && Tag_val(v) == 0 && Wosize_val(v) == 1)

#define Is_int_option(v) (Is_none(v) || (Is_some(v) && Is_long(Field(v, 0))))
#define Is_string_option(v) (Is_none(v) || (Is_some(v) && Is_string(Field(v, 0))))

#define Is_custom(v) (Is_block(v) && Tag_val(v) == Custom_tag)
#define Custom_block_name(v) (Custom_ops_val(v)->identifier)

#define Is_custom_named(v, name) \
  (Is_custom(v) && !strcmp_not_a_macro(name, Custom_block_name(v)))

#endif
