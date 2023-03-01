#include <caml/address_class.h>
#include <caml/mlvalues.h>

CAMLprim value core_heap_block_is_heap_block(value v) {
  return (Is_block(v) && Is_in_value_area(v) && Tag_val(v) != Lazy_tag &&
          Tag_val(v) != Double_tag && Tag_val(v) != Forward_tag)
             ? Val_true
             : Val_false;
}
