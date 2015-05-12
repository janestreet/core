#include "ocaml_utils.h"

CAMLextern int caml_rev_convert_signal_number(int);

CAMLprim value ml_caml_to_nonportable_signal_number(value v_signo)
{
  return Val_int(caml_convert_signal_number(Int_val(v_signo)));
}

CAMLprim value ml_nonportable_to_caml_signal_number(value v_signo)
{
  return Val_int(caml_rev_convert_signal_number(Int_val(v_signo)));
}
