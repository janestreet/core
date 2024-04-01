#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>

/* [timezone_js_loader_stubs] is intended to be used exclusively from
 * javascript, so the only C stub that actually does anything is the
 * [should_use_timezone_js_loader] function, which returns
 * [platform_not_supported]; all the rest raise. */

CAMLprim value should_use_timezone_js_loader(value yes,
                                             value platform_not_supported,
                                             value disabled) {
  CAMLparam3(yes, platform_not_supported, disabled);
  CAMLreturn(platform_not_supported);
}

CAMLprim value timezone_js_loader_create_zone(value zone_name) {
  CAMLparam1(zone_name);
  caml_failwith("timezone_js_loader: timezone_js_loader_create_zone is not "
                "implemented for the native target");
  CAMLreturn(Val_unit);
}

CAMLprim value timezone_js_loader_get_next_transition_or_this_time_if_none(
    value zone, value instant) {
  CAMLparam2(zone, instant);
  caml_failwith("timezone_js_loader: "
                "timezone_js_loader_get_next_transition_or_this_time_if_none "
                "is not implemented for the native target");
  CAMLreturn(Val_unit);
}

CAMLprim value timezone_js_loader_from_epoch_seconds(value seconds) {
  CAMLparam1(seconds);
  caml_failwith(
      "timezone_js_loader: timezone_js_loader_from_epoch_seconds is not "
      "implemented for the native target");
  CAMLreturn(Val_unit);
}

CAMLprim value timezone_js_loader_compare_instants(value a, value b) {
  CAMLparam2(a, b);
  caml_failwith(
      "timezone_js_loader: timezone_js_loader_compare_instants is not "
      "implemented for the native target");
  CAMLreturn(Val_unit);
}

CAMLprim value timezone_js_loader_epoch_seconds(value instant) {
  CAMLparam1(instant);
  caml_failwith("timezone_js_loader: timezone_js_loader_epoch_seconds is not "
                "implemented for the native target");
  CAMLreturn(Val_unit);
}

CAMLprim value timezone_js_loader_now(value unit) {
  CAMLparam1(unit);
  caml_failwith("timezone_js_loader: timezone_js_loader_now is not "
                "implemented for the native target");
  CAMLreturn(Val_unit);
}

CAMLprim value timezone_js_loader_instant_plus_hours(value instant,
                                                     value hours) {
  CAMLparam2(instant, hours);
  caml_failwith(
      "timezone_js_loader: timezone_js_loader_instant_plus_hours is not "
      "implemented for the native target");
  CAMLreturn(Val_unit);
}

CAMLprim value timezone_js_loader_get_offset_nanos_for(value zone,
                                                       value instant) {
  CAMLparam2(zone, instant);
  caml_failwith(
      "timezone_js_loader: timezone_js_loader_get_offset_nanos_for is not "
      "implemented for the native target");
  CAMLreturn(Val_unit);
}

CAMLprim value timezone_js_loader_enable_for_testing(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_unit);
}

CAMLprim value timezone_js_loader_disable_for_testing(value unit) {
  CAMLparam1(unit);
  CAMLreturn(Val_unit);
}
