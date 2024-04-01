//Provides:should_use_timezone_js_loader const (const)
function should_use_timezone_js_loader(yes, _platform_not_supported, disabled) {
  if (globalThis.DISABLE_TIMEZONE_JS_LOADER === undefined) {
    return yes;
  } else {
    return disabled;
  }
}


//Provides:timezone_js_loader_disable_for_testing
function timezone_js_loader_disable_for_testing() {
  globalThis.DISABLE_TIMEZONE_JS_LOADER = true;
}

//Provides:timezone_js_loader_enable_for_testing
function timezone_js_loader_enable_for_testing() {
  globalThis.DISABLE_TIMEZONE_JS_LOADER = undefined;
}

//Provides:timezone_js_loader_create_zone
//Requires:caml_jsstring_of_string
function timezone_js_loader_create_zone(zone_name) {
  zone_name = caml_jsstring_of_string(zone_name);
  return new globalThis.TemporalPolyfill.Temporal.TimeZone(zone_name);
}

//Provides:timezone_js_loader_get_next_transition_or_this_time_if_none
function timezone_js_loader_get_next_transition_or_this_time_if_none(zone, instant) {
  var ret = zone.getNextTransition(instant);
  if (ret) {
    return ret;
  } else {
    return instant;
  }
}

//Provides:timezone_js_loader_from_epoch_seconds
//Requires:caml_int64_to_float
function timezone_js_loader_from_epoch_seconds(seconds) {
  return globalThis.TemporalPolyfill.Temporal.Instant.fromEpochSeconds(caml_int64_to_float(seconds));
}

//Provides:timezone_js_loader_compare_instants
function timezone_js_loader_compare_instants(a, b) {
  return globalThis.TemporalPolyfill.Temporal.Instant.compare(a, b);
}

//Provides:timezone_js_loader_epoch_seconds
//Requires:caml_int64_of_float
function timezone_js_loader_epoch_seconds(instant) {
  return caml_int64_of_float(instant.epochSeconds);
}

//Provides:timezone_js_loader_now
function timezone_js_loader_now() {
  return globalThis.TemporalPolyfill.Temporal.Now.instant();
}

//Provides:timezone_js_loader_instant_plus_hours
//Requires:caml_int64_to_float
function timezone_js_loader_instant_plus_hours(instant, hours) {
  hours = caml_int64_to_float(hours);
  var duration = globalThis.TemporalPolyfill.Temporal.Duration.from({ hours: hours });
  return instant.add(duration);
}

//Provides:timezone_js_loader_get_offset_nanos_for
//Requires:caml_int64_of_float
function timezone_js_loader_get_offset_nanos_for(zone, instant) {
  return caml_int64_of_float(zone.getOffsetNanosecondsFor(instant));
}
