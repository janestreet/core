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

//Provides:timezone_js_loader_ensure_temporal
function timezone_js_loader_ensure_temporal() {
  if (globalThis.Temporal === undefined) {
    globalThis.Temporal = globalThis.TemporalPolyfill.Temporal;
  }
}

//Provides:timezone_js_loader_validate_zone_name
//Requires:caml_jsstring_of_string,timezone_js_loader_ensure_temporal
function timezone_js_loader_validate_zone_name(zone_name) {
  timezone_js_loader_ensure_temporal();
  globalThis.Temporal.Now.instant().toZonedDateTimeISO(caml_jsstring_of_string(zone_name));
  return zone_name;
}

//Provides:timezone_js_loader_get_next_transition_or_this_time_if_none
//Requires:caml_jsstring_of_string
function timezone_js_loader_get_next_transition_or_this_time_if_none(zone_name, instant) {
  var zone = caml_jsstring_of_string(zone_name);
  var zdt = instant.toZonedDateTimeISO(zone);
  var next = zdt.getTimeZoneTransition("next");
  if (next) {
    return next.toInstant();
  } else {
    return instant;
  }
}

//Provides:timezone_js_loader_from_epoch_seconds
//Requires:caml_int64_to_float,timezone_js_loader_ensure_temporal
function timezone_js_loader_from_epoch_seconds(seconds) {
  timezone_js_loader_ensure_temporal();
  return globalThis.Temporal.Instant.fromEpochMilliseconds(caml_int64_to_float(seconds) * 1000);
}

//Provides:timezone_js_loader_compare_instants
//Requires:timezone_js_loader_ensure_temporal
function timezone_js_loader_compare_instants(a, b) {
  timezone_js_loader_ensure_temporal();
  return globalThis.Temporal.Instant.compare(a, b);
}

//Provides:timezone_js_loader_epoch_seconds
//Requires:caml_int64_of_float
function timezone_js_loader_epoch_seconds(instant) {
  return caml_int64_of_float(Math.floor(instant.epochMilliseconds / 1000));
}

//Provides:timezone_js_loader_now
//Requires:timezone_js_loader_ensure_temporal
function timezone_js_loader_now() {
  timezone_js_loader_ensure_temporal();
  return globalThis.Temporal.Now.instant();
}

//Provides:timezone_js_loader_instant_plus_hours
//Requires:caml_int64_to_float,timezone_js_loader_ensure_temporal
function timezone_js_loader_instant_plus_hours(instant, hours) {
  timezone_js_loader_ensure_temporal();
  hours = caml_int64_to_float(hours);
  var duration = globalThis.Temporal.Duration.from({ hours: hours });
  return instant.add(duration);
}

//Provides:timezone_js_loader_get_offset_nanos_for
//Requires:caml_int64_of_float,caml_jsstring_of_string
function timezone_js_loader_get_offset_nanos_for(zone_name, instant) {
  var zone = caml_jsstring_of_string(zone_name);
  var zdt = instant.toZonedDateTimeISO(zone);
  return caml_int64_of_float(zdt.offsetNanoseconds);
}
