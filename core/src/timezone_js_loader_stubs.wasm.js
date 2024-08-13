//Provides: timezone_js_for_wasm_loader_create_zone
function timezone_js_for_wasm_loader_create_zone(zone_name) {
    return new globalThis.TemporalPolyfill.Temporal.TimeZone(zone_name);
}

//Provides: timezone_js_for_wasm_loader_get_next_transition_or_this_time_if_none
function timezone_js_for_wasm_loader_get_next_transition_or_this_time_if_none(zone, instant) {
    var ret = zone.getNextTransition(instant);
    if (ret) {
        return ret;
    } else {
        return instant;
    }
}

//Provides: timezone_js_for_wasm_loader_from_epoch_seconds
function timezone_js_for_wasm_loader_from_epoch_seconds(seconds) {
    return globalThis.TemporalPolyfill.Temporal.Instant.fromEpochSeconds(seconds);
}

//Provides: timezone_js_for_wasm_loader_compare_instants
function timezone_js_for_wasm_loader_compare_instants(a, b) {
    return globalThis.TemporalPolyfill.Temporal.Instant.compare(a, b);
}

//Provides: timezone_js_for_wasm_loader_epoch_seconds
function timezone_js_for_wasm_loader_epoch_seconds(instant) {
    return instant.epochSeconds;
}

//Provides: timezone_js_for_wasm_loader_now
function timezone_js_for_wasm_loader_now() {
    return globalThis.TemporalPolyfill.Temporal.Now.instant();
}

//Provides: timezone_js_for_wasm_loader_instant_plus_hours
//Requires: caml_int64_to_float
function timezone_js_for_wasm_loader_instant_plus_hours(instant, hours) {
    var duration = globalThis.TemporalPolyfill.Temporal.Duration.from({ hours: hours });
    return instant.add(duration);
}

//Provides: timezone_js_for_wasm_loader_get_offset_nanos_for
function timezone_js_for_wasm_loader_get_offset_nanos_for(zone, instant) {
    return zone.getOffsetNanosecondsFor(instant);
}
