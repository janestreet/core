//Provides: timezone_js_for_wasm_loader_ensure_temporal
function timezone_js_for_wasm_loader_ensure_temporal() {
    if (globalThis.Temporal === undefined) {
        globalThis.Temporal = globalThis.TemporalPolyfill.Temporal;
    }
}

//Provides:timezone_js_for_wasm_loader_validate_zone_name
//Requires:timezone_js_for_wasm_loader_ensure_temporal
function timezone_js_for_wasm_loader_validate_zone_name(zone_name) {
    timezone_js_for_wasm_loader_ensure_temporal();
    globalThis.Temporal.Now.instant().toZonedDateTimeISO(zone_name);
    return zone_name;
}

//Provides: timezone_js_for_wasm_loader_get_next_transition_or_this_time_if_none
function timezone_js_for_wasm_loader_get_next_transition_or_this_time_if_none(zone_name, instant) {
    var zdt = instant.toZonedDateTimeISO(zone_name);
    var next = zdt.getTimeZoneTransition("next");
    if (next) {
        return next.toInstant();
    } else {
        return instant;
    }
}

//Provides: timezone_js_for_wasm_loader_from_epoch_seconds
//Requires: timezone_js_for_wasm_loader_ensure_temporal
function timezone_js_for_wasm_loader_from_epoch_seconds(seconds) {
    timezone_js_for_wasm_loader_ensure_temporal();
    return globalThis.Temporal.Instant.fromEpochMilliseconds(seconds * 1000);
}

//Provides: timezone_js_for_wasm_loader_compare_instants
//Requires: timezone_js_for_wasm_loader_ensure_temporal
function timezone_js_for_wasm_loader_compare_instants(a, b) {
    timezone_js_for_wasm_loader_ensure_temporal();
    return globalThis.Temporal.Instant.compare(a, b);
}

//Provides: timezone_js_for_wasm_loader_epoch_seconds
function timezone_js_for_wasm_loader_epoch_seconds(instant) {
    return Math.floor(instant.epochMilliseconds / 1000);
}

//Provides: timezone_js_for_wasm_loader_now
//Requires: timezone_js_for_wasm_loader_ensure_temporal
function timezone_js_for_wasm_loader_now() {
    timezone_js_for_wasm_loader_ensure_temporal();
    return globalThis.Temporal.Now.instant();
}

//Provides: timezone_js_for_wasm_loader_instant_plus_hours
//Requires: caml_int64_to_float, timezone_js_for_wasm_loader_ensure_temporal
function timezone_js_for_wasm_loader_instant_plus_hours(instant, hours) {
    timezone_js_for_wasm_loader_ensure_temporal();
    var duration = globalThis.Temporal.Duration.from({ hours: hours });
    return instant.add(duration);
}

//Provides: timezone_js_for_wasm_loader_get_offset_nanos_for
function timezone_js_for_wasm_loader_get_offset_nanos_for(zone_name, instant) {
    var zdt = instant.toZonedDateTimeISO(zone_name);
    return zdt.offsetNanoseconds;
}
