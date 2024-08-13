(module
   (import "env" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "env" "unwrap" (func $unwrap (param (ref eq)) (result anyref)))
   (import "env" "caml_string_of_jsstring"
      (func $caml_string_of_jsstring (param (ref eq)) (result (ref eq))))
   (import "env" "caml_jsstring_of_string"
      (func $caml_jsstring_of_string (param (ref eq)) (result (ref eq))))
   (import "env" "caml_callback_1"
      (func $caml_callback_1 (param (ref eq) (ref eq)) (result (ref eq))))
   (import "env" "caml_copy_int64"
      (func $caml_copy_int64 (param i64) (result (ref eq))))
   (import "env" "Int64_val"
      (func $Int64_val (param (ref eq)) (result i64)))

   (import "js" "timezone_js_for_wasm_loader_create_zone"
      (func $timezone_js_for_wasm_loader_create_zone
         (param anyref) (result anyref)))
   (import "js" "timezone_js_for_wasm_loader_get_next_transition_or_this_time_if_none"
      (func $timezone_js_for_wasm_loader_get_next_transition_or_this_time_if_none
         (param anyref anyref) (result anyref)))
   (import "js" "timezone_js_for_wasm_loader_from_epoch_seconds"
      (func $timezone_js_for_wasm_loader_from_epoch_seconds
         (param f64) (result anyref)))
   (import "js" "timezone_js_for_wasm_loader_compare_instants"
      (func $timezone_js_for_wasm_loader_compare_instants
         (param anyref anyref) (result (ref eq))))
   (import "js" "timezone_js_for_wasm_loader_epoch_seconds"
      (func $timezone_js_for_wasm_loader_epoch_seconds
         (param anyref) (result f64)))
   (import "js" "timezone_js_for_wasm_loader_now"
      (func $timezone_js_for_wasm_loader_now (result anyref)))
   (import "js" "timezone_js_for_wasm_loader_instant_plus_hours"
      (func $timezone_js_for_wasm_loader_instant_plus_hours
         (param anyref f64) (result anyref)))
   (import "js" "timezone_js_for_wasm_loader_get_offset_nanos_for"
      (func $timezone_js_for_wasm_loader_get_offset_nanos_for
         (param anyref anyref) (result f64)))

   (func (export "should_use_timezone_js_loader")
      (param $yes (ref eq))
      (param $platform_not_supported (ref eq))
      (param $disabled (ref eq))
      (result (ref eq))
      ;; wasm only supports timezone use through the js loader. File system support isn't
      ;; implemented yet (2024-03) so a fallback to a mock file system would fail.
      (local.get $yes))

   (func (export "timezone_js_loader_disable_for_testing")
      (param $unit (ref eq)) (result (ref eq))
      (local.get $unit))

   (func (export "timezone_js_loader_enable_for_testing")
      (param $unit (ref eq)) (result (ref eq))
      (local.get $unit))

   (func (export "timezone_js_loader_create_zone")
      (param $zone_name (ref eq)) (result (ref eq))
      (return_call $wrap
         (call $timezone_js_for_wasm_loader_create_zone
            (call $unwrap
               (call $caml_jsstring_of_string (local.get $zone_name))))))

   (func (export "timezone_js_loader_get_next_transition_or_this_time_if_none")
      (param $zone (ref eq)) (param $instant (ref eq)) (result (ref eq))
      (return_call $wrap
         (call $timezone_js_for_wasm_loader_get_next_transition_or_this_time_if_none
            (call $unwrap (local.get $zone))
            (call $unwrap (local.get $instant)))))

   (func (export "timezone_js_loader_from_epoch_seconds")
      (param $seconds (ref eq)) (result (ref eq))
      (call $wrap
         (call $timezone_js_for_wasm_loader_from_epoch_seconds
            (f64.convert_i64_s (call $Int64_val (local.get $seconds))))))

   (func (export "timezone_js_loader_compare_instants")
      (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
      (call $timezone_js_for_wasm_loader_compare_instants
         (call $unwrap (local.get $a)) (call $unwrap (local.get $b))))

   (func (export "timezone_js_loader_epoch_seconds")
      (param $instant (ref eq)) (result (ref eq))
      (call $caml_copy_int64
         (i64.trunc_sat_f64_s
            (call $timezone_js_for_wasm_loader_epoch_seconds
               (call $unwrap (local.get $instant))))))

   (func (export "timezone_js_loader_now")
      (param (ref eq)) (result (ref eq))
      (call $wrap (call $timezone_js_for_wasm_loader_now)))

   (func (export "timezone_js_loader_instant_plus_hours")
      (param $instant (ref eq)) (param $hours (ref eq)) (result (ref eq))
      (call $wrap
         (call $timezone_js_for_wasm_loader_instant_plus_hours
            (call $unwrap (local.get $instant))
            (f64.convert_i64_s (call $Int64_val (local.get $hours))))))

   (func (export "timezone_js_loader_get_offset_nanos_for")
      (param $zone (ref eq)) (param $instant (ref eq)) (result (ref eq))
      (call $caml_copy_int64
         (i64.trunc_sat_f64_s
            (call $timezone_js_for_wasm_loader_get_offset_nanos_for
               (call $unwrap (local.get $zone))
               (call $unwrap (local.get $instant))))))
)
