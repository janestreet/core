(module
   (import "env" "caml_array_blit"
      (func $caml_array_blit
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "caml_floatarray_blit"
      (func $caml_floatarray_blit
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "caml_js_global"
      (func $caml_js_global (param (ref eq)) (result (ref eq))))
   (import "env" "caml_js_get"
      (func $caml_js_get (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "env" "caml_js_new"
      (func $caml_js_new (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "env" "caml_js_fun_call"
      (func $caml_js_fun_call
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "env" "caml_js_from_array"
      (func $caml_js_from_array (param (ref eq)) (result (ref eq))))
   (import "env" "caml_js_from_float"
      (func $caml_js_from_float (param (ref eq)) (result (ref eq))))
   (import "env" "caml_pure_js_expr"
      (func $caml_pure_js_expr (param (ref eq)) (result (ref eq))))
   (import "env" "wrap" (func $wrap (param anyref) (result (ref eq))))
   (import "env" "caml_string_of_jsstring"
      (func $caml_string_of_jsstring (param (ref eq)) (result (ref eq))))
   (import "env" "caml_jsstring_of_string"
      (func $caml_jsstring_of_string (param (ref eq)) (result (ref eq))))
   (import "env" "caml_md5_chan"
      (func $caml_md5_chan (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "env" "caml_ml_open_descriptor_in"
      (func $caml_ml_open_descriptor_in (param (ref eq)) (result (ref eq))))
   (import "env" "ocaml_exception" (tag $ocaml_exception (param (ref eq))))
   (import "env" "caml_create_bytes"
      (func $caml_create_bytes (param (ref eq)) (result (ref eq))))
   (import "env" "caml_blit_string"
      (func $caml_blit_string
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "caml_md5_string"
      (func $caml_md5_string
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "env" "caml_bigstring_blit_ba_to_bytes"
      (func $bigstring_blit_bigstring_bytes_stub
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "caml_invalid_argument"
      (func $caml_invalid_argument (param (ref eq))))
   (import "env" "caml_ba_alloc"
      (func $caml_ba_alloc
         (param i32) (param i32) (param i32) (param (ref extern))
         (param (ref $int_array))
         (result (ref eq))))
   (import "env" "caml_ba_get_kind"
      (func $caml_ba_get_kind (param (ref eq)) (result i32)))
   (import "env" "caml_ba_get_layout"
      (func $caml_ba_get_layout (param (ref eq)) (result i32)))
   (import "env" "caml_ba_create_buffer"
      (func $caml_ba_create_buffer
         (param i32) (param i32) (result (ref extern))))
   (import "env" "caml_ba_get_data"
      (func $caml_ba_get_data (param (ref eq)) (result (ref extern))))
   (import "env" "caml_ba_set_data"
      (func $caml_ba_set_data (param (ref eq)) (param (ref extern))))
   (import "env" "caml_ba_get_dim"
      (func $caml_ba_get_dim (param (ref eq)) (result (ref $int_array))))
   (import "env" "caml_ba_sub"
       (func $caml_ba_sub
          (param (ref eq)) (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "env" "caml_ba_blit"
       (func $caml_ba_blit (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "env" "caml_ba_dim_1"
       (func $caml_ba_dim_1 (param (ref eq)) (result (ref eq))))

   (export "core_array_unsafe_int_blit" (func $caml_array_blit))
   (export "core_array_unsafe_float_blit" (func $caml_floatarray_blit))

   (type $block (array (mut (ref eq))))
   (type $string (array (mut i8)))
   (type $float (struct (field f64)))

   (data $Date "Date")
   (data $strftime "strftime")

   (func (export "core_gc_compactions") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "core_gc_heap_chunks") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "core_gc_heap_words") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "core_gc_major_collections") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "core_gc_major_plus_minor_words")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "core_gc_major_words") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "core_gc_minor_collections") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "core_gc_minor_words") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "core_gc_promoted_words") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "core_gc_top_heap_words") (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "core_gc_run_memprof_callbacks")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "core_md5_fd") (param $fd (ref eq)) (result (ref eq))
      (local $ic (ref eq))
      (local $s (ref eq))
      (local.set $ic (call $caml_ml_open_descriptor_in (local.get $fd)))
      (local.set $s
         (call $caml_md5_chan (local.get $ic) (ref.i31 (i32.const -1))))
      (return (local.get $s)))

   (func (export "core_md5_digest_subbigstring")
      (param $buf (ref eq)) (param $ofs (ref eq)) (param $len (ref eq))
      (param $res (ref eq)) (result (ref eq))
      (local $bytes (ref eq))
      (local $res2 (ref eq))
      (local.set $bytes (call $caml_create_bytes (local.get $len)))
      (drop
         (call $bigstring_blit_bigstring_bytes_stub
            (local.get $buf) (local.get $ofs) (local.get $bytes)
            (ref.i31 (i32.const 0)) (local.get $len)))
      (local.set $res2
         (call $caml_md5_string
            (local.get $bytes) (ref.i31 (i32.const 0)) (local.get $len)))
      (drop
         (call $caml_blit_string
           (local.get $res2) (ref.i31 (i32.const 0))
           (local.get $res) (ref.i31 (i32.const 0))
           (ref.i31 (i32.const 16))))
     (ref.i31 (i32.const 0)))

   (type $int_array (array (mut i32)))

   (data $bigstring_destroy_already_deallocated
      "bigstring_destroy: bigstring is already deallocated")

   (func $bigstring_destroy_stub (export "bigstring_destroy_stub")
      (param $v (ref eq)) (result (ref eq))
      (if (ref.test (ref i31)
            (extern.internalize (call $caml_ba_get_data (local.get $v))))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $bigstring_destroy_already_deallocated
                  (i32.const 0) (i32.const 51)))))
      (call $caml_ba_set_data (local.get $v)
         (extern.externalize (ref.i31 (i32.const 0))))
      (array.set $int_array (call $caml_ba_get_dim (local.get $v)) (i32.const 0)
         (i32.const 0))
      (ref.i31 (i32.const 0)))

   (data $bigstring_realloc_already_deallocated
      "bigstring_realloc: bigstring is already deallocated")

   (func (export "bigstring_realloc")
      (param $bigstring (ref eq)) (param $vsize (ref eq))
      (result (ref eq))
      (local $new_bigstring (ref eq))
      (local $size i32) (local $old_size i32)
      (local $new_data (ref extern))
      (local.set $size (i31.get_u (ref.cast (ref i31) (local.get $vsize))))
      (if (ref.test (ref i31)
            (extern.internalize
               (call $caml_ba_get_data (local.get $bigstring))))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $bigstring_realloc_already_deallocated
                  (i32.const 0) (i32.const 51)))))
      (local.set $new_data
         (call $caml_ba_create_buffer
            (call $caml_ba_get_kind (local.get $bigstring))
            (local.get $size)))
      (local.set $new_bigstring
         (call $caml_ba_alloc
            (call $caml_ba_get_kind (local.get $bigstring))
            (call $caml_ba_get_layout (local.get $bigstring))
            (i32.const 1)
            (local.get $new_data)
            (array.new_fixed $int_array 1 (local.get $size))))
      (local.set $old_size
         (i31.get_u
            (ref.cast (ref i31) (call $caml_ba_dim_1 (local.get $bigstring)))))
      (if (i32.lt_u (local.get $old_size) (local.get $size))
         (then
            (local.set $size (local.get $old_size))))
      (drop (call $caml_ba_blit
         (call $caml_ba_sub (local.get $bigstring)
            (ref.i31 (i32.const 0)) (ref.i31 (local.get $size)))
         (call $caml_ba_sub (local.get $new_bigstring)
            (ref.i31 (i32.const 0)) (ref.i31 (local.get $size)))))
      (drop (call $bigstring_destroy_stub (local.get $bigstring)))
      (local.get $new_bigstring))
)
