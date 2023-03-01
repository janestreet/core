///////// CORE

//Provides: core_array_unsafe_int_blit
//Requires: caml_array_blit
var core_array_unsafe_int_blit = caml_array_blit
//Provides: core_array_unsafe_float_blit
//Requires: caml_array_blit
var core_array_unsafe_float_blit = caml_array_blit

//Provides: core_time_ns_format
//Requires: caml_jsbytes_of_string, caml_string_of_jsbytes
function core_time_ns_format(time,format){
  var d = new Date(time * 1000);
  var formatjs = caml_jsbytes_of_string(format);
  var jstring = joo_global_object.strftime(formatjs, d);
  return caml_string_of_jsbytes(jstring);
}

//Provides: core_gc_compactions
function core_gc_compactions () { return 0 }
//Provides: core_gc_heap_chunks
function core_gc_heap_chunks () { return 0 }
//Provides: core_gc_heap_words
function core_gc_heap_words () { return 0 }
//Provides: core_gc_major_collections
function core_gc_major_collections () { return 0 }
//Provides: core_gc_major_plus_minor_words
function core_gc_major_plus_minor_words () { return 0 }
//Provides: core_gc_major_words
function core_gc_major_words () { return 0 }
//Provides: core_gc_minor_collections
function core_gc_minor_collections () { return 0 }
//Provides: core_gc_minor_words
function core_gc_minor_words () { return 0 }
//Provides: core_gc_promoted_words
function core_gc_promoted_words () { return 0 }
//Provides: core_gc_top_heap_words
function core_gc_top_heap_words () { return 0 }
//Provides: core_gc_run_memprof_callbacks
function core_gc_run_memprof_callbacks () { return 0 }

//Provides: core_md5_fd
//Requires: caml_ml_open_descriptor_in, caml_md5_chan, caml_ml_close_channel
function core_md5_fd(fd){
    var ic = caml_ml_open_descriptor_in(fd);
    try {
        return caml_md5_chan(ic, -1);
    } finally {
        caml_ml_close_channel(ic);
    }
}

//Provides: core_md5_digest_subbigstring
//Requires: caml_md5_string, caml_blit_string, caml_create_bytes
//Requires: bigstring_blit_bigstring_bytes_stub, caml_string_of_bytes
function core_md5_digest_subbigstring(buf, ofs, len, res){
    var bytes = caml_create_bytes(len);
    bigstring_blit_bigstring_bytes_stub(buf, ofs, bytes, 0, len);
    var res2 = caml_md5_string(caml_string_of_bytes(bytes), 0, len);
    caml_blit_string(res2, 0, res, 0, 16);
    return 0;
}

//Bigstring

//Provides: bigstring_destroy_stub
//Requires: caml_invalid_argument
function bigstring_destroy_stub(v_bstr) {
  if (v_bstr.hasOwnProperty('__is_deallocated')) {
    caml_invalid_argument("bigstring_destroy: bigstring is already deallocated");
  }
  // Mutate the original bigstring in-place, to simulate what the C version does
  v_bstr.__is_deallocated = true;
  v_bstr.data = new v_bstr.data.__proto__.constructor(0);
  v_bstr.dims = [ 0 ];
  return 0;
}

//Provides: bigstring_realloc
//Requires: caml_invalid_argument, caml_ba_create_unsafe, bigstring_destroy_stub
function bigstring_realloc(bigstring, size) {
    if (bigstring.hasOwnProperty('__is_deallocated')) {
        caml_invalid_argument("bigstring_realloc: bigstring is already deallocated");
    }

    var new_data = new bigstring.data.__proto__.constructor(size);
    new_data.set(bigstring.data.slice(0, size));
    var new_bigstring = caml_ba_create_unsafe(bigstring.kind, bigstring.layout, [size], new_data);
    bigstring_destroy_stub(bigstring);

    return new_bigstring;
}
