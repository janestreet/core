#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <time.h>

CAMLprim value core_time_ns_format_tm(struct tm *tm, value v_fmt) {
  size_t len;
  char *buf;
  int buf_len;
  value v_str;
  /* 100 * length should be large enough to contain the output of strftime. The
     longest expansion we know of is "%c" in the km_KH.utf8 locale, which
     requires 151 bytes. */
  buf_len = 100 * caml_string_length(v_fmt);
  buf = malloc(buf_len);
  if (!buf)
    caml_failwith("core_time_ns_format_tm: malloc failed");
  len = strftime(buf, buf_len, String_val(v_fmt), tm);

  if (len == 0) {
    /* From the man page:
         "Note that the return value 0 does not necessarily indicate an error;
          for example, in many locales %p yields an empty string."
       Given how large our buffer is we just assume that 0 always indicates
       an empty string. */
    v_str = caml_copy_string("");
    free(buf);
    return v_str;
  }

  v_str =
      caml_copy_string(buf); /* [strftime] always null terminates the string */
  free(buf);
  return v_str;
}

CAMLprim value core_time_ns_format(value t, value v_fmt) {
  time_t clock;
  struct tm *tm;
  clock = (time_t)Double_val(t);
  /* This [tm] must not be freed. It refers to statically allocated
     memory and its contents change every time [localtime] is
     called. */
  tm = localtime(&clock);
  if (tm == NULL)
    caml_failwith("core_time_ns_format: localtime failed");
  return core_time_ns_format_tm(tm, v_fmt);
}
