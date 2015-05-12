#include <string.h>
#include <syslog.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/fail.h>

#define Val_none Val_int(0)
#define Some_val(v) Field(v, 0)

static int log_open_options[] = {
  /* THESE MUST STAY IN THE SAME ORDER AS IN syslog.ml!!! */
  LOG_PID, LOG_CONS, LOG_ODELAY, LOG_NDELAY, LOG_NOWAIT, LOG_PERROR
};

CAMLprim value core_syslog_open_option_to_int(value v_open_option) {
  return Val_int(log_open_options[Int_val(v_open_option)]);
}

static int log_facilities[] = {
  /* THESE MUST STAY IN THE SAME ORDER AS IN syslog.ml!!! */
  LOG_KERN, LOG_USER, LOG_MAIL, LOG_DAEMON, LOG_AUTH, LOG_SYSLOG, LOG_LPR, LOG_NEWS,
  LOG_UUCP, LOG_CRON, LOG_AUTHPRIV, LOG_FTP,
  LOG_LOCAL0,
  LOG_LOCAL1,
  LOG_LOCAL2,
  LOG_LOCAL3,
  LOG_LOCAL4,
  LOG_LOCAL5,
  LOG_LOCAL6,
  LOG_LOCAL7
};

CAMLprim value core_syslog_facility_to_int(value v_facility) {
  return Val_int(log_facilities[Int_val(v_facility)]);
}

static int log_levels[] = {
  /* THESE MUST STAY IN THE SAME ORDER AS IN syslog.ml!!! */
  LOG_EMERG, LOG_ALERT, LOG_CRIT, LOG_ERR, LOG_WARNING, LOG_NOTICE, LOG_INFO, LOG_DEBUG
};

CAMLprim value core_syslog_level_to_int(value v_level) {
  return Val_int(log_levels[Int_val(v_level)]);
}

/* XXX: WARNING: this function leaks memory if v_ident is not None!
   No way around that if syslog is called in a multi-threaded environment!
   Therefore it shouldn't be called too often.  What for, anyway? */
CAMLprim value core_syslog_openlog(value v_ident, value v_open_option, value v_facility) {
  char *ident = NULL; /* default to argv[0], as per syslog(3) */
  if (v_ident != Val_none) {
    int len = caml_string_length(Some_val(v_ident)) + 1;
    ident = caml_stat_alloc(len);
    memcpy(ident, String_val(Some_val(v_ident)), len);
  }
  caml_enter_blocking_section();
  openlog(ident, Int_val(v_open_option), Int_val(v_facility));
  /* openlog doesn't inter ident (if specified), so freeing it here
     would create an invalid program. */
  caml_leave_blocking_section();
  return Val_unit;
}

/* A priority is a level | facility.  See syslog(3). */
CAMLprim value core_syslog_syslog(value v_priority, value v_message) {
  int len = caml_string_length(v_message) + 1;
  char *message = caml_stat_alloc(len);
  memcpy(message, String_val(v_message), len);
  caml_enter_blocking_section();
  syslog(Int_val(v_priority), "%s", message);
  free(message);
  caml_leave_blocking_section();
  return Val_unit;
}

CAMLprim value core_syslog_closelog() {
  closelog();
  return Val_unit;
}

CAMLprim value core_syslog_setlogmask(value v_mask) {
  setlogmask(Int_val(v_mask));
  return Val_unit;
}
