var dateTimeFormat =
  Intl
  && Intl.DateTimeFormat
  && Intl.DateTimeFormat();
var resolvedOptions =
  dateTimeFormat
  && dateTimeFormat.resolvedOptions
  && dateTimeFormat.resolvedOptions();
var tz = resolvedOptions && resolvedOptions.timeZone
// If a timezone is available, set the TZ env variable.
if (tz) {
  if (!globalThis.jsoo_env)
    globalThis.jsoo_env = {};
  globalThis.jsoo_env.TZ = tz;
}
