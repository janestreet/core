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
if(tz){
  if(!joo_global_object.jsoo_static_env)
    joo_global_object.jsoo_static_env = {};
  joo_global_object.jsoo_static_env.TZ = tz;
}
