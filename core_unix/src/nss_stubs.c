/*$ open Core_nss_cinaps $*/

#define _GNU_SOURCE

#include <string.h>
#include <sys/types.h>
#include <grp.h>
#include <pwd.h>
#include <assert.h>
#include <errno.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/signals.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/unixsupport.h>


static value pw_entry_alloc(struct passwd *entry)
{
  CAMLparam0();
  CAMLlocal1(res);
  CAMLlocal3(name, passwd, gecos);
  CAMLlocal2(dir, shell);

  res = caml_alloc_tuple(7);
  Store_field(res, 0, caml_copy_string(entry->pw_name));
  Store_field(res, 1, caml_copy_string(entry->pw_passwd));
  Store_field(res, 2, Val_int(entry->pw_uid));
  Store_field(res, 3, Val_int(entry->pw_gid));
  Store_field(res, 4, caml_copy_string(entry->pw_gecos));
  Store_field(res, 5, caml_copy_string(entry->pw_dir));
  Store_field(res, 6, caml_copy_string(entry->pw_shell));

  CAMLreturn(res);
}

static value gr_entry_alloc (struct group *entry)
{
  CAMLparam0();
  CAMLlocal1(res);
  CAMLlocal3(name, passwd, mem);

  res = caml_alloc_tuple(4);
  Store_field(res, 0, caml_copy_string(entry->gr_name));
  Store_field(res, 1, caml_copy_string(entry->gr_passwd));
  Store_field(res, 2, Val_int(entry->gr_gid));
  Store_field(res, 3, caml_copy_string_array((const char **)entry->gr_mem));

  CAMLreturn(res);
}


/*$ generate gr gid */

CAMLprim value core_unix_getgrgid_r(value v_gid, value v_buf)
{
  CAMLparam2(v_gid, v_buf);
  CAMLlocal1(res);
  char * buf = (char *) Caml_ba_data_val(v_buf);
  size_t buflen = Caml_ba_array_val(v_buf)->dim[0];
  gid_t gid = Int_val(v_gid);
  struct group entry;
  struct group *result;
  int retval;

  caml_enter_blocking_section();
  retval = getgrgid_r(gid, &entry, buf, buflen, &result);
  caml_leave_blocking_section();
  if(retval) {
    unix_error(retval, "getgrgid_r", caml_alloc_sprintf("%d", Int_val(v_gid)));
  }
  else {
    if(!result) {
      caml_raise_not_found();
    } else {
      assert(result == &entry);
      res = gr_entry_alloc(&entry);
      CAMLreturn(res);
    }
  }
}


/*$ generate pw uid */

CAMLprim value core_unix_getpwuid_r(value v_uid, value v_buf)
{
  CAMLparam2(v_uid, v_buf);
  CAMLlocal1(res);
  char * buf = (char *) Caml_ba_data_val(v_buf);
  size_t buflen = Caml_ba_array_val(v_buf)->dim[0];
  uid_t uid = Int_val(v_uid);
  struct passwd entry;
  struct passwd *result;
  int retval;

  caml_enter_blocking_section();
  retval = getpwuid_r(uid, &entry, buf, buflen, &result);
  caml_leave_blocking_section();
  if(retval) {
    unix_error(retval, "getpwuid_r", caml_alloc_sprintf("%d", Int_val(v_uid)));
  }
  else {
    if(!result) {
      caml_raise_not_found();
    } else {
      assert(result == &entry);
      res = pw_entry_alloc(&entry);
      CAMLreturn(res);
    }
  }
}


/*$ generate pw nam */

CAMLprim value core_unix_getpwnam_r(value v_nam, value v_buf)
{
  CAMLparam2(v_nam, v_buf);
  CAMLlocal1(res);
  char * buf = (char *) Caml_ba_data_val(v_buf);
  size_t buflen = Caml_ba_array_val(v_buf)->dim[0];
  const char * nam = Caml_ba_data_val(v_nam);
  struct passwd entry;
  struct passwd *result;
  int retval;

  caml_enter_blocking_section();
  retval = getpwnam_r(nam, &entry, buf, buflen, &result);
  caml_leave_blocking_section();
  if(retval) {
    unix_error(retval, "getpwnam_r", v_nam);
  }
  else {
    if(!result) {
      caml_raise_not_found();
    } else {
      assert(result == &entry);
      res = pw_entry_alloc(&entry);
      CAMLreturn(res);
    }
  }
}


/*$ generate gr nam */

CAMLprim value core_unix_getgrnam_r(value v_nam, value v_buf)
{
  CAMLparam2(v_nam, v_buf);
  CAMLlocal1(res);
  char * buf = (char *) Caml_ba_data_val(v_buf);
  size_t buflen = Caml_ba_array_val(v_buf)->dim[0];
  const char * nam = Caml_ba_data_val(v_nam);
  struct group entry;
  struct group *result;
  int retval;

  caml_enter_blocking_section();
  retval = getgrnam_r(nam, &entry, buf, buflen, &result);
  caml_leave_blocking_section();
  if(retval) {
    unix_error(retval, "getgrnam_r", v_nam);
  }
  else {
    if(!result) {
      caml_raise_not_found();
    } else {
      assert(result == &entry);
      res = gr_entry_alloc(&entry);
      CAMLreturn(res);
    }
  }
}


