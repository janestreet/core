open Base
module C = Configurator

let posix_timers_code = {|
#include <time.h>

int main()
{
  struct timespec ts;
  clock_gettime(CLOCK_REALTIME, &ts);
  clock_settime(CLOCK_REALTIME, &ts);
  clock_getres(CLOCK_REALTIME, &ts);
  return 0;
}
|}

let timerfd_code = {|
#include <sys/timerfd.h>

int main()
{
  timerfd_create(0, 0);
  return 0;
}
|}

let wordexp_code = {|
#include <wordexp.h>

int main()
{
  wordexp_t w;
  wordexp("", &w, 0);
  return 0;
}
|}

let sys_gettid_code = {|
#define _GNU_SOURCE
#include <unistd.h>
#include <sys/syscall.h>

int main ()
{
  syscall(SYS_gettid);
  return 0;
}
|}

let getthrid_code = {|
#include <sys/types.h>
#include <unistd.h>

/* Advice from Philip Guenther on the ocaml-core mailing list is that we need to prototype
 * this ourselves :(
 * See: https://groups.google.com/forum/#!topic/ocaml-core/51knlnuJ8MM */
extern pid_t getthrid(void);

int main()
{
  getthrid();
  return 0;
}
|}

let msg_nosignal_code = {|
#include <sys/types.h>
#include <sys/socket.h>

int main()
{
   send(0, "", 0, MSG_NOSIGNAL);
   return 0;
}
|}

let mutex_timed_lock_code = {|
#include <pthread.h>
#include <time.h>

int main()
{
   pthread_mutex_t m;
   struct timespec ts;
   pthread_mutex_timedlock(&m, &ts);
   return 0;
}
|}

let fdatasync_code = {|
#include <unistd.h>

int main()
{
  fdatasync(0);
  return 0;
}
|}

let thread_cputime_code = {|
#include <pthread.h>
#include <time.h>

int main()
{
   clockid_t clock;
   pthread_getcpuclockid(pthread_self(), &clock);
   return 0;
}
|}

let recvmmsg_code = {|
#define _GNU_SOURCE
#include <sys/socket.h>

int main () {
  recvmmsg(0, 0, 0, 0, 0);
  return 0;
}
|}

let mkostemp_code = {|
#include <stdlib.h>

int main () {
  mkostemp("", 0);
  return 0;
}
|}

let () =
  C.main ~name:"core" (fun c ->
    let posix_timers =
      C.c_test c posix_timers_code ~link_flags:["-lrt"] ||
      C.c_test c posix_timers_code
    in

    let thread_id_method =
      if C.c_test c sys_gettid_code then
        1
      else if C.c_test c getthrid_code then
        2
      else
        -1
    in

    let linux = String.equal (C.ocaml_config_var_exn c "system") "linux" in

    let simple_vars =
      List.map ~f:(fun (v, code, link_flags) ->
        (v, C.C_define.Value.Switch (C.c_test c code ~link_flags)))
        [ "TIMERFD"          , timerfd_code          , []
        ; "WORDEXP"          , wordexp_code          , []
        ; "MSG_NOSIGNAL"     , msg_nosignal_code     , []
        ; "FDATASYNC"        , fdatasync_code        , []
        ; "RECVMMSG"         , recvmmsg_code         , []
        ; "MUTEX_TIMED_LOCK" , mutex_timed_lock_code , ["-lpthread"]
        ; "THREAD_CPUTIME"   , thread_cputime_code   , ["-lpthread"]
        ; "MKOSTEMP"         , mkostemp_code         , []
        ]
    in

    let arch_vars : (string * C.C_define.Value.t) list =
      let target = C.ocaml_config_var_exn c "target" in
      [ "ARCH_x86_64", Switch (String.is_prefix target ~prefix:"x86_64")
      ; "ARCH_i386"  , Switch (String.is_prefix target ~prefix:"i386"  )
      ]
    in

    let rlimit_vars =
      C.C_define.import c ~includes:["sys/resource.h"]
        [ "RLIMIT_AS"  , Switch
        ; "RLIMIT_NICE", Switch
        ]
    in

    let ocaml_vars =
      C.C_define.import c ~includes:["caml/config.h"]
        [ "ARCH_BIG_ENDIAN", Switch
        ; "ARCH_SIXTYFOUR" , Switch
        ]
    in

    let vars =
      List.concat
        [ rlimit_vars
        ; ocaml_vars
        ; simple_vars
        ; arch_vars
        ; [ "POSIX_TIMERS"    , Switch posix_timers
          ; "THREAD_ID_METHOD", Int thread_id_method
          ; "LINUX_EXT"       , Switch linux
          ]
        ]
    in

    (* Make sure things are consistent *)
    [%test_eq: C.C_define.Value.t]
      (List.Assoc.find_exn vars "ARCH_SIXTYFOUR")
      (List.Assoc.find_exn vars "ARCH_x86_64");

    let jsc_vars =
      List.map vars ~f:(fun (name, v) -> ("JSC_" ^ name, v))
    in

    C.C_define.gen_header_file c ~fname:"config.h" jsc_vars)
