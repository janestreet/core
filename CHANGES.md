## 112.06.00

- Renamed `Linux_ext.gettid` as `Unix.gettid`, and added OpenBSD support.

    `SYS_gettid` is not available on OpenBSD, but is used in
    `Core_extended`. See the mailing list discussion about this here:

    https://groups.google.com/forum/#!topic/ocaml-core/51knlnuJ8MM

    Seems like the OpenBSD alternative is:

        pid_t        getthrid(void);

    although it's not defined in any header file, which is a bit unfortunate.

- Added `Piecewise_linear.precache`, which computes a lookup table that
  speeds up subsequent calls to `Piecewise_linear.get`.
- Added `Time_ns` module, representing times as 63-bit integers of
  nanoseconds since the epoch.
- Fixed build of `unix_stubs.c` on OpenBSD.
- In `Daemon`, fixed an error message regarding `WSTOPPED` (fixes #47).
- Added `Time.Span.Stable.V2`, with sexps that use new suffixes for
  microseconds (`us`) and nanoseconds (`ns`).

    `Time.Span.of_string` supports the new format, but
    `Time.Span.to_string` doesn't yet produce it -- we plan to change
    that later, after the new `of_string` has made it out more widely.

- Added `Time.Span.to_string_hum`, which gives more options for
  rendering time spans.
- Merged the `recvmmsg` stubs in `Bigstring` and `Iobuf`.

    Factored out a shared underlying `recvmmsg` call that both
    stubs use.

    Restored `-pedantic` by avoiding a C99 feature (variable-length
    stack arrays).

- Made `Date.t` abstract, and changed its representation from a 4-word
  record to an immediate int (packing year, month, day).
- In `Daemon`, changed the permissions of the `std{err,out}` files
  generated during daemonization from `0o777` to `0o644`.
- Moved `Thread_safe_queue` from `core` to `core_kernel`.

    This was done so that `Async_kernel` can use it, eliminating one of
    `Async_kernel`'s dependencies on `Core`.

    `Thread_safe_queue_unit_tests` remains `Core`, at least for now,
    because it has some dependencies on other stuff in `Core`.

## 112.01.00

- Removed vestigial code supporting OCaml 4.00.
- Added `Command` support for flags that are passed one or more times.

  Added `Command.Spec.one_or_more` and
  `Command.Spec.non_empty_sequence` to deal with the cases where you
  expect a flag or anonymous argument (respectively) to be passed one
  or (optionally) more times.  This is common enough and distinct from
  the case where you want the argument passed zero or more times that
  it seems like we should canonize it in the library.
- In `Lock_file`, made stale lock detection more robust.

  Made `Lock_file.create foo` succeed if `foo` is absent and
  `foo.nfs_lock` file is present and stale.  Previously, it would
  fail.
- Removed `Syslog.syslog`'s `add_stderr` argument; use the `PERROR`
  option instead.
- Fixed `unix_stubs.c` compilation on NetBSD

  Closes #45
- Added `Filename` operators `/^` and `/@`, and `of_parts`, like the
  same functions for Catalog paths.
- Changed `Iobuf` functions that advance the iobuf to not also return
  a redundant number of bytes processed.

  This avoids a small allocation (in the case of the `int option`
  functions) and normalizes the result (so the same information isn't
  returned two ways).  Actually, it doesn't yet avoid the allocation in
  the implementation, as the corresponding `Bigstring` functions must
  still return the number of bytes processed, and currently do so as an
  option.  We hope to eventually change that.

  In the future I expect we will change `unit` to some `error` variant
  to also avoid the exception construction for `EWOULDBLOCK/EAGAIN`.  We
  can even make Unix syscalls `noalloc` if we're careful.
- In `Unix` module, added unit tests for `Cidr.does_match`.

## 111.28.00

- Added `Piecewise_linear.create_from_linear_combination`.

        val create_from_linear_combination : (t * float) list -> t Or_error.t

- Added `Time.is_{earlier,later} : Time.t -> than:Time.t -> bool`, which
  are easier to read than `Time.(<)` and friends.
- Added `Command.exec`, which allows one to include the `Command`
  hierarchy from one executable in another.

    `Command.exec` takes the file path to an executable that uses the
    `Command` module and returns a `Command.t` that integrates the
    executable (by exec'ing it), including providing recursive help and
    autocompletion as if it were a standard `Command.t`.

- Replaced most uses of `Hashtbl.replace` with `Hashtbl.set`.
- Renamed `Float.epsilon` to `robust_comparison_tolerance`, to avoid
  confusion with `epsilon_float`.

## 111.25.00

- Added `Gc.disable_compaction` function.
- Added `Time.to_string_abs_trimmed`, which prints a trimmed time and
  takes a `zone` argument.
- Fixed `unix_stubs.c` to suppress a warning when building with some
  versions of gcc.
- Changed `Time.Zone` to allow the zoneinfo location to be specified
  by an environment variable.

  Closes #40
- Fix compatibility with 4.02

## 111.21.00

- Fixed an issue where `Time.Zone.init` would not properly traverse the
  directory containing timezone information.
- Added `Time.With_utc_sexp`, which has stable serialization of `Time.t`
  that is byte-for-byte equal across timezones.
- Made `Uuid` stable.
- Made `Piecewise_linear` stable.

## 111.17.00

- Fixed a bug in `Bigstring.really_recv` if `recv` doesn't receive all
  the data it wants.

  This bug has been around forever; it may not have caused trouble
  because `Bigstring.really_recv` (1) is barely used (the only use is
  in `Bigstring.unmarshal_from_sock`) and (2) passes `recv` the
  `MSG_WAITALL` flag, so it will read the full amount unless it gets
  interrupted by a signal.
- Fixed `Bigstring.read`'s handling of `EINTR` so that it retries
  rather than returning zero.

  This fixes a bug introduced in 111.09 in the interaction between
  `Bigstring.read` and `Async.Reader`.  Prior to 111.09,
  `Bigstring.read` would raise on `EINTR`, and `Async.Reader` would
  propagate the exception.  From 111.09 to 111.16, `Bigstring.read`
  would return zero, which would confuse `Async.Reader` into thinking
  it reached EOF when it hadn't.  From 111.17, `Bigstring.read` will
  retry and not return zero when not at EOF.

  We believe the bug was rare, because otherwise we would have
  frequently seen `EINTR` exceptions prior to 111.09.
- Added `Command.Spec.apply` and `pair`, which allow one to program
  more with `Spec.param` rather than `Spec.t`.

  ```ocaml
  val apply : ('a -> 'b) param -> 'a param -> 'b param
  val pair : 'a param -> 'b param -> ('a * 'b) param
  ```
- Added `Command.Spec.file`, which builds an `Arg_type` value with the
  same autocompletion as `Spec.file`.

  ```ocaml
  (** [file] defines an [Arg_type.t] that completes in the same way as
      [Command.Spec.file], but perhaps with a different type than [string] or with an
      autocompletion key. *)
  val file
    :  ?key:'a Univ_map.Multi.Key.t
    -> (string -> 'a)
    -> 'a t
  ```

## 111.11.00

- Change some `Bigstring` functions to retry on `EINTR` rather than
  raise.

  The following functions (and their unsafe versions) were affected:

  * `read`
  * `really_read`
  * `really_recv`
  * `really_write`
  * `really_send_no_sigpipe`

  Some other `Bigstring` functions, like `input` and `output`, already
  retried on `EINTR`, so this change has precedent.

  All of the affected stubs raise `Bigstring.IOError` on failure,
  rather than `Unix_error`, which means the normal method for retrying
  on `EINTR` doesn't work.  In particular `Async.Reader` didn't retry
  them, even though it was supposed to.

  Additionally, the documentation for the following functions was
  corrected to say that they raise =Unix_error= rather than =IOError=:

  * `pread_assume_fd_is_nonblocking`
  * `pwrite_assume_fd_is_nonblocking`
- Eliminated global binary-to-decimal tables computed at startup for
  converting `Date` and `Of_day` to string.

  Used an efficient implementation of division by 10, rather than the
  `sprintf` tables in `time_internal.ml`.  This result in much less
  allocation at startup and it is a bit faster:

  * before:

  | Name           | Time/Run | mWd/Run | Percentage |
  |----------------|----------|---------|------------|
  | Date.to_string |  69.39ns |   3.00w |    100.00% |

  - after:

  | Name           | Time/Run | mWd/Run | Percentage |
  |----------------|----------|---------|------------|
  | Date.to_string |  53.38ns |   3.00w |    100.00% |
- Fixed `Time.Zone` tests so that they are deterministic.
- Added `Iobuf.to_string_hum`, which produces a readable, multi-line
  representation of an iobuf, intended for debugging.
- Fixed brittle unit tests of `Command`.

## 111.08.00

- Improved `Command` to print a good error message if command-line
  parsing raises.

    `Command`'s `Exn.handle_uncaught` now protects the phase of parsing
    command-line arguments in addition to protecting the phase of
    running the `main` function as it did already.

## 111.06.00

- Added inline benchmarks for =Iobuf= and =Time=.

  Hera are some of the results from the new benchmarks, with some
  indexed tests dropped.

  | Name                                 | Time/Run | mWd/Run | Percentage |
  |--------------------------------------|----------|---------|------------|
  | [time.ml:Time] Time.to_string        | 848.74ns | 249.98w |    100.00% |
  | [time.ml:Time] Time.to_ofday         |  59.66ns |  38.00w |      7.03% |
  | [time.ml:Time] Time.now              |  39.78ns |   2.00w |      4.69% |
  | [time.ml:Time] Time.Zone.find_office |  83.64ns |   4.00w |      9.85% |
  | [time.ml:Time] Time.Span.of_hr       |   3.71ns |   2.00w |      0.44% |
  | [time.ml:Time] Time.Span.of_min      |   3.69ns |   2.00w |      0.44% |
  | [time.ml:Time] Time.Span.of_sec      |   2.72ns |         |      0.32% |
  | [time.ml:Time] Time.Span.of_ms       |   6.02ns |   2.00w |      0.71% |
  | [time.ml:Time] Time.Span.of_ns       |   5.98ns |   2.00w |      0.71% |

  | Name                                     | Time/Run | Percentage |
  |------------------------------------------|----------|------------|
  | [iobuf.ml:Blit tests] functor blit:5     |  15.53ns |      7.66% |
  | [iobuf.ml:Poke tests] char:0             |   4.11ns |      2.03% |
  | [iobuf.ml:Poke tests] uint8:0            |   5.35ns |      2.64% |
  | [iobuf.ml:Poke tests] int8:0             |   4.59ns |      2.26% |
  | [iobuf.ml:Poke tests] int16_be:0         |   5.19ns |      2.56% |
  | [iobuf.ml:Poke tests] int16_le:0         |   5.14ns |      2.53% |
  | [iobuf.ml:Poke tests] uint16_be:0        |   5.11ns |      2.52% |
  | [iobuf.ml:Poke tests] uint16_le:0        |   5.12ns |      2.53% |
  | [iobuf.ml:Poke tests] int32_be:0         |   5.17ns |      2.55% |
  | [iobuf.ml:Poke tests] int32_le:0         |   4.91ns |      2.42% |
  | [iobuf.ml:Poke tests] uint32_be:0        |   5.73ns |      2.83% |
  | [iobuf.ml:Poke tests] uint32_le:0        |   5.74ns |      2.83% |
  | [iobuf.ml:Poke tests] int64_be:0         |   5.33ns |      2.63% |
  | [iobuf.ml:Poke tests] int64_le:0         |   4.93ns |      2.43% |
  | [iobuf.ml:Peek tests] char:0             |   5.50ns |      2.71% |
  | [iobuf.ml:Peek tests] uint8:0            |   4.68ns |      2.31% |
  | [iobuf.ml:Peek tests] int8:0             |   4.91ns |      2.42% |
  | [iobuf.ml:Peek tests] int16_be:0         |   5.19ns |      2.56% |
  | [iobuf.ml:Peek tests] int16_le:0         |   4.90ns |      2.42% |
  | [iobuf.ml:Peek tests] uint16_be:0        |   5.17ns |      2.55% |
  | [iobuf.ml:Peek tests] uint16_le:0        |   5.10ns |      2.51% |
  | [iobuf.ml:Peek tests] int32_be:0         |   5.17ns |      2.55% |
  | [iobuf.ml:Peek tests] int32_le:0         |   4.92ns |      2.42% |
  | [iobuf.ml:Peek tests] uint32_be:0        |   5.45ns |      2.69% |
  | [iobuf.ml:Peek tests] uint32_le:0        |   5.46ns |      2.69% |
  | [iobuf.ml:Peek tests] int64_be:0         |   6.61ns |      3.26% |
  | [iobuf.ml:Peek tests] int64_le:0         |   6.31ns |      3.11% |
- Re-implemented `Thread_safe_queue` to improve performance and reduce
  allocation.

  The new implementation requires 3 words per element, down from the 7
  words required by the old implementation.

  The new implementation pools elements so that they can be reused, so
  there is no allocation in steady-state use.

  The new implementation has `dequeue_exn` rather than `dequeue`, so
  that one can dequeue without allocating 2 words.

  Eliminated `create'`.  One should just use `create` and explicit calls
  to `enqueue` and `dequeue_exn`.

  Eliminated `dequeue_until_empty`.  One should use an explicit while
  loop guarded by `length` and using `dequeue_exn`.

  Moved `Thread_safe_queue` from `Core_kernel` to `Core`, since it's
  thread related.

  All in, there is now no allocation in a steady-state usage of
  enqueueing and dequeueing elements, as opposed to 9 words per
  `enqueue+dequeue` in the old implementation.  This reduces the cost
  from `enqueue+dequeue` taking 166-216ns to `enqueue+dequeue_exn`
  taking 48-82ns (plus eliminating gc impacts).  Here are some `BENCH`
  results, the first table being the old implementation, and the
  second table the new.

  | Name                                                       | Time/Run | mWd/Run | mjWd/Run |
  |------------------------------------------------------------|----------|---------|----------|
  | [thread_safe_queue.ml] enqueue + dequeue of immediate      | 183.89ns |   9.00w |    7.02w |
  | [thread_safe_queue.ml] enqueue + dequeue of young object   | 216.69ns |  11.00w |    9.01w |
  | [thread_safe_queue.ml] enqueue + dequeue_exn of old object | 166.75ns |   9.00w |    7.02w |

  | Name                                                         | Time/Run | mWd/Run |
  |--------------------------------------------------------------|----------|---------|
  | [thread_safe_queue.ml] enqueue + dequeue_exn of immediate    |  48.20ns |         |
  | [thread_safe_queue.ml] enqueue + dequeue_exn of young object |  81.96ns |   2.00w |
  | [thread_safe_queue.ml] enqueue + dequeue_exn of old object   |  48.30ns |         |
- Changed `{Bigstring,Iobuf}.recvmmsg_assume_fd_is_nonblocking`, when
  no message is available, to return a negative number rather than
  raise.

  This was done for performance reasons, because raising an exception
  is expensive, due to the stashing of the backtrace and the string
  creation.
- Added `Iobuf.unsafe_resize`.
- Changed `Bigstring.blit` so that it doesn't release the OCaml lock
  on `map_file` bigstrings.

  The old behavior of releasing the lock for blits of (small)
  bigstrings involving mmapped files was problematic and inconsistent.
  Its cost is high, and fundamentally any access to a mapped bigstring
  could cause some level of blocking.
- Added time-related `Arg_type.t` values to `Command.Spec`.
- Added module `Type_immediacy`, which has witnesses that express
  whether a type's values are always, sometimes, or never immediate.

  This code used to be in the `Typerep_immediate` library in typerep.

## 111.03.00

- Added `Unix.Syslog` module.
- Changed `Command.run` to no longer ignore the first element of its
  `~argv` parameter.
- Made `Time.Span.to_short_string` show microsecond precision.

## 110.01.00

- Fixed `Time` unit tests that failed in London because of timezone
  dependence.
- Added `Iobuf.protect_window_and_bounds`, which calls a user function
  and restores the iobuf's bounds afterwards.
- Fixed compilation on OpenBSD, which doesn't support
  `Unix.mcast_join`'s `?source : Inet_addr.t` argument.

## 109.60.00

- Added `Iobuf.unsafe_advance`.

    This can be used to benchmark inner loops that have redundant bounds
    checking, to see if the checks matter.  For example, see the
    following two `advance` calls:

        let rec process_buffer buf ~f =
          let len = Iobuf.length buf in
          if header_len <= len then
            let msg_len = header_len + Iobuf.Unsafe.Peek.uint16_be buf ~pos:0 in
            if msg_len <= len then begin
              let len = msg_len - header_len in
              Iobuf.advance buf header_len;
              f (Protocol.packed_of_iobuf buf);
              Iobuf.advance buf len;
              process_buffer buf ~f
            end

- Added `Weak_hashtbl.add_exn` and `sexp_of_t`.
- Fixed `Lock_file.create` to behave correctly if the target mountpoint
  is out of space.

    Previously in this situation, `Lock_file.create` would create an
    empty lock and exit with exception trying to write pid/message
    there. Subsequent runs would not able to read pid out of empty pid
    file and `blocking_create` would block instead of removing defective
    lock.

## 109.58.00

- Added `Debug.should_print_backtrace : bool ref`, to control whether
  `Debug.am*` functions print backtraces.
- Added to `Float` inline benchmarks.
- Moved all of the `Gc` module into `Core_kernel`.

  Part of the `Gc` module used to be in `Core` because it used
  threads.  But it doesn't use threads anymore, so can be all in
  `Core_kernel`.
- Improved `Iobuf` support for copying to/from strings and bigstrings.

  The new modules are `Iobuf.{Consume,Peek}.To_{bigstring,string}`.
  They match a `Blit`-like interface.  We don't yet implement the
  `Blit` interface in all applicable contexts, but do use `Blit.Make`
  and expose some of the operations we want in the forms we expect
  them to take under a `Blit` interface.
- Added `Linux_ext.Timerfd.to_file_descr`.
- Added to `Time.next_multiple` an optional argument to control
  whether the inequality with `after` is strict.
- Added `Time.Zone.local`, a lazily cached `Time.Zone.machine_zone ()`.

  This is the first stage in a plan to make explicit timezones more
  pervasive.  First, they are made more convenient, by replacing the
  relatively wordy `Time.Zone.machine_zone ()` with `Time.Zone.local`.
  This involves making the underlying timezone type lazy.

  The next stage will be to remove `machine_zone` and use
  `Time.Zone.local` everywhere instead.  Then (it is hoped) instead of
  `of_local_whatever`, we just say e.g. `of_date_ofday
  Time.Zone.local` and currently-implicitly-local functions will be
  able to switch over to explicit timezones without becoming too
  verbose.
- Added `Timing_wheel.Alarm.null`.
- Made `Unix.File_descr.t` have `with sexp`.

  Closes janestreet/async_unix#3
- Fixed OpenBSD compilation failures in C stubs.
- Fixed `Lock_file.is_locked` to require read permission, not write
  permission, on the lock file.
- Added to `Unix.mcast_join` an optional `?source:Inet_addr.t` argument.

  From pull-request on bitbucket:
    https://bitbucket.org/janestreet/core/pull-request/1/receive-source-specific-multicast/diff

## 109.55.00

- Fixed building on FreeBSD and OpenBSD.
- Added `with typerep` to many `Core` types.
- Made `open Core.Std` support `with typerep`.
- Added `Iobuf.recvmmsg_assume_fd_is_nonblocking_no_options`,
  a specialization of `recvmmsg_assume_fd_is_nonblocking` for improved
  performance.

  This improvement was driven by profiling at high message rates.
- Changed `Unix.Rlimit.virtual_memory` be an `Or_error.t`, for platforms
  where it is undefined.

## 109.53.00

- Added `Linux_ext.Epoll.close`.
- Added `Weak_hashtbl` module, moved from `Async`.

  It had only been in `Async` to use `Async`'s finalizers.  The move
  to `Core` exposes a bit more with respect to finalization so that
  one can still implement `Async.Weak_hashtbl`, as well as do other
  things (e.g. use `Weak_hashtbl` in `Incremental`, which does not use
  `Async`).

  Simplified the implementation of `Weak_hashtbl` to eliminate "entry
  ids".  They were only used to avoid removing a table entry that was
  in use.  But there is a more direct way to test for that -- just
  check if the weak is `None` or `Some`.
- Added an autoload file for utop
- Disabled warning 40 in corebuild

## 109.52.00

- Added `Unix.File_descr.equal`.
- Added `Lock_file.Nfs.unlock`, the `Or_error` version of
  `unlock_exn`.
- Improved the detail of the error messages exposed by
  `Lock_file.Nfs.create{,_exn}`.
- Added `Unix.set_mcast_ifname`, to control the interface used for UDP
  multicast traffic.

  Added bindings for setsockopt `IP_MULTICAST_IF`.

  See 6.3 in: http://www.tldp.org/HOWTO/Multicast-HOWTO-6.html
- Changed `Command` argument processing to treat a single dash (`-`)
  as an anonymous argument rather than a flag.

  This change follows the unix convention of passing `-` as an
  anonymous argument meaning `stdin`.
- Added more bin-prot support to `Iobuf`: `Consume.bin_prot`,
  `Fill.bin_prot`, `Peek.bin_prot`, `Poke.bin_prot`.

  Framing doesn't do much for `Iobuf`, so these are to be more
  standard, unframed accessors, as opposed to `fill_bin_prot`.
- Added `Core.Debug.am`, `amf`, and `ams`, for outputting debugging
  messages showing the current source-code position.

  Unfortunately, these aren't available in `Core.Std.Debug`, but only
  in `Core.Debug`.  That will be fixed in 109.49.
- Made `Time_stamp_counter` compile on non x86-64 platforms.
- Made `Core.Std.Debug` be `Core.Debug` rather than
  `Core_kernel.Debug`.

  This exposes the `Debug.am*` functions added in 109.48.

## 109.47.00

- Added `Time_stamp_counter` module, which has fast (< 10 nanos) access to the hardware time-stamp counter.

  This module provides the fast function `Time_stamp_counter.now ()`
  which is our best effort high-performance cycle counter for a given
  platform.  For x86 systems this retrieves the CPU's internal time
  stamp counter using the `RDTSC` instruction.  For systems that do not
  have a RDTSC instruction, we fallback to using
  `clock_gettime(CLOCK_MONOTONIC)`.

  Here is a benchmark of execution time in nanos and allocations in words:

  ```
  Name                            Time/Run   Minor
  ------------------------------- ---------- -------
  Time.now                           39.02    2.00
  TSC.now                             7.54
  TSC.to_time                         4.88    2.00
  TSC.to_time (TSC.now ())            8.54    2.00
  TSC.to_time_nanos                   4.49
  TSC.to_time_nanos(TSC.now ())       8.95
  Calibrator.calibrate                 279   34.00
  ```

  Historically, the rate of increment of the TSC (sometimes referred to
  as the TSC frequency) varied based of CPU overclocking, temperature,
  load etc.  On modern Intel CPU's the TSC is expected to be stable.  On
  Linux systems, the "constant_tsc" in `/proc/cpuinfo` indicates that the
  machine has a stable TSC rate.  While this module assumes that the TSC
  is relatively stable, it can adapt to small variations in the TSC
  frequency.

- Changed `Daemon.daemonize` and `daemonize_wait` to leave the `umask` alone by default.

  Previously, these had alwasy set the umask to `0`, which means that
  all app-harness programs and all binaries run from grass were creating
  world-writeable (`0666`) files by default.

## 109.45.00

- Added `Core.Std.phys_same`, which is like `phys_equal`, except has a
  more general type.

  ```ocaml
  val phys_equal : 'a -> 'a -> bool
  val phys_same  : _  -> _  -> bool
  ```

  `phys_same` is useful when dealing with existential types, and one
  has a packed value and an unpacked value that one wants to check are
  physically equal.  One can't use `phys_equal` in such a situation
  because the types are different.
- Added `Iobuf.set_bounds_and_buffer` and `set_bounds_and_buffer_sub`,
  which make it easier to use with zero allocation.

  ```ocaml
  (** [set_bounds_and_buffer ~src ~dst] copies bounds (ie limits + window) and shallowly
      copies the buffer from [src] to [dst].  [read_write] access is required on [src]
      because the caller might have [read_write] access to [dst], and would after the call
      then effectively have [read_write] access to [src]. *)
  val set_bounds_and_buffer : src:(read_write, _) t -> dst:(_, seek) t -> unit

  (** [set_bounds_and_buffer_sub ?pos ?len ~src ~dst ()] is a more efficient version of:
      [set_bounds_and_buffer ~src:(Iobuf.sub ?pos ?len src) ~dst].

      [set_bounds_and_buffer ~src ~dst] is not the same as
      [set_bounds_and_buffer_sub ~dst ~src ()], because the limits are narrowed in the
      latter case. *)
  val set_bounds_and_buffer_sub
    :  ?pos:int
    -> ?len:int
    -> src:(read_write, _) t
    -> dst:(_, seek) t
    -> unit -> unit
  ```
- Added `?timeout:Time.Span.t` argument to
  `Lock_file.blocking_create`, `Lock_file.Nfs.blocking_create` and
  `critical_section`.

## 109.44.00

- Added `val Day_of_week.num_days : from:t -> to_:t -> int`.
- Added `Time.of_date_ofday_precise` and `Time.Zone.next_clock_shift`,
  to deal with clocks going forward and backward.

  Due to clocks going forward/backward, some local times occur twice,
  and some never occur at all.  `Time.of_date_ofday_precise`
  identifies these cases and returns all of the relevant information.

- Added accessors for `Unix.Cidr`: `base_address` and `bits`.

  ```ocaml
  (** Accessors.
      - [base_address 192.168.0.0/24 ` 192.168.0.0]
      - [bits         192.168.0.0/24 ` 24]. *)
  val base_address : t -> Inet_addr.t
  val bits         : t -> int
  ```

## 109.42.00

- Removed `Zone` from `Core.Std`; use `Time.Zone` instead.

- Removed `Time.Date`; use `Date` instead.

- Improved the performance of `Piecewise_linear` by using arrays with binary search on indices.

  The previous implementation `Piecewise_linear` used `(float * float)
  list` (a list of (x,y) pairs) to represent piecewise linear functions,
  with a linear search through the knots to evaluate the function at a
  point.  This type is now:

  ```ocaml
  { x : float array
  ; y : float array
  }
  ```

  and the implementation uses binary search to identify the correct array index.

  Here are the costs of the `get` function under the old (list) and new (array)
  implementations for various numbers of knots:

  ```
  knots |  old | new
  ------+------+-----
      1 |  11ns| 12ns
      2 |  18ns| 14ns
      5 |  27ns| 19ns
    100 | 182ns| 38ns
   1000 |1974ns| 52ns
  ```

- Added module `Time.Ofday.Zoned`, which is a pair of an `Time.Ofday.t` and a `Time.Zone.t`.

- Added `with compare` to `Time.Zone.Stable.t`.

- Added `Timing_wheel` functionality.

  * Added `Config` module, which combines `alarm_precision` and `timing_wheel_level_bits`.
  * Removed the need to supply a dummy value to `create`.
  * Added `mem` and `clear` functions.
  * Added functions for dealing with the interval number: `interval_num`, `now_interval_num`, `interval_num_start`, `add_at_interval_num`.

  This makes it easier to use a timing wheel with `int` interval
  numbers, which are more efficient than than `float` times.

## 109.41.00

- Added `Command.Spec.map_anon` and `map_flag`.

  ```ocaml
  (** [map_flag flag ~f] transforms the parsed result of [flag] by applying [f] *)
  val map_flag : 'a flag -> f:('a -> 'b) -> 'b flag

  (** [map_anons anons ~f] transforms the parsed result of [anons] by applying [f] *)
  val map_anons : 'a anons -> f:('a -> 'b) -> 'b anons
  ```

- Fixed `Unix.open_flag` to compile with OCaml 4.01.

  It needed the additional constructor `O_CLOEXEC`.

## 109.37.00

- Command.run now calls Exn.handle_uncaught so you don't have to.
- Fixes for building on FreeBSD.
- Fixed Blang to build with OCaml 4.01.

  In blang.mli:

  Blang.t is a private variant type, Blang.Stable.V1.t is a private
  variant type, and client code knows Blang.t = Blang.Stable.V1.t.
  Previously, this was done in a strange way, using with type 'a t =
  private 'a t on the signature of Blang.Stable.V1. In addition to
  being strange, this line no longer builds in OCaml 4.01, which
  caused problems for building Real World Ocaml.

  This patch changed the code to something much more straightforward,
  although not quite so straightforward as we expect to be able to
  achieve once a nonrec bug is fixed.

## 109.35.00

- Added `or_error` functions in `Unix.Exit_*` types to `unit
  Or_error.t`.

  This makes it easier to deal with combining with infix operators
  `>>=?` and `>>|?`

## 109.34.00

- Added `val Backtrace.get_opt : unit -> t option`.

  This is more convenient to use than `Backtrace.get`, which is an
  `Or_error.t`.

- Moved functions for dealing with finalizers into the `Gc.Expert` module.

  This was done to make people be very explicit and sure that they want
  finalizers, which are very hard to use because they essentially
  introduce multithreading semantics.

  One should typically use async finalizers.

- Eliminated the thread that had been used to sequentialize all finalizers.

## 109.32.00

- Normalized `Command`'s help messages.

  Made anonymous argument names uppercase and subcommand names lowercase.

- In `Iobuf`, added duals to `flip` and `snapshot` to work on the high end of the window.

  `flip` has been renamed to `flip_lo`.  The dual of `flip_lo` is the
  newly added `flip_hi`, and shifts the window forward to continue
  reading, rather than back to switch from writing to reading, as
  `flip_lo` does.

  `flip_hi`, in practice, needs snapshots of the upper bound of the
  window, we split `Snapshot` into `Lo_bound` and `Hi_bound` and
  introduced bounded versions of `flip_lo`, `compact`, and `flip_hi` to
  support buffers which are only partially filled, but have
  substructure, like packet buffers.

  Here's the new API.

  ```ocaml
  module type Bound = sig
    type ('d, 'w) iobuf

    (** Expose =t = private int= only if a =t= is stored in a mutable data structure
       somewhere and leads to a measurable =caml_modify= performance problem. *)
    type t with sexp_of

    val window : (_, _) iobuf -> t
    val limit  : (_, _) iobuf -> t
    val restore : t -> (_, seek) iobuf -> unit
  end
  module Lo_bound : Bound
  module Hi_bound : Bound
  val flip_lo         : (_, seek) t -> unit
  val bounded_flip_lo : (_, seek) t -> Lo_bound.t -> unit
  val flip_hi         : (_, seek) t -> unit
  val bounded_flip_hi : (_, seek) t -> Hi_bound.t -> unit
  ```

## 109.30.00

- Created submodule `Core.Signal.Expert` module.

  This is for functions previously in `Core.Signal` that introduce
  multithreading semantics, and are hence hard to reason about and
  should only be used by experts.

## 109.28.00

- Moved `Timing_wheel` from `Zero`.

## 109.27.00

- Disabled use of `recvmmssg`, which isn't available on our CentOS 5
  machines.
- Defined `Option.compare` using `with compare` so that their
  comparisons are consistent.
- Cleaned up the `Dequeue` module's interface and implementation.

  The interface now matches the conventions used elsewhere in `Core`.
  The new implementation is also cleaner and more efficient.
- Reimplemented the `Stack` module to improve performance, and renamed
  the old implementation as `Linked_stack`.

  The new `Stack` is implemented with this type:

  ```ocaml
  type 'a t `
    { dummy : 'a;
      mutable length : int;
      mutable elts : 'a array;
    }
  ```

  `Linked_stack` is implemented with this type:

  ```ocaml
  type 'a t `
    { mutable length : int;
      mutable elts : 'a list;
    }
  ```

  Using an array rather than a linked list is a more efficient and
  traditional implementation.  Pushing to the stack now does not
  require allocation, except in the rare case when the stack grows.

  One downside is that `Stack.clear` is now O(n) rather than O(1).

  This change also eliminates the `Stack.Empty` exception, so any code
  matching on that exception should fail to compile, and should be
  changed to depend on option-returning `top` and `pop` operations.
- Improved `Lock_file.Nfs`.
  * Allowed an arbitrary message to be stored and retreived.
  * Fixed a case where `create` might throw an exception.
  * Delete both files used for locking when we unlock.
- Split `Core` into `Core_kernel` and `Core`.
- `Core_kernel` is composed of all modules of `Core` that do not
  depend on unix or threads, and `Core` contains the rest and depends
  on `Core_kernel`.

  The goal is to have a very portable standard library that can
  especially run on exotic environment such as Javascript.

  So that code that directly refers to `Core` (rather than `Core.Std`)
  for modules that have moved to `Core_kernel`, we included "proxy"
  modules in `Core` that simply include the corresponding module from
  `Core_kernel`.

- Fixed `Core.Flags` to build on 32-bit machines.

  It had contained a unit test with an integer literal too large to be
  accepted by the compiler when building on a 32-bit machine.

## 109.24.00

- Added module `Core.Iobuf`, a module aimed at zero-copy I/O.

  An iobuf is a bigstring combined with a position and length, that
  defines a contiguous region of bytes in the bigstring.  Operations on
  an iobuf operate relative to start of the region and cannot look
  outside the region.

- Added module `Core.Validated` for creating an abstract type that
  ensures a validation function has been run.

- Added function `Bigstring.recvmmsg_assume_fd_is_nonblocking`, which
  allows one to read a number of UDP packets with a single system
  call.

- Fixed `Unix.create_process` on OSX.

## 109.23.00

- Exposed `Core.Std.Flags` module.
- Made the `Heap` module implement `Container.S1`.
- Added module `Ref.Permissioned`, which is a ref with `read_only` /
  `read_write` access control.
- Exposed the unique id in `Type_equal.Id`.

  This allows, e.g. hash tables indexed by unique ids.
- Removed the use of `Obj.magic` from the implementation of
  `Type_equal.Id.same`.

  It is not needed because the `Id.t` contains a `Uid.t` and we can
  just use `Uid.equal`.

## 109.21.00

- Massively improved the signatures of `Map` and `Set`, both for
  readability and ocamldoc, as well as improved type error messages.

  For instance the type of `Int.Set.singleton` was:

  ```ocaml
  ('a, 'comparator, 'a Core.Std.Int.Set.elt_ -> ('a, 'comparator) Core.Std.Int.Set.t_) Core.Core_set_intf.without_comparator
  ```

  Now it is simply:

  ```ocaml
  int -> Int.Set.t
  ```
- Added an optional argument to `Command.run` that can be used to
  specify default flags from a user config file.

  The optional argument can extend the command line based on the path
  to the command.
- Rename module `Weekday` as `Day_of_week`.

  The name `Weekday` conflicted with ordinary usage of "weekday" to
  mean Monday through Friday.
- Changed `sexp_of_t` for `{Month,Ofday,Time,Time.Span}.{Set,Map}` to
  use the nice sexp format of the underlying atomic type.

  Previously, the converter had used thes raw type (`float`, `int`,
  etc.).  `t_of_sexp` still accepts both formats; we will remove the
  ability to accept the raw format in the distant future.

  This output-format change was planned when we originally in 108.06b
  improved those `t_of_sexp` functions to accept both formats.
- Added `Unix.remove`.
- Removed some `IFDEF`'s connected to OCaml <4 support.

## 109.20.00

- Wrapped `Unix.wordexp` in an `Or_error.t` since it is not available on all systems.

- Added function `Process_env.parse_ssh_client`.
  This gets the address from which you're currently ssh'd in.

- Added to `Unix` module the ability to get and set `IP_MULTICAST_LOOP` and `IP_MULTICAST_TTL`.

- Exposed module `Core.Std.Ref`, which was previously only available via `Core.Ref`.

- Remove `Mutex.am_holding_mutex` and changed the type of `Mutex.try_lock`.

  With NPTL it is impossible to determine which thread is holding the
  lock.  So, `Mutex.am_holding_mutex` is unimplementable.  Also,
  `Mutex.try_lock` was incorrect because it claimed to raise if one was
  attempting to recursively lock.  Since it's not possible to
  distinguish between recursive locking and the lock being held by
  another thread, we changed the type to make this clear:

  ```ocaml
  val try_lock : t -> [ `Already_held_by_me_or_other | `Acquired ]
  ```

- Removed our custom version of the OCaml runtime's `core_sys_open` function.

  There used to be a bug in the OCaml runtime, PR#5069, in which
  `open_{in,out}_gen` could block while holding the OCaml lock, because
  they made a call to `fcntl` outside the blocking section.  We had our
  own C code with the bug fix and re-exposed the fixed versions of the
  functions in `Core`.

  The bug in OCaml has been fixed, so we have removed our patched
  function from `Core`.

- In `unix_stubs.c`, switched from using `FNM_FILE_NAME` to `FNM_PATHNAME`.

  The GNU project introduced FNM_FILE_NAME as a non-portable synonym for
  FNM_PATHNAME.

  We were using pre-processor macros to define FNM_FILE_NAME as
  FNM_PATHNAME if unavailable, but it is simpler to just use the more
  portable FNM_PATHNAME everywhere.

## 109.19.00

- Changed `Time.to_string` and `Time.sexp_of_t` to include the
  timezone.

  This is an incompatible change with very old programs in which
  `Time.of_string` and `Time.t_of_sexp` did not support the timezone.

  If you have programs that are:

  * very old and do Time string/sexp handling
  * rely on reading in time values without using `Time.of_string` and
    `Time.t_of_sexp`.
  * rely on chains of writing/reading/writing times across machines
    and timezones where the time is always intended to be taken as the
    local time on the currently reading machine

  you should recompile/review your code to make sure you won't have
  issues.
- Added function `List.remove_consecutive_duplicates : 'a t ->
  equal:('a -> 'a -> bool) -> 'a t`.

  This returns the input list with consecutive duplicates removed, and
  doesn't change the order of the remaining elements.
- Added module `User_and_group`, which is a pair of a unix username
  and primary unix group.

  The string/sexp converters follow the usual unix convention of
  `<user>:<group>`.
- Added function `Date.first_strictly_after : t -> on:Weekday.t -> t`.

  `first_strictly_after t ~on:day_of_week` returns the first
  occurrence of `day_of_week` strictly after `t`.
- Added functor `Type_equal.Lift`.

  It is always safe to conclude that if type `a` equals `b`, then type
  `a X.t` equals `b X.t`, for any type `X.t`.  The OCaml type checker
  uses this fact when it can.  However, sometimes, e.g. when using
  `Type_equal.conv`, one needs to explicitly use this fact to
  construct an appropriate `Type_equal.t`.  The `Type_equal.Lift*`
  functors do this.

  ```ocaml
  module Type_equal : sig
    type ('a, 'b) t
    ...
    module Lift (X : T1) : sig
      val lift : ('a, 'b) t -> ('a X.t, 'b X.t) t
    end
  end
  ```

## 109.18.00

- changed implementation of `Array.sort` to use introsort.

  See http://en.wikipedia.org/wiki/Introsort.
- tweaked a unit test in `Core.Flags` to not print a message to
  stderr.

## 109.17.00

- Fixed `Random.self_init`, which was broken since 109.00.00 with the
  upgrade to OCaml 4.0

  The fix changed the type signature expressed in `core_random.ml` of
  the standard OCaml `caml_sys_random_seed` C function from `unit ->
  int` from `unit -> int array`.  That C function changed between
  OCaml 3.12 and 4.0.
- Moved module `Core_extended.Unix.Cidr` into `Core.Unix`.
- Wrapped `Unix.wordexp` into an `Or_error.t` to handle systems that
  does not implement it in the libc.
- Fixed two other printer names
- Added `Array.int_blit` and `Array.float_blit`, which are specialized
  fast blits for `int array` and `float array`.

  For motivation underlying this change and other design alternatives
  please see Section 3 "Fast, Slow and Incorrect Array blits" of
  http://janestreet.github.com/ocaml-perf-notes.html
- Added `Unpack_buffer.Unpack_one.sexp` for parsing sexps using the
  `Unpack_buffer` interface.

## 109.15.00

- Changed the tolerance of `Time.Robustly_compare` functions from
  `1E-7` to `1E-6`.
- Fixed the names of some toplevel pretty-printers, which referred to
  nonexistent modules.

  Fix some of the `pp`'s for Core which are used to install printers
  in the top-level.  Some of the toplevel printers refer to
  non-existent modules like `Core.Nativeint.pp`; this feature changed
  to the correct name, like `Core.Std.Nativeint.pp`.
- Added to module `Unix` functionality for getting and setting flags
  in the open-file-descriptor table.

  ```ocaml
  module Open_flags : sig type t include Flags.S with type t :` t  ...  end
  val fcntl_getfl : File_descr.t -> Open_flags.t
  val fcntl_setfl : File_descr.t -> Open_flags.t -> unit
  ```
- Added module `Linux_ext.Timerfd`.

  This allows one to create a file descriptor that can be monitored by
  `epoll` or `select` and notify them at a certain time.  It makes it
  possible to use `epoll` with sub-millisecond timeouts.
- Added `Version_util.application_specific_fields`, which allows
  custom build-time information to be included in an executable.

## 109.14.00

- Fixed major performance problem with hashing in `Int.Table`.

  Our `Int.Table.replace` was 3 times slower than polymorphic hash
  table and `find` was _8_ times slower.

  This was caused by using:

  ```ocaml
  external seeded_hash_param : int -> int -> int -> 'a -> int = "caml_hash" "noalloc"
  ```

  in `Int.Table` but:

  ```ocaml
  external old_hash_param : int -> int -> 'a -> int = "caml_hash_univ_param" "noalloc"
  ```

  everywhere else.

  The `seeded_hash_param` was introduced in Caml 4.

  We fixed this problem by changing `Int.hash` from:

  ```ocaml
  let hash (x : t) = Hashtbl.hash x
  ```

  to:

  ```ocaml
  let hash (x : t) = if x >= 0 then x else ~-x
  ```
- Added `Bigstring.{pread,pwrite}`, which allow reading and writing at
  a specific file offset.
- Added module `Nothing`, which is a type with no values.

  This is useful when an interface requires you to specify a type that
  you know will never be used in your implementation.
- Changed `Identifiable.Make` so that it registers a pretty printer.

  `Identifiable.Make` now uses `Pretty_printer.Register`.  This
  requires all calls to `Identifiable.Make` to supply a `val
  module_name : string`.
- Made `Core.Zone` match the `Identifiable` signature.
- Made polymorphic equality always fail on `Core.Map.t` and
  `Core.Set.t`.

  Before this change, polymorphic equality on a `Core.Map` or a
  `Core.Set` could either raise or return `false`.  It returnd `false`
  if the data structures were unequal, and raised if the data
  structures were equal.

  This is because their type definitions looked like:

  ```ocaml
  type ('k, 'v, 'comparator) t =
    { tree : ('k, 'v) Tree0.t;
      comparator : ('k, 'comparator) Comparator.t;
    }
  ```

  and polymorphic equality visits a block's fields in order.  So, it
  will detect unequal trees and return false, but if the trees are
  equal, it will compare the comparators and raise because of the
  functional value.

  This change reversed the order of the fields so polymorphic equality
  always fails.

## 109.13.00

- Added `Command.Spec.flags_of_args_exn`, for compatibility with
  OCaml's standard library.

  This function converts a `Core.Std.Arg.t` into a `Command.Spec.t`.
- Made various modules `Identifiable`: `Char`, `String`, and the
  various `Int` modules.

  In particular, `Int` being identifiable is useful, because one can
  now write:

  ```ocaml
  module My_numeric_identifier : Identifiable ` Int
  ```

  You might think that we could now delete `String_id`, and just
  write:

  ```ocaml
  module My_string_identifier : Identifiable ` String
  ```

  But this is not quite equivalent to using `String_id`, because
  `String_id.of_string` enforces that its argument is nonempty.

- Removed module `Space_safe_tuple`, which became unnecessary in OCaml
  4.00.0.

  OCaml 4.00.0 included Fabrice's patch to fix the space leak that
  `Space_safe_tuple` was circumventing (PR#5288, commit SVN 11085).
- Added `Exn.to_string_mach`, for single-line output.
- Added `Linux_ext.bind_to_interface`, to improve security of UDP
  applications.

  ```ocaml
  val bind_to_interface : (File_descr.t -> string -> unit) Or_error.t
  ```

  This uses the linux-specifc socket option `BINDTODEVICE` to prevent
  packets being received from any interface other than one named.
- Fixed `Unix.mkdir_p` on Mac OS X.

## 109.12.00

- Add some functions to `Byte_units`.
  - Added functions: `to_string_hum`, `scale`, `Infix.//`.
  - Eliminated the notion of "preferred measure", so a `Byte_units.t`
    is just a `float`.
- Improved the performance of `Array.of_list_rev`.

  The new implementation puts the list elements directly in the right
  place in the resulting array, rather that putting them in order and
  then reversing the array in place.

  Benchmarking shows that the new implementation runs in 2/3 the time of
  the old one.
- Fixed `Fqueue.t_of_sexp`, which didn't work with `sexp_of_t`.

  There was a custom `sexp_of_t` to abstract away the internal record
  structure and make the sexp look like a list, but there wasn't a
  custom `t_of_sexp` defined, so it didn't work.
- Added `Stable.V1` types for `Host_and_port`.
- Removed `Identifiable.Of_sexpable` and `Identifiable.Of_stringable`,
  in favor of `Identifiable.Make`

  `Identifiable.Of_sexpable` encouraged a terrible implementation of
  `Identifiable.S`.  In particular, `hash`, `compare`, and bin_io were
  all built by converting the type to a sexp, and then to a string.

  `Identifiable.Of_stringable` wasn't as obviously bad as
  `Of_sexpable`.  But it still used the string as an intermediate,
  which is often the wrong choice -- especially for `compare` and
  `bin_io`, which can be generated by preprocessors.

  Added `Identifiable.Make` as the replacement.  It avoids using sexp
  conversion for any of the other operations.
- Added `List.intersperse` and `List.split_while`.

  These came from `Core_extended.List`.

  ```ocaml
  val intersperse : 'a list -> sep:'a -> 'a list
  val split_while : 'a list -> f:('a -> bool) -> 'a list ** 'a list
  ```
- Added a functor, `Pretty_printer.Register`, for registering pretty printers.
  The codifies the idiom that was duplicated in lots of places:

  ```ocaml
  let pp formatter t = Format.pp_print_string formatter (to_string t)
  let () = Pretty_printer.register "Some_module.pp")
  ```

## 109.11.00

- Added module `Interned_string` This has a functor for creating
  modules of interned strings.  It uses the very simple mechanism of
  mapping distinct strings to consecutive ints.
- Added value `Hashtbl.find_and_remove`.

## 109.10.00

- Added `|>`, which means the same as `|!`, but is likely to replace
  it someday.  This is mostly because `|>` is an accepted notation
  elsewhere, particularly in F#.  In the future, we will consider
  eliminating `|!` in favor of `|>`, so as to avoid the duplication.
- Made `module Lazy` into a monad.
- Renamed
  `List.stable_dedup_involving_an_application_of_the_set_functor` as
  `List.stable_dedup_staged`.  Made it use `Staged.t` to make explicit
  the expectation of partial application.
- Added pretty printers for the toplevel to `Error` and `Info`.

## 109.09.00

- In `Core.Std`, exposed `Or_error.ok_exn` and `Or_error.error`
- Removed some values exported by `Core.Std`.

  Removed some values from `Core.Std` that weren't widely used, or we
  didn't think should be exposed, including `ascending`, `descending`,
  and `equal`, which use polymorphic comparison, and we want to
  discourage.

  Here's a guide to some of what was removed, and what one should now
  use instead.

  | removed                           | replace with                          |
  |-----------------------------------+---------------------------------------|
  | `Int_replace_polymorphic_compare` | `Int.Replace_polymorphic_compare`     |
  | `ascending`                       | `Polymorphic_compare.ascending`       |
  | `descending`                      | `Polymorphic_compare.descending`      |
  | `equal`                           | `Polymorphic_compare.equal`           |
  | `ifprintf`                        | `Printf.ifprintf`                     |
  | `sscanf`                          | `Scanf.sscanf`                        |
  | `Scan_failure`                    | `Scanf.Scan_failure`                  |
  | `string_of__of__sexp_of`          | `Sexplib.Conv.string_of__of__sexp_of` |
  | `of_string__of__of_sexp`          | `Sexplib.Conv.of_string__of__of_sexp` |
  | `type vec`                        | `type float64_vec`                    |

- Disallowed `<:sexp_of<` with two underscores; using a single underscore instead.
- Added `Command.Spec.Arg_type.of_alist_exn` as an alternative for `of_map`.
  This captures the common pattern to create the map from an alist.
- Improved the performance of `Hashtbl`.
  Constrained hashtbl size to a power of two and used a bitmask rather
  than mod operation for finding hash buckets.
- Improved the performance of `Univ`, using the `Type_equal` GADT.
  The new implementation improves the run-time and space usage over
  the old one.  In the old implementation, a `Univ.t` was represented
  as record with three fields: an exception, a string, and a closure.
  Creating a univ required allocating three heap blocks, the exception
  (3 words), the closure (3 words), and the three-field record (4
  words).  In the new implementation, a `Univ.t` is represented as a
  2-field heap block containing the `Constr.t` and the value.
  Creating a univ allocates that single 3-word block, improving on the
  10 words needed previously.

  Matching on univs is also faster.  In the old implementation,
  matching on a univ required making a function call, testing
  exception equality, and allocating a `Some` block.  Now, it does
  just the test and allocation.  Furthermore, it is possible to use
  `does_match` and `match_exn` to avoid the allocation.
- Added `Version_util.build_info_as_sexp`.
- Added `_squelch_unused_module_warning_` to
  `Comparable.S.Replace_polymorphic_compare`.

## 109.08.00

- Cleaned up and updated the `README`.
- Changed executables to enable backtraces if `OCAMLRUNPARAM` is not set.
- Changed `Command` so that executables show build info and version info
  This happens when an executatble is called as:

    foo.exe version

  Before this change, rather than display build info, executables
  would display the not-so-helpful:

  (no option given - printing version)
- Added back `Float` rounding functions with a hardcoded direction.
- Exposed `with bin_io` and `with compare` for the =sexp_bool= type.
- Added value `Core.Never_returns.sexp_of_t`.
- Added values `Or_error.tag{,_arg}`
  These are analogous to `Error` functions of the same name.
- Added functor `Sexpable.Of_sexpable`
  This is for serializing values of one type as though it were some
  other isomorphic type.
- Added module `Backtrace.Exn`
  This exposes OCaml stdlib's `Printexc` functions for backtraces.
- Added module `Flags`
  This implements Unix-style sets of flags that are represented as an
  `int` with various bits set, one bit for each flag, e.g.,
  `Linux_ext.Epoll.Flag`.
- Added module `Uuid`
  This module implements universally unique identifiers based on version
  3 of the UUID specification.  It used to be in `Core_extended=`
- Added module `Type_equal`, which defines the "equality" GADT.

## 109.07.00

- Added a number of functions to =Bounded_int_table=: =equal=,
  =exists{,i}=, =for_all{,i}=, =filter_map{,i}=, =map{,i}=.  Also
  added a functor, =Bounded_int_table.With_key=, that makes a
  bounded-int table binable and sexpable, and adds =of_alist= and
  =of_alist_exn=.
- Added =Doubly_linked.iter_elt= and =Bag.iter_elt=.
- Added =module Invariant=, which defines signatures that are to
  be included in other signatures to ensure a consistent interface to
  invariant-style functions.
- Added =module Ordering=, which defines:
    =type t = Less | Equal | Greater=

## 109.06.00

- Added [Map.symmetric_diff], for returning a list of differences
  between two maps.  It has a fast-path implementation for maps that
  share a large amount of their internal structure.

## 109.05.00

- Updated [Core.Unix.stat] so that access, modify, and change times
  have nanosecond precision.
- Fixed a bug in [Nano_mutex.invariant].
- Simplified the implementation of [with_return] using a local
  explicit polymorphic type variable.

## 109.04.00

- Fix [Backtrace.get], which was broken in 109.00, with the
  switch to OCaml 4.0.
- Added [Heap.iter_el].

## 109.02.00

- Add Char.of_string

