As Core is built on top of Base and Core\_kernel
you might want to have a look at Base's
[changelog](https://github.com/janestreet/base/blob/master/CHANGES.md)
and Core\_kernel's
[changelog](https://github.com/janestreet/core_kernel/blob/master/CHANGES.md).

## v0.11.1

- Fix compatibility with spawn v0.12.0

## v0.11

- Fix a segfault in `Time`. (fixes #102)

## v0.10

- Renamed `Float.to_string` as `to_string_12`, to reflect its 12-digit
  precision. And introduce a new `Float.to_string` with the behavior of
  `Float.to_string_round_trippable`.

- Added to `Command.exec` support for specifying another executable via
  `argv.(0)` rather than `Sys.executable_name`.

- Changed `Lock_file` to not delete lock files in forked processes.

- In `Linux_ext.Epoll`, reduced closure allocation

- Added support to `Piecewise_linear` for `Time_ns`

- Added function `Iobuf.Expert.reinitialize_of_bigstring`, which allows users to
  use an `Iobuf` while being passed a bigstring, without allocating

- Changed `Command` to no longer print the entire help on parse errors.

- Added `[@@deriving hash]` to `Identifiable.S*` and `Hashable.S*`. Added
  functors `Identifiable.Make*_and_derive_hash_fold_t`, which generate
  `hash_fold_t` from `hash`.

- Added to `Iobuf` inlining annotations for Flambda.

- Added to `Schedule` a `Zoned_between` constructor, which allows the span to
  cross the midnight boundary, and allows a different time zone for the start
  and end ofdays. Because of the new expressive power, renamed the existing
  `Schedule` to `Schedule_v4_deprecated` and added the new functionality in
  `Schedule_v5`.

- Improved `Schedule_v5.Between` to support crossing the midnight boundary.

- Exposed `[@@deriving hash]` in `Uuid`.

- Changed `Command` to display build time in the local zone.

- Changed `Command.exec` to allow executing a subcommand of the child executable.

- Added `Command.Param.flag_optional_with_default_doc`, which takes a default
  value and formats that value into the doc.

- Added `Command.lazy_group` and `Command.Sexpable.Lazy`, so that help for lazy
  commands and groups of commands doesn't have to force the lazy unless
  necessary.

- Changed `Filename` and `Unix` functions that generate temporary filenames and
  directories to include `.tmp.` in the name, so one can reliably recognize
  them.

- Changed `Daemon.daemonize_wait` to obey stdout/stderr redirection even when
  they are already pointing at a regular file.

- Fixed `Time.Format`'s handling of zones.

- In `Linux_ext`, added support for the `TCP_QUICKACK` socket option, for use on
  TCP connections where latency is important

- Added `Sys.quote` function for quoting bash input, like `Filename.quote`, but
  with more readable output.

- Added support to `Signal` for signals that were added in OCaml 4.03: bus,
  poll, sys, trap, urg, xcpu, xfsz.

- Added support to `Udp.sendto` for MacOS.

- Added `Linux_ext.Eventfd` module, with bindings for `eventfd(2)`.

- Removed bounds checks from `Iobuf.unsafe_blit`.

- Optimized `Inet_addr.inet4_addr_to_int32_exn`, removing an intermediate string
  allocation.

- Optimized `Time_ns` comparison functions to use the `Int63` primitives; they
  had been shadowed to use slower generic operations exported by
  `Identifiable.Make`.

- Added to `Unix.Rlimit` a `Limit` submodule, with `min` and `max` functions.

- Added function `Unix.Inet_addr.inet4_addr_of_int63`, for efficient conversion
  from an `Int63.t` to an `Inet_addr.t`.

- Changed `Unix.create_process_env`'s execvp emulation w.r.t environment
  handling, PATH, and allowed nonfatal errors.

- Changed `Unix.sysconf` to return an option, and to correctly detect when there
  is no value.

- Made `Unix.IOVec.max_iovecs` lazy, to avoid calling sysconf at top-level.

- Removed function `Time.Ofday.Zoned.compare`, which was misleading because it
  does not correspond to a temporal ordering. The comparison is now available as
  `Time.Ofday.Zoned.With_nonchronolgical_compare`.

- Removed `Time_ns` functions: `of_time`, `local_now`, `of_local_time`. These
  functions do not exist in `Time.Ofday` and are simple wrappers around existing
  functions.

- Renamed `Command.basic` as `basic_spec`.
  Renamed Command.basic' as basic.
  We want to encourage the use of `Command.Param` and discourage the use of
  `Command.Spec`.

- Removed `Uuid.t_of_sexp`.

- Switched Core to `-safe-string`.

- Changed `Uuid.sexp_of_t` to show the nil UUID when `am_running_inline_test`.

- Added module-level documentation for `Interval`.

- Changed `Time_ns.Ofday`'s `to_string` and `sexp_of_t` functions to output nine
  decimal places rather than six.

- Deprecated `Uuid.t_of_sexp`, in favor of `Unstable.t_of_sexp` and
  `Stable.V1.t_of_sexp`.

- Fixed `Unix.Iovec.of_string` and `of_bigstring` so that the default len
  includes the suffix of the input starting at `pos`, as usual.

- Removed `Unix.create_process`'s support for the old `ml_create_process`
  backend, leaving it with only the `spawn_vfork` backend.

## v0.9

## 113.43.00

- `Time.Ofday.of_string` supports "AM and "PM" suffixes.

- Change the innards of `Command.Flag` so it no longer uses mutable state. It now
  updates a `Univ_map.t` instead.

- Renamed:

    core/test        --> core/test-bin

  since these directories contain executables to run rather than
  libraries with standard unit tests.  This is in preparation for moving
  the standard unit tests to a more "normal" test directory.

- Move unit tests from core/src to core/test .

- The output for the "version" subcommand and the "-version" flag is now the same.

- Core.Std.Lock_file doesn't work properly on OSX, see this:

    https://github.com/janestreet/jenga/issues/4#issuecomment-205176593

  This fix the issue by requiring both flock and lockf only on Linux.

  Closes janestreet/jenga#4

- Unix.fork_exec needs to handle exceptions raised by exec

- Remove `Core.Core_list`, which was not in `Core.Std` and had only an unused `to_sequence`.

- Array.random_element

- Shift operations are unspecified outside of the range `0 <= x < bitsize`.

  This fixes unit tests for core/src/core_unix.ml in 32bit.

- Replace all occurences (in Core) of `Time_ns.Span.to_int_ns` and `Time_ns.to_int_ns_since_epoch` with
  their 63bit counterpart.

  `Time_ns.Span.to_int_ns` and `Time_ns.to_int_ns_since_epoch` are not implemented in 32bit.

  The is one step toward have unit tests to pass in 32bit.

- Wrap Unix.strptime in Date and Time

- New arg types in Command.Param:

    sexp = Arg_type.create Sexp.of_string

    sexp_conv a_of_sexp = Arg_type.create (fun s -> a_of_sexp (Sexp.of_string s))

- `Time_ns.Ofday.create` presently makes 2 function calls to
  `caml_int_compare`. Changed to use the native int comparisons and
  avoid the calls in `create` (and other comparisons).

## 113.33.01

- Fix build problem on BSD related to `endian.h`.

## 113.33.00

- Updated to follow core\_kernel's evolution.

- Add variance annotations to a few types in `Command`.

- `Command.Param.choice` represents a sum type.

- Added `Command.Spec.of_param` for conversion from new-style to old
  style command line specifications.  This is expected to be useful for
  gradually converting Spec-based commands into Param-based ones.

- `flag_names` and `anon_names` gives the flags and anons used by a
  Command.Param.t, while `mnemonic` combines them to give a string
  that's good for referring to that param in error messages.

  This feature improves the interface of `Command.Param.one_of` by
  saving the caller from passing the flag names separately.

- Remove the closure allocation from `Date0.create_exn`

- Many apps take schedules as command line parameters (to determine when
  they'll be up, or when they should raise issues, etc.).  Let's add a
  Schedule.Stable.V4.flag function to generate the command line param to
  make writing this common pattern easier.

- Add Ofday.Option to `time_ns.ml`.

  Exports `Time_ns.Span.Option.unchecked_value` and
  `.Ofday.Option.unchecked_value`!

- Finally fix `Time_ns.Of_day.of_local_time` w.r.t. DST transitions after
  regression test failures at the DST transition.

  As a result, clarified slightly the role of `Time_ns.Ofday`, like
  `Time.Ofday`, as 24h wall-clock times, not linear time offsets, which
  latter would have to be up to 25h for DST.

- Adds `Command.Spec.Arg_type.percent`.

- Add `Stable.V1` to `Time_ns.Ofday.Option.t`, add it to `Core.Stable`, and
  standardize the interface to `Option` modules in `Time_ns`.

- Add `Time_ns.to_ofday`.

## 113.24.02

- Deprecated core.syntax and updated `coretop` to use `ppx_jane`

## 113.24.01

- Updated corebuild to use ppx-jane instead of camlp4

  Closes #70

## 113.24.00

N.B. Some interface change were made which are not listed here, as they are only
cascading from `core_kernel`. Look at core\_kernel's CHANGES.md file to get a
complete history of the changes made for this release.

- Add `Core.Command.Arg_type.Export.time_zone`

- Fix `Command.shape` to run external programs only once to get their sexp.

  Introduces a new variant of Command.t called Proxy.  The Exec variant represents the
  top-level command of an external executable that has not yet been run; the Proxy variant
  represents an arbitrary subcommand that has been extracted by running an external
  executable.  Command.exec constructs an Exec variant; Command.shape of an Exec variant
  runs the executable and generates a tree of Proxy variants representing all the
  information from the generated sexp, so the executable will not need to be re-run.

- A version of recvmmsg that pre-allocates and reuses the iovec
  record. Profiling indicates this is a non-trivial amount of our I/O
  loop (under very heavy load).

  Since nobody is using the heavyweight features of the existing
  recvmmsg, replace it with the lightweight one.  This leads to minor
  but important changes to the interfaces of
  `Iobuf.recvmmsg_assume_fd_nonblocking` and `Udp.recvmmsg_loop`.

- Switch to ppx.

- `Time.set_sexp_zone` affects `t_of_sexp`

  If we're willing to read sexps without zones, we should be willing to let you
  control what timezone they're read with.

- Sped up `Llimiter`.

- Make `Interval.Int` implement `Container` and `Binary_searchable`.

- Add `Identifiable.S` to `Unix.Cidr` so that it supports hash tables.
  Update `Unix.Cidr.t` to normalize values, e.g. "192.168.1.101/24" ==> "192.168.1.0/24".

- Added "noalloc" attribute to `Linux_ext.unsafe_timerfd_settime`.

- In Iobuf, made some functions take:

    (`> write`, _) Iobuf.t

  rather than:

    (read_write, _) Iobuf.t

  if they only need to write to the iobuf and don't need to read it.

- `Time.of_string_abs` didn't support ISO 8601 time zone strings
  without colons, or those specified as locations. This version adds
  support for time zones without colons

- `Unix.Passwd.getpwents` takes the lock, partially applies `Exn.protect`, then
  releases the lock, then completes the application and actually runs stuff.

- Command.file-completion

  Files are now completed correctly when paths contain a directory.

  Previously, completion when pointed at a directory would put a space at the end.
  This would cause the user to hit backspace every time a directory was in the path.

- Add `diff_weekdays` and `diff_weekend_days` functions to date module.

- `Time.to_date_ofday_precise` implements a complete inverse for
  `of_date_ofday`. This is needed to give the DWIM-est semantics to Schedule.t
  that we can think of.

- `Reduce allocation in Linux_ext.Epoll`

- Add `Time_ns.Of_day.(add_exn, sub_exn, diff)`.

- Adding `head_padded_fixed_string` to Iobuf.


- Move `Core_extended.Std.Sys.home` to `Core.Std.Sys.home_directory`.

- Add `Iobuf.{read,write,input,output}` akin to the bigstring versions.

- Add expert iobuf functions for extracting bigstrings and iovecs that share the iobuf's
  underlying storage.

- Add stable `Int63.t` conversions for types in `Time_ns`.

- Rename DNS-based `Inet_addr` sexp conversions to expose their blocking nature.

- Add `Unix.Inet_addr.Stable`.


- Add [Core.Std.Schedule].

- Fixed `Iobuf_packet.iter`, which behaved incorrectly if the packet
  didn't start at the lo_min of the iobuf.  It used `Iobuf.rewind` when
  it should have used `Iobuf.Lo_bound.restore`.

  Added `@@deriving compare` to `Iobuf.Bound`.

- Add ability to `Time.format` for a specific time zone

- Make more information available via Command.Shape.t

  Expose machine-readable info on anonymous arguments.

- Remove unnecessary rebinding of `(^/)` in `core_filename.ml`.  It had one
  call site and wasn't exposed.  The `(^/)` everyone uses comes from
  std.ml

- Add `getifaddrs` to `Core.Std.Unix`.

  Handles Packet (for interfaces that do not have an address on Linux systems only), IPv4 and IPv6
  address families.

- Implement `Time_ns.Span.to_string_hum` by analogy to `Time.Span.to_string_hum`.
  The code and tests are essentially copied from "lib/core/src/span.ml".

- Remove our stubs for `Unix.stat`.

  They were upstreamed in 4.02.2.

## 112.35.00

- Tweaked `Unix.stat`'s C code to reduce float rounding error, by using
  double-precision rather than single-precision floats.

    Following Xavier's comment:
    http://caml.inria.fr/mantis/view.php?id=6285

- Added `Date.O` module.
- In `Interval`, exposed `compare` in stable types by having the
  appropriate modules match the `Stable` signature.
- Added `Iobuf.Fill.decimal` and `Poke.decimal`, for efficiently writing
  integers in decimal format.
- Removed `Filename.O` (and `/^`), since it's not used that much, and is
  inconsistent with the older operator for this that's exposed in
  `Core.Std`: `^/`.
- Improved `Command` autocompletion to work even if some arguments
  can't be parsed.

    This is useful because the completion mode does not get fed
    precisely the same arguments that it would get if you hit RETURN.

    As a simple example, if completion is set up for `my-exe` which
    takes a sexp as its first argument, then:

        my-exe '(a b)' <TAB>

    will run `my-exe` in completion mode with:

        '(a b)'

    as its first argument, rather than:

        (a b)

    as would be passed if RETURN had been pressed instead.  Since
    `Sexp.of_string "'(a b)'"` fails, in this example tab completion
    won't work.

    Changed the internals a bit to make this possible.  Most notably,
    the `Parser` module is now applicative rather than monadic, which
    required only a few simple changes to support.

- Made `Time_ns.to_time` and `Time_ns.Span.to_span` round to the nearest
  microsecond in all cases.

    Previously, `Time_ns.Span.to_span` sometimes rounded incorrectly for
    negative spans.

- Added `Time.Zone.prev_clock_shift`, the analog of `next_clock_shift`.

        val prev_clock_shift
          :  t
          -> before:Time_internal.T.t
          -> (Time_internal.T.t * Span.t) option

    Implemented `next_clock_shift` and `prev_clock_shift` using
    `Array.binary_search_segmented`.

- Added `Lock_file.get_pid : string -> Pid.t option`.
- Added `val random: unit -> int` to `Time_ns` and `Time_ns.Span`.
- Renamed `Iobuf.sub` as `Iobuf.sub_shared`.

    This more closely matches `Bigstring`, and clarifies the semantics
    of `Iobuf.sub` vs `sub` in `Blit_intf.S`.

- Added `Iobuf` blit modules: `Blit`, `Blit_consume`, `Blit_fill`,
  `Blit_consume_and_fill`.
- Added `Piecewise_linear.first_knot` and `last_knot`.

        val first_knot : t -> (key * value) option
        val last_knot  : t -> (key * value) option

- Made `Unix.Cidr` match `Comparable.S_binable`, and added `Cidr.create` and
  `Cidr.netmask_of_bits`.
- Moved `Unix.tm` and `Unix.strftime` from `Core_kernel` to `Core`.
- Made `Crc.crc32` return `Int63.t` rather than `int64`, and added
  `Crc.bigstring_crc32` and `Iobuf.crc32`.

    Cleaned up old cruft in the C stubs for CRC checking.

- Added `Iobuf.narrow_lo` and `narrow_hi`, which comprise
  `Iobuf.narrow`.
- Changed `Linux_ext.Timerfd`, `Epoll.wait`, and `Unix.select` to use
  (`int`) `Time_ns` rather than (`float`) `Time`.

    This avoids spurious float conversions and rounding problems.

    Made all timeouts consistently treat negative timeouts as "timeout
    immediately".

    This fixes an incorrect behavior of `Linux_ext.Timerfd.set_after`
    and `set`, which had been rounding to the nearest microsecond, which
    was particularly bad for time spans smaller than 500ns, which would
    be rounded to zero, and then would cause the timerfd to never fire.
    Now, the small span is directly fed to `timerfd_settime`.  We also
    changed a span of zero to be treated as `1ns`, to avoid the behavior
    of `timerfd_settime 0`, which causes the timerfd to be cleared and
    never fire.

- Made `Or_error` match `Applicative.S`.
- Added `Command.Param`, with the intention of one day replacing
  `Command.Spec` and providing an applicative interface for command-line
  parsing.

    This change required lots of rearrangement of `command.mli` so that
    `Command.Param` and `Command.Spec` could share large portions of
    their interface.  As a side effect, the interface is more sweeksy
    than before.

- Added `Command.shape`, for exposing the shape of a command, including
  what subcommands its has.

- Changed `Syscall_result.to_result` to return a preallocated object,
  and changed many uses to take advantage of this property.

    Pattern matching on results is much clearer than if-analysis and
    avoids double-checking errors in many cases.

    `Syscall_result.to_result` can only return preallocated results for
    a few `Ok` values from `Syscall_result.Int`, of course, and likewise
    for other large `ok_value` types.  We have initially, and
    arbitrarily, limited preallocation to 64 `errno`'s and 2048
    `ok_value`'s.

- Added `Sys.big_endian : bool`, from `Caml.Sys`.

- Disabled unit tests in `Time_ns` that started failing around 10:40pm
  NYC 2015-05-15.

    The tests indicate an off-by-one-microsecond error in round tripping
    between `Time.Span.t` and `Time_ns.Span.t`.

## 112.24.00

- Renamed `Dequeue` as `Deque`.
- Added `Fdeque`, a functional deque (a double-ended `Fqueue`, or a functional `Deque`).
- Changed `Fqueue`'s bin-io format, and added a stable type.

  Deprecated deque-like functions in favor of `Fdeque`.

- Added `Fheap`, a functional heap implementation based on pairing heaps.
- Reverted the change to `Or_error`'s bin-io format made in 112.17, going back
  to the format in 112.16 and before.
- Added to `Unix.env` type a ``Replace_raw` variant, as used in `exec` and
  `fork_exec`.
- Added `Array.Permissioned` module, which has a permissioned array type and
  permissioned versions of all the regular array functions.
- Added `Date.week_number : t -> int`.
- Added values to `Day_of_week`: `of_int_exn`, `iso_8601_weekday_number`,
  `weekdays`.

       val of_int_exn : int -> t
       val iso_8601_weekday_number : t -> int
       val weekdays : t list (- [ Mon; Tue; Wed; Thu; Fri ] *)

- Switched `Float` IEEE functions to use `Int63.t` for the mantissa rather than
  `int`, so they work on 32-bit platforms.
- Added a `length` field to the `Map.t` record, making `Map.length` `O(1)`
  rather than `O(n)`.
- Moved a fragment of `Time_ns` from `Core` to `Core_kernel`, enough so that
  `Async_kernel` can use `Core_kernel.Time_ns` and ultimately only depend on
  `Core_kernel`.
- Fixed compilation of `Time_ns` 32-bit Linux.
- Added `Bounded_int_table.clear`.
- Fixed the `module_name` passed to `Identifiable.Make` for a number of modules.

  The module name must be an absolute module path.

  Reported here: https://github.com/janestreet/core/issues/52

- Added `Tuple.Binable` functor, for making binable tuples.
- Sped up a `Time_stamp_counter` unit test.

  `Time_stamp_counter` unit test has an 18s unit test, which seems
  excessive.  Take a couple of orders of magnitude off the number of
  iterations.

- Added `Time_ns.pause`, whose implementation is the same as `Time.pause`.

  This involved moving the `nanosleep` C code from `Core` to
  `Core_kernel`.

  This was necessary so that `Async_kernel` can pause without introducing
  a dependence of Async on Core.

- Made `Core_kernel.Time_ns.Alternate_sexp` use a similar format to `Core.Time_ns`.

  This was needed so that `Async_kernel` can use a nice sexp format for
  time spans.

- Changed `Timing_wheel` implementation to use `Time_ns`, and moved to
  `Core_kernel.Timing_wheel_ns`; made `Core.Timing_wheel` a wrapper around
  `Timing_wheel_ns`.

  Generalized the timing-wheel interface to be parametric in `Time`, so
  that one interface applies to both `Timing_wheel` and
  `Timing_wheel_ns`.

  Generalized the timing-wheel unit tests to a functor,
  `Timing_wheel_unit_tests.Make`, that is used to test both
  `Timing_wheel_ns` and `Timing_wheel_float`.  Moved a few tests that
  depend on `Time` and `Date` from the functor into
  `timing_wheel_float_unit_tests.ml`.

  Split out `Timing_wheel.Debug` into a separate functor,
  `Timing_wheel_debug.Make`.

  This was done in so that `Async_kernel` can depend only on
  `Core_kernel` and not `Core`.

- Added optional arguments to `Command.group`: `?body` and
  `?preserve_subcommand_order`.

  `preserve_subcommand_order : unit` causes subcommands to be in the order
  they are specified, rather than sorted.

  `body : (path:string list -> unit)` is called when no additional
  arguments are passed.

- Added accessor function `Command.summary : t -> string`.
- Fixed a bug in `Time.Span` robust comparison.
- Changed `Command`'s tab-completion bash code so that it is possible for
  programs to return completions containing spaces.

  Actually knowing when and how to do so is difficult, because of course there's escaping to
  worry about. Adding helper functions to make that sort of thing manageable is left for
  future work.

- In `Command`, made the `-version` and `-build-info` flags work at the top
  level when there are subcommands.
- Added `Sequence.interleaved_cartesian_product`, which implements cartesian
  product of potentially infinite sequences.

## 112.17.00

- Deprecated the single-line files that simply `include` the
  corresponding Core_kernel module.  Those are unnecessary, because
  people should use `Core.Std`.

  We will keep these aliases around for a version before deleted them
  entirely.
- Changed finalizers and signal handlers to, upon an unhandled
  exception, exit nonzero rather than asynchronously raise.
- Removed `Time.Zone.find_office`.

  Replaced uses with the still-blocking `Time.Zone.find_exn`
- Made many changes to `Time` to make the time zone explicit instead
  of implicitly using the local timezone.

  Added `zone:Time.Zone.t` parameters to many functions.  In almost
  all cases, used `~zone:Time.Zone.local` where previously it was
  implicit.

  Removed `of_local_ofday` and `to_local_ofday` in favor of the
  explicit versions (with `Time.Zone.local`).

  Removed `Time.Zone.machine_zone ()` in favor of `local`.
- Exported `Core.Std.With_return`.
- Exposed `Core.Std.with_return_option`.
- Fixed `Time_ns.Ofday.of_span_since_start_of_day` to check its input.
- Changed `Time_ns.to_span` and `of_span` to round to microseconds,
  for round trippability.
- Added `Unix.Error` module, for the `Unix.error` type.
- Added `Unix.Syscall_result`, a new abstract type representing the
  result of a Unix system call as an `int`, to avoid allocation.

  A lot of Unix system calls return an integer on success, so for ones
  that are called a lot, we can encode errors as `-errno`.  This
  module abstracts this concept.
- Changed `Iobuf.recvmmsg` functions to return the new
  `Unix.Syscall_result`.
- Changed `Unix.exec`'s `?env` argument to support extending the
  environment in addition to replacing it.
- Added `with compare` to `Unix.Exit.t` and `Unix.Exit_or_signal.t`.
- Moved `Backtrace` to `Core_kernel`.

  Deleted `backtrace_stubs.c`, now that we have `Printexc.get_callstack`.
- Changed `Bigstring.read_assume_fd_is_nonblocking` and
  `send_nonblocking_no_sigpipe` to return `Unix.Syscall_result.t`, to
  reduce allocation.
- Changed `Iobuf.send_nonblocking_no_sigpipe` to handle `EINTR` like
  `EAGAIN`, instead of raising.
- Added `Command.Spec.char`.
- Changed `Process_env.parse_ssh_client` to accept an `SSH_CLIENT`
  that is just IP address without ports.

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
