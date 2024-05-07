## Release v0.17.0

* `Bool.Stable.V1`:
  - Add `hash`.
  - Expose a comparator, type-equal to the unstable one.

* Timezones can be used from javascript targets without needing to bundle and serve a copy of the unix timezone database.

* Add support for `@@deriving diff` to core.

* `Bigstring`:
  - Remove `max_mem_waiting_gc_in_bytes` parameter from `Bigstring.create`
  - Add functions:
    * `Bigstring.unsafe_memset` for filling a range with a character without bounds checks
    * `Bigstring.memcmp_string` for efficient comparison between `Bigstring` and `string` data
    * `Bigstring.get_string` and some variants of it, as alternatives to `Bigstring.to_string`. These are nice because they follow the convention of the `Bigstring.get_int*` functions, and in particular avoid optional arguments.

* Remove `Diffable_comparable` from core. It is no longer necessary now that `Map` and
`Set` include `Diffable.S`.

* Make `Map` and `Set` implement `Diffable.S`.

* Updated `Container.Generic` to allow two phantom type parameters.

* Make `Error`, `Result`, `Or_error`, `Percent`, `String_id.S`, `Date`, `Span` and `Ofday`
implement `Diffable.S`.

* Added `Utf8`, `Utf16le`, `Utf16be`, `Utf32le` and `Utf32be` submodules to `Uchar` and `String` for Unicode
encoding support.

* Add a `Binary` submodule to `Int`, `Int32`, etc, which provide `to_string` and `sexp_of_t`
with syntax matching the OCaml binary int literal syntax.

* Add Quickcheck generator to `Uchar.t` and add Quickcheck generators to `String` for
generating valid Unicode strings in different encodings.

* Update the type of `Quickcheck.Generator.create` and `Quickcheck.Generator.generate` to use `Splittable_random.t` instead of `Splittable_random.State.t`. The former is simply a shorter alias for the latter.

* `Byte_units`:
  - Add `[@@deriving typerep]`
  - Add quickcheck `gen_incl` and `gen_uniform_incl`.

* Add `add_validate_parsing_flag` parameter to `Command.run`, enabling a `-validate-parsing` flag in all subcommands. The command immediately exits with a return code of 0 if argument parsing is successful.

* Add `Bin_prot` support for writing locally-allocated values in `Array`, `Bigstring`, `Blang`, `Bool`, `Bytes`, `Char`, `Date`, `Either`, `Filename`, `Float`, `Int`, `Int32`, `Int64`, `Lazy`, `List`, `Maybe_bound`, `Nativeint`, `Option`, `Ref`, `Result`, `String`, `Unit`

* Add `Bin_prot.Writer.to_bigstring` and `Bin_prot.Reader.of_bigstring`, for converting directly between a value and a buffer containing precisely the serialization of that value.

* Add `Binable.S_local` interface, reflecting support for writing locally-allocated values.

* Add `Comparable` support for locally-allocated values in `Lazy`

* Add `[@@deriving equal]` to a number of stable types: `Byte_units.Stable.V2`, `Day_of_week.Stable.V1`, `Either.Stable.V1`, `Fdeque.Stable.V1`, `Fqueue.Stable.V1`, `Int63.Stable.V1`, `List.Stable.V1`, `Md5.Stable.V1`, `Nothing.Stable.V1`, `Percent.Stable.V3`, `Pid.Stable.V1`, `Queue.Stable.V1`, `Result.Stable.V1`, `Set_once.Stable.V1`, `Source_code_position.Stable.V1`, `Unit.Stable.V2`

* Add `[@@deriving equal]` to types exposed by `Perms`.

* Expose the fact that `Pid.t` is an immediate.

* Add `Char.Stable`.

* Add `Comparable.Extend_plain`, which is like `Comparable.Extend` but only requires `sexp_of_t` and not `t_of_sexp`.

* Remove or limit usage of `[@@deriving fields]` to reduce bloat in: `Command.Shape`, `Gc`, `Source_code_position`

* Add `Float.Stable.V1.globalize`

* Changes in `Gc` to support the OCaml 5 runtime:
  - In documentation for `Gc.Stat`, indicate `heap_words`, `heap_chunks`, `free_blocks`, `largest_free`, `top_heap_words`, and `stack_size` metrics are not available in OCaml 5 runtime, defaulting to `0`.
  - In documentation for `Gc.Control`:
    - Include behavior under OCaml 5 runtime for `minor_heap_size`, detailing the total size of the minor heap across active domains.
    - Update default `space_overhead` value documentation to reflect differences between OCaml 4 and OCaml 5 runtimes.
    - Add new flag `0x400` to `Control.verbose` for outputting GC statistics at program exit in OCaml 5 runtime.
    - Update `Gc.stat` and `quick_stat` function documentation to reflect behavior differences between OCaml 4 and OCaml 5 runtimes, emphasizing approximation of GC statistics in OCaml 5 due to per-domain buffers.
    - Clarify `counters` function's return values may differ between `quick_stat` and itself under OCaml 5 runtime, detailing accuracy in single-domain scenarios.
    - Note potential overflow of integers returned by GC statistics functions on 32-bit machines and clarify accuracy of `minor_words` in byte-code versus native code programs.

* Additional `Gc` support for locally-allocated values.

* Add `Sexp_grammar` support to `Char.Stable.V1`, `Filename.Stable.V1`, `Host_and_port`, `Maybe_bound.Stable.V1`, `Nothing.Stable.V1` `Time_ns.Ofday.Stable.V1`, `Result.Stable.V1`, `Time_ns.Ofday.Stable.V1`, `Time_ns.Span.Stable.V1`

* Add `Host_and_port.Hide_port_in_tests`, re-exposing the type with test-friendly serialization which elides the port.

* Deprecate `List.stable_dedup_staged` and `Set.stable_dedup_list` in favor of `List.stable_dedup`

* Deprecate `Map.comparator` in favor of `Map.Comparator.Module.t`

* Add new `Map` functions:
  - `Map.of_list_with_key_fold` to resolve duplicate keys by folding values.
  - `Map.of_list_with_key_reduce` to resolve duplicate keys by reducing values.
  - `Map.unzip` to transform a map of tuples into a tuple of maps.
  - `Map.merge_disjoint_exn` to merge two maps with disjoint keys, raising an exception if any keys overlap.
  - `Map.sum` and `Map.sumi` for summing values with a provided summable type and function.

* Add `[@@deriving sexp_of]` to `Substring`.

* Improve `Time.to_sec_string` documentation.

* Add fast, approximate rounding functions to `Time_ns.Span`.

* Add `Time_ns.O` and `Time_ns.Span.O`, containing infix operators.

* Improve `Time_ns.next_multiple` and `Time_ns.prev_multiple` documentation.

* Add `For_testing.reset_counter` to `Unique_id.Id`.

* Add support for validating locally-allocated values to `Validated`.

* Add new functions to `Univ_map`:
  - `key_id_set`, exposing the set of raw `Uid` keys
  - `find_packed_by_id` and `find_packed_by_id_exn`, allowing lookup by raw `Uid` key


## Release v0.16.0

Changes that affect multiple modules:

* Replaced references to `Caml` with `Stdlib`.

* Added `stable_witness` values to many stable-version submodules.

* Added support for locally-allocated values.
  * Added `[@local]` to many function arguments, especially closures.
  * Added `[@local_opt]` to primitives, and exported more primitives explicitly.
  * Added deriving `globalize` to some types.

Changes to individual modules:

* `Array`: Added `fold_map` and `fold_mapi`.

* `Bigstring`:
  * Added `get_head_padded_fixed_string_local`, `get_tail_padded_fixed_string_local`, and
    `write_bin_prot_known_size`.
  * Updated documentation for `unsafe_destroy`.

* `Binable`: Removed deprecated functor aliases.

* `Binary_searchable`: Added `S0_permissions` signature.

* `Bounded_index`: Added `zero_based_index` and `num_indexes`.

* `Byte_units`: Add `abs`, `sign`, and `neg`.

* `Command`:
  * Moved to its own library. Still re-exported from `Core`.
  * Removed references to `Core`-dependent types.
  * No longer exports process-management functions.
  * Added `Command.basic_or_error`.
  * Added `Arg_type.parse` and `Arg_type.autocomplete`.
  * Added `Flag.escape_with_autocomplete`, `Flag.no_arg_required`, and
    `Flag.no_arg_abort`.
  * Removed deprecated function `Flag.one_or_more`.
  * Added `Param.choose_one_non_optional`, `Param.optional_to_required`, and
    `Param.parse`.

* `Comparable`:
  * Renamed `Polymorphic_compare` to `Comparisons`.
  * Added `Stable.V1.With_stable_witness.Make`.

* `Container`: Added `S0_permissions` and `S1_with_creators_permissions`.

* `Date`:
  * Document caveats for functions based on `is_weekday` or `is_weekend`.
  * Add `round_forward_to_weekday`, `round_backward_to_weekday`,
    `round_forward_to_business_day`, and `round_backward_to_business_day`.
  * Add `Stable.V1.Option.to_int` and `Stable.V1.Option.of_int_exn`.

* `Day_of_week`:
  * Add deriving `sexp_grammar` and `typerep`.
  * Add deriving `sexp_grammar` to `Stable.V1`.

* `Doubly_linked`: Add `first_exn` and `last_exn`.

* `Filename`: split out `Filename_base` library for parts not dependent on `Core`.

* `Float`: add `Stable.V1`.

* `Gc`:
  * Add `Stat.add`.
  * Add `stat_size`.
  * Remove `prepare_heap_to_count_minor_allocation`.

* `Hashable`: Add `Stable.V1.With_stable_witness`.

* `Hashtbl`: Add `Make_stable` and `Make_stable_with_hashable`.

* `Hash_queue`: Add `to_alist`, `replace_or_enqueue`, `replace_or_enqueue_front`, and
  `replace_or_enqueue_back`.

* `Hash_set`: Add `Make_stable` and `Make_stable_with_hashable`.

* `Heap_block`: Moved to its own `Heap_block` library.

* `Identifiable`: Add `Make_plain_using_comparator`.

* `Indexed_container`: Added this module. It exports `S1_permissions` and
  `S1_with_creators_permissions`, defined by analogy to the same names in `Container`.

* `Info`: Added `Stable.V2.t_sexp_grammar`.

* `Int`: Exported `comparator_witness` equivalence with `Base.Int`.

* `Int63`:
  * Exported `comparator_witness` equivalence with `Base.Int`.
  * Added `Stable.V1`.

* `Lazy`: Exported `t_stable_witness`.

* `List`: remove deprecated function `stable_dedup`.

* `Map`:
  * Removed creator/accessor signatures except `*_generic` versions.
  * Made `t` injective in all three type parameters.
  * Replaced references to `comparator` type with `Comparator.Module.t`.
  * Removed deprecated type alias `comparator`.
  * Reworded documentation on Tree types.
  * Added `quickcheck_observer` and `quickcheck_shrinker` to `S_plain`.
  * Added `of_list_with_key`, `of_list_with_key_or_error`, `of_list_with_key_exn`,
    `of_list_with_key_multi`, `split_le_gt`, `split_lt_ge`, and `transpose_keys`.
  * Added `Make_applicative_traversals`, providing `mapi` and `filter_mapi` that
    operate lazily inside an applicative.
  * Added `Stable.V1.With_stable_witness`.

* `Maybe_bound`:
  * Added deriving `equal` and `hash` to `t`, `As_lower_bound.t`, and `As_upper_bound.t`.

* `Option`:
  * Added deriving `equal` and `hash` to `Stable.V1`.

* `Or_error`:
  * Deprecated `Expect_test_config_with_unit_expect`, as `[%expect]` now always has type
    unit.

* `Percent`:
  * Addressed round-trippability via strings and sexps.
    * Added `to_string_round_trippable`.
    * Documented caveats about `Stringable` and `Sexpable` implementations.
    * Documented caveats on `Stable.V1`.
    * Added `Stable.V2` based on `t_of_sexp` / `sexp_of_t` and documented caveats.
    * Added `Stable.V3` as a fully round-trippable version.
    * Made similar updates to `Option.Stable`.
    * Documented caveats on `Always_percentage`.
    * Added `Almost_round_trippable` as a more human-readable format than `Stable.V3`.
  * Addressed precision issues.
    * Added `*_slow_more_accurate` versions of `of_percentage`, `to_percentage`, `of_bp`,
      and `to_bp`.
    * Documented caveats on the original versions.
  * Added `t_sexp_grammar`, `one_hundred_percent`.

* `Pid`: Added deriving `quickcheck`.

* `Quickcheck`: Updated implementation and documentation of `geometric`. Swapped argument
  order.

* `Sequence`: Added inlined records to `Step.t`.

* `Set`:
  * Removed creator/accessor signatures except `*_generic` versions.
  * Replaced references to `comparator` type with `Comparator.Module.t`.
  * Added `comparator_s`.
  * Updated documentation of `split`.
  * Added `split_le_gt` and `split_lt_ge`.
  * Added `Stable.V1.With_Stable_Witness`.

* `Set_once`:
  * Added documentation of `equal` semantics.
  * Added deriving `compare` and `equal`.

* `Sexp`: Added deriving `equal` and `sexp_grammar` to `Stable.V1.t`.

* `String_id`:
  * Documented shared bin shape for all modules created by `Make`.
  * Added `Make_with_distinct_bin_shape`.

* `Time_float`: Updated deprecation messages to mention `Time_float_unix` instead of
  `Time_unix`.

* `Time_ns`:
  * Added deriving `equal` and `hash` to `Stable.V1.Ofday`.
  * Added more `of_int_*` conversions to `Span`.
  * Added optional `Random.State` argument to `Span.randomize`.
  * Removed deprecated submodules from `Stable`.
  * Exported `Option` submodule.
  * Exported `Stable` submodule.
  * Exported `V1`, `Option.V1`, and `Option.V2` from `Span.Stable`.
  * Added deriving `hash`, `equal`, and `sexp_grammar` to `Span.Stable.V2`.

* `Tuple`: Added `T2.map_both` and `T3.map_all`.

* `Unit`: Added `Stable.V2` with zero-width bin-io.

* `Univ_map`: Moved to its own `Univ_map` library.

* `Validated`: Added `S_allowing_substitution`.
