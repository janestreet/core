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
