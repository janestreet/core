(** Module for simple closed intervals over arbitrary types. Used by calling the
    {{!module:Core.Interval.Make}[Make]} functor with a type that satisfies
    {{!module:Base.Comparable}[Comparable]} (for correctly ordering elements).

    Note that the actual interface for intervals is in
    {{!modtype:Core__.Interval_intf.Gen}[Interval_intf.Gen]}, following a Core pattern of
    defining an interface once in a [Gen] module, then reusing it across monomorphic ([S])
    and polymorphic ([S1], [S2], ... [SN]) variants, where [SN] denotes a signature of N
    parameters. Here, [S1] is included in this module because the signature of one ['a]
    parameter is the default.

    See the documentation of {{!module:Core.Interval.Make}[Interval.Make]} for a more
    detailed usage example. *)

open! Import
open Interval_intf

(**
   {2 Intervals using polymorphic compare}

   This part of the interface is for polymorphic intervals, which are well ordered by
   polymorphic compare. Using this with types that are not (like sets) will lead to crazy
   results.
*)
include Interval_intf.S1 (** @inline *)

(** {2 Type-specialized intervals}

    The module type [S] is used to define signatures for intervals over a specific type,
    like [Interval.Ofday] (whose bounds are [Time.Ofday.t]) or [Interval.Float], whose
    bounds are floats.

    Note the heavy use of destructive substitution, which removes the redefined type or
    module from the signature. This allows for clean type constraints in codebases, like
    Core's, where there are lots of types going by the same name (e.g., "t").
*)

(** {3 Signatures }

    The following signatures are used for specifying the types of the type-specialized
    intervals.

*)

module type S1 = Interval_intf.S1

module type S = S
  with type 'a poly_t := 'a t
  with type 'a poly_set := 'a Set.t

(**
   [S_time] is a signature that's used below to define the interfaces for [Time] and
   [Time_ns] without duplication.
*)
module type S_time = S_time
  with type 'a poly_t := 'a t
  with type 'a poly_set := 'a Set.t

(** {3 Specialized interval types} *)

module Ofday    : S with type bound = Time.Ofday.t
module Ofday_ns : S with type bound = Time_ns.Ofday.t

module Time    : S_time with module Time := Time
                         and type t = Time.t t
module Time_ns : S_time with module Time := Time_ns
                         and type t = Time_ns.t t


module Float : S with type bound = Float.t

module Int : sig
  include S with type bound = Int.t (** @open *)

  include Container.S0        with type t := t with type elt := bound
  include Binary_searchable.S with type t := t with type elt := bound
end

(** [Interval.Make] is a functor that takes a type that you'd like to create intervals for
    and returns a module with functions over intervals of that type.

    For example, suppose you had a [Percent.t] type and wanted to work with intervals over
    it, i.e., inclusive ranges like 40-50% or 0-100%. You would create your
    [Percent_interval] module by calling:

    {[module Percent_interval = Interval.Make(Percent)]}

    You now have a module with lots of functionality ready to use. For instance you could
    call [Percent_interval.empty] to create an empty interval, or:

    {[Percent_interval.create (Percent.of_percentage 3) (Percent.of_percentage 30)]}

    to get an actual interval that ranges from [3%] to [30%]. You can then ask questions
    of this interval, like whether it's a {{!val:is_subset} subset} of another interval or
    whether it {!val:contains} a particular value.

    NB. In order to use the [Interval.Make] functor, your type must satisfy
    Comparable and support bin-io and s-expression conversion. At a minimum, then,
    [Percent] must look like this:

    {[
      module Percent = struct
        module T = struct
          type t = float [@@deriving bin_io, compare, sexp]
        end
        include T
        include Comparable.Make_binable(T)
      end
    ]}
*)
module Make (Bound : sig
    type t [@@deriving bin_io, sexp]
    include Comparable.S with type t := t
  end)
  : S with type bound = Bound.t

(**
   [Stable] is used to build stable protocols. It ensures backwards compatibility by
   checking the sexp and bin-io representations of a given module. Here it's also applied
   to the [Float], [Int], [Time], [Time_ns], and [Ofday] intervals.
*)
module Stable : sig
  module V1 : sig
    type nonrec 'a t = 'a t [@@deriving bin_io, compare, sexp]
    module Float   : Stable with type t = Float.  t
    module Int     : Stable with type t = Int.    t
    module Time    : Stable with type t = Time.   t
    module Time_ns : Stable with type t = Time_ns.t
    module Ofday   : Stable with type t = Ofday.  t
  end
end
