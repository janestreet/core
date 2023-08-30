open! Import
open Quickcheckable_intf

module type Conv = Conv
module type Conv_filtered = Conv_filtered
module type Conv1 = Conv1
module type Conv_filtered1 = Conv_filtered1
module type S = Quickcheck.S
module type S1 = Quickcheck.S1
module type S2 = Quickcheck.S2
module type S_int = Quickcheck.S_int

module Of_quickcheckable
  (Quickcheckable : S)
  (Conv : Conv with type quickcheckable := Quickcheckable.t) : S with type t := Conv.t =
struct
  let quickcheck_generator =
    Quickcheck.Generator.map Quickcheckable.quickcheck_generator ~f:Conv.of_quickcheckable
  ;;

  let quickcheck_observer =
    Quickcheck.Observer.unmap Quickcheckable.quickcheck_observer ~f:Conv.to_quickcheckable
  ;;

  let quickcheck_shrinker =
    Quickcheck.Shrinker.map
      Quickcheckable.quickcheck_shrinker
      ~f:Conv.of_quickcheckable
      ~f_inverse:Conv.to_quickcheckable
  ;;
end

module Of_quickcheckable1
  (Quickcheckable : S1)
  (Conv : Conv1 with type 'a quickcheckable := 'a Quickcheckable.t) :
  S1 with type 'a t := 'a Conv.t = struct
  let quickcheck_generator generate_a =
    Quickcheck.Generator.map
      (Quickcheckable.quickcheck_generator generate_a)
      ~f:Conv.of_quickcheckable
  ;;

  let quickcheck_observer observe_a =
    Quickcheck.Observer.unmap
      (Quickcheckable.quickcheck_observer observe_a)
      ~f:Conv.to_quickcheckable
  ;;

  let quickcheck_shrinker shrink_a =
    Quickcheck.Shrinker.map
      (Quickcheckable.quickcheck_shrinker shrink_a)
      ~f:Conv.of_quickcheckable
      ~f_inverse:Conv.to_quickcheckable
  ;;
end

module Of_quickcheckable_filtered
  (Quickcheckable : S)
  (Conv : Conv_filtered with type quickcheckable := Quickcheckable.t) :
  S with type t := Conv.t = struct
  let quickcheck_generator =
    Quickcheck.Generator.filter_map
      Quickcheckable.quickcheck_generator
      ~f:Conv.of_quickcheckable
  ;;

  let quickcheck_observer =
    Quickcheck.Observer.unmap Quickcheckable.quickcheck_observer ~f:Conv.to_quickcheckable
  ;;

  let quickcheck_shrinker =
    Quickcheck.Shrinker.filter_map
      Quickcheckable.quickcheck_shrinker
      ~f:Conv.of_quickcheckable
      ~f_inverse:Conv.to_quickcheckable
  ;;
end

module Of_quickcheckable_filtered1
  (Quickcheckable : S1)
  (Conv : Conv_filtered1 with type 'a quickcheckable := 'a Quickcheckable.t) :
  S1 with type 'a t := 'a Conv.t = struct
  let quickcheck_generator generate_a =
    Quickcheck.Generator.filter_map
      (Quickcheckable.quickcheck_generator generate_a)
      ~f:Conv.of_quickcheckable
  ;;

  let quickcheck_observer observe_a =
    Quickcheck.Observer.unmap
      (Quickcheckable.quickcheck_observer observe_a)
      ~f:Conv.to_quickcheckable
  ;;

  let quickcheck_shrinker shrink_a =
    Quickcheck.Shrinker.filter_map
      (Quickcheckable.quickcheck_shrinker shrink_a)
      ~f:Conv.of_quickcheckable
      ~f_inverse:Conv.to_quickcheckable
  ;;
end
