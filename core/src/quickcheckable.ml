open! Import
include Quickcheck_intf.Definitions
include Quickcheckable_intf.Definitions

module%template.portable
  [@modality p] Of_quickcheckable
    (Quickcheckable : S)
    (Conv : Conv with type quickcheckable := Quickcheckable.t) : S with type t := Conv.t =
struct
  let quickcheck_generator =
    (Quickcheck.Generator.map [@mode p])
      Quickcheckable.quickcheck_generator
      ~f:Conv.of_quickcheckable
  ;;

  let quickcheck_observer =
    (Quickcheck.Observer.unmap [@mode p])
      Quickcheckable.quickcheck_observer
      ~f:Conv.to_quickcheckable
  ;;

  let quickcheck_shrinker =
    (Quickcheck.Shrinker.map [@mode p])
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

module%template
  [@modality portable] Of_quickcheckable1
    (Quickcheckable : S1
  [@modality portable])
    (Conv : sig
     @@ portable
       include Conv1 with type 'a quickcheckable := 'a Quickcheckable.t
     end) : S1 [@modality portable] with type 'a t := 'a Conv.t = struct
  [@@@mode.default p = (portable, nonportable)]

  let quickcheck_generator generate_a =
    (Quickcheck.Generator.map [@mode p])
      ((Quickcheckable.quickcheck_generator [@mode p]) generate_a)
      ~f:Conv.of_quickcheckable
  ;;

  let quickcheck_observer observe_a =
    (Quickcheck.Observer.unmap [@mode p])
      ((Quickcheckable.quickcheck_observer [@mode p]) observe_a)
      ~f:Conv.to_quickcheckable
  ;;

  let quickcheck_shrinker shrink_a =
    (Quickcheck.Shrinker.map [@mode p])
      ((Quickcheckable.quickcheck_shrinker [@mode p]) shrink_a)
      ~f:Conv.of_quickcheckable
      ~f_inverse:Conv.to_quickcheckable
  ;;
end

module%template.portable
  [@modality p] Of_quickcheckable_filtered
    (Quickcheckable : S)
    (Conv : Conv_filtered with type quickcheckable := Quickcheckable.t) :
  S with type t := Conv.t = struct
  let quickcheck_generator =
    (Quickcheck.Generator.filter_map [@mode p])
      Quickcheckable.quickcheck_generator
      ~f:Conv.of_quickcheckable
  ;;

  let quickcheck_observer =
    (Quickcheck.Observer.unmap [@mode p])
      Quickcheckable.quickcheck_observer
      ~f:Conv.to_quickcheckable
  ;;

  let quickcheck_shrinker =
    (Quickcheck.Shrinker.filter_map [@mode p])
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

module%template
  [@modality portable] Of_quickcheckable_filtered1
    (Quickcheckable : S1
  [@modality portable])
    (Conv : sig
     @@ portable
       include Conv_filtered1 with type 'a quickcheckable := 'a Quickcheckable.t
     end) : S1 [@modality portable] with type 'a t := 'a Conv.t = struct
  [@@@mode.default p = (portable, nonportable)]

  let quickcheck_generator generate_a =
    (Quickcheck.Generator.filter_map [@mode p])
      ((Quickcheckable.quickcheck_generator [@mode p]) generate_a)
      ~f:Conv.of_quickcheckable
  ;;

  let quickcheck_observer observe_a =
    (Quickcheck.Observer.unmap [@mode p])
      ((Quickcheckable.quickcheck_observer [@mode p]) observe_a)
      ~f:Conv.to_quickcheckable
  ;;

  let quickcheck_shrinker shrink_a =
    (Quickcheck.Shrinker.filter_map [@mode p])
      ((Quickcheckable.quickcheck_shrinker [@mode p]) shrink_a)
      ~f:Conv.of_quickcheckable
      ~f_inverse:Conv.to_quickcheckable
  ;;
end
