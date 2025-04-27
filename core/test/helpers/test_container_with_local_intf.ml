(** Testing implementations of [Container_with_local] by comparison to an implementation
    of the corresponding [Container] or [Indexed_container] interface. *)

open! Base
open Expect_test_helpers_base

module Definitions = struct
  module type Indexed_container_with_creators = sig
    type 'a t [@@deriving equal, globalize, quickcheck, sexp_of]

    type 'a elt
    [@@deriving compare ~localize, equal ~localize, globalize, quickcheck, sexp_of]

    type 'a concat [@@deriving quickcheck, sexp_of]

    module Global :
      Indexed_container.Generic_with_creators
      with type ('a, 'phantom1, 'phantom2) t := 'a t
       and type 'a elt := 'a elt
       and type ('a, 'phantom1, 'phantom2) concat := 'a concat

    module Local :
      Container_with_local.Generic_indexed_with_creators
      with type ('a, 'phantom1, 'phantom2) t := 'a t
       and type 'a elt := 'a elt
       and type ('a, 'phantom1, 'phantom2) concat := 'a concat
  end

  module type Input = sig
    type t [@@deriving quickcheck, sexp_of]
  end

  module type Output = sig
    type t [@@deriving equal, globalize, sexp_of]
  end
end

module type Test_container_with_local = sig
  include module type of struct
    include Definitions
  end

  (** Test a module satisfying a [Container_with_local] interface. *)
  val test_indexed_container_with_creators
    :  here:[%call_pos]
    -> ?cr:CR.t
    -> (module Indexed_container_with_creators)
    -> unit

  (** Test a locally-allocating function against a globally-allocating function. Compares
      the results of the two functions for equality. Also tests that the
      locally-allocating function performs no heap allocation. If [?noalloc] is provided,
      tests that for no heap allocation instead. This helps if the function used for
      equality sometimes allocates, for example in cases that raise exceptions. *)
  val test
    :  here:[%call_pos]
    -> (module Input with type t = 'input)
    -> (module Output with type t = 'output)
    -> ?cr:CR.t
    -> ?noalloc:(local_ 'input -> local_ 'output)
    -> (local_ 'input -> local_ 'output)
    -> ('input -> 'output)
    -> unit
end
