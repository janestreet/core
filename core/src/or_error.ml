open! Import
include Base.Or_error

type 'a t = ('a, Error.t) Result.t
[@@deriving bin_io, diff ~extra_derive:[ sexp ], quickcheck]

module Portable = struct
  type 'a t = ('a, Error.Portable.t) Result.t
  [@@deriving bin_io, diff ~extra_derive:[ sexp ], quickcheck]

  include (Portable : module type of Portable with type 'a t := 'a t)
end

module Expect_test_config = struct
  module IO : module type of Base.Or_error = Base.Or_error

  let run f = ok_exn (f ())
  let sanitize s = s
  let upon_unreleasable_issue = Expect_test_config.upon_unreleasable_issue
  let trailing_output_syntax = `Attribute
end

module Expect_test_config_with_unit_expect = Expect_test_config

module Stable = struct
  module V1 = struct
    type 'a t = ('a, Error.Stable.V1.t) Result.Stable.V1.t
    [@@deriving bin_io, compare, sexp, stable_witness]

    let map x ~f = Result.Stable.V1.map x ~f1:f ~f2:Fn.id
  end

  module V2 = struct
    type 'a t = ('a, Error.Stable.V2.t) Result.Stable.V1.t
    [@@deriving bin_io, compare, equal, sexp, sexp_grammar, stable_witness, diff]

    let map x ~f = Result.Stable.V1.map x ~f1:f ~f2:Fn.id
  end

  module Portable = struct
    module V1 = struct
      type 'a t = ('a, Error.Stable.Portable.V1.t) Result.Stable.V1.t
      [@@deriving bin_io, compare, sexp, stable_witness]

      let map x ~f = Result.Stable.V1.map x ~f1:f ~f2:Fn.id
    end

    module V2 = struct
      type 'a t = ('a, Error.Stable.Portable.V2.t) Result.Stable.V1.t
      [@@deriving
        bin_io, compare, diff, equal, globalize, sexp, sexp_grammar, stable_witness]

      [%%rederive.portable
        type 'a t = ('a, Error.Stable.Portable.V2.t) Result.t [@@deriving hash]]

      let map x ~f = Result.Stable.V1.map x ~f1:f ~f2:Fn.id
    end
  end
end
