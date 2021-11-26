open! Import
include Base.Or_error

type 'a t = ('a, Error.t) Result.t [@@deriving bin_io]

module Expect_test_config_with_unit_expect = struct
  module IO_run = Base.Or_error

  module IO_flush = struct
    include Expect_test_config.IO_flush

    let to_run t = Base.Or_error.return t
  end

  let run f = ok_exn (f ())
  let sanitize s = s
  let flushed () = true
  let upon_unreleasable_issue = Expect_test_config.upon_unreleasable_issue
end

module Expect_test_config = struct
  module IO = Base.Or_error
  module IO_run = IO

  module IO_flush = struct
    include IO

    let to_run t = t
  end

  let run f = ok_exn (f ())
  let flushed () = true
  let upon_unreleasable_issue = Expect_test_config.upon_unreleasable_issue
  let sanitize s = s
end

module Stable = struct
  module V1 = struct
    type 'a t = ('a, Error.Stable.V1.t) Result.Stable.V1.t
    [@@deriving bin_io, compare, sexp]

    let map x ~f = Result.Stable.V1.map x ~f1:f ~f2:Fn.id
  end

  module V2 = struct
    type 'a t = ('a, Error.Stable.V2.t) Result.Stable.V1.t
    [@@deriving bin_io, compare, sexp]

    let map x ~f = Result.Stable.V1.map x ~f1:f ~f2:Fn.id
  end
end
