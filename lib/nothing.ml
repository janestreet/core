module Stable = struct
  module V1 = struct
    type t = Nothing0.t with sexp, bin_io, compare
  end
end

module T = struct
  include Stable.V1

  let hash (_ : t) =
    failwith "Nothing.hash: impossible, values of this type don't exist"

  let to_string (_ : t) =
    failwith "Nothing.to_string: impossible, values of this type don't exist"

  let of_string (_ : string) =
    failwith "Nothing.of_string: not supported"
end

include T
include Identifiable.Make (struct
  include T
  let module_name = "Core.Std.Nothing"
end)

let unreachable_code (_ : t) =
  failwith "Nothing.unreachable_code: impossible"

