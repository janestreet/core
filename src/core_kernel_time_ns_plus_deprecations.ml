include Core_kernel.Time_ns

let arg_type = `Use_Time_ns_unix
let comparator = `Use_Time_ns_unix
let get_sexp_zone = `Use_Time_ns_unix
let interruptible_pause = `Use_Time_ns_unix
let of_date_ofday_zoned = `Use_Time_ns_unix
let of_string = `Use_Time_ns_unix
let of_string_abs = `Use_Time_ns_unix
let of_string_fix_proto = `Use_Time_ns_unix
let of_string_gen = `Use_Time_ns_unix
let pause = `Use_Time_ns_unix
let pause_forever = `Use_Time_ns_unix
let pp = `Use_Time_ns_unix
let set_sexp_zone = `Use_Time_ns_unix
let sexp_of_t = `Use_Time_ns_unix
let sexp_of_t_abs = `Use_Time_ns_unix
let t_of_sexp = `Use_Time_ns_unix
let t_of_sexp_abs = `Use_Time_ns_unix
let to_date_ofday_zoned = `Use_Time_ns_unix
let to_ofday_zoned = `Use_Time_ns_unix
let to_string = `Use_Time_ns_unix
let to_string_fix_proto = `Use_Time_ns_unix
let validate_bound = `Use_Time_ns_unix
let validate_lbound = `Use_Time_ns_unix
let validate_ubound = `Use_Time_ns_unix

module Hash_queue = struct end
module Hash_set = struct end
module Map = struct end

module Ofday = struct
  include Ofday

  let arg_type = `Use_Time_ns_unix
  let now = `Use_Time_ns_unix
  let of_ofday_float_round_nearest = `Use_Time_ns_unix
  let of_ofday_float_round_nearest_microsecond = `Use_Time_ns_unix
  let to_ofday_float_round_nearest = `Use_Time_ns_unix
  let to_ofday_float_round_nearest_microsecond = `Use_Time_ns_unix

  module Option = struct end
  module Zoned = struct end
end

module Option = struct end
module Replace_polymorphic_compare = struct end
module Set = struct end

module Span = struct
  include Span

  let arg_type = `Use_Time_ns_unix

  module Option = struct end
  module Stable = struct end
end

module Stable = struct
  include Stable

  module Ofday = struct
    include Ofday
    module Option = struct end
  end

  module Option = struct end
  module V1 = struct end
end

module Table = struct end
module Zone = struct end
