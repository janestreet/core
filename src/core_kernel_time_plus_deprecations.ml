include Core_kernel.Time
module Exposed_for_tests = struct end
module Hash_queue = struct end
module Hash_set = struct end
module Map = struct end

module Ofday = struct
  include Ofday

  let arg_type = `Use_Time_unix
  let now = `Use_Time_unix

  module Zoned = struct end
end

module Set = struct end

module Span = struct
  include Span

  let arg_type = `Use_Time_unix
end

module Stable = struct
  include Stable

  module Ofday = struct
    include Ofday
    module Zoned = struct end
  end

  module Span = Span
  module V1 = struct end
  module With_t_of_sexp_abs = struct end

  module With_utc_sexp = struct
    include With_utc_sexp
    module V1 = struct end

    module V2 = struct
      include V2

      let comparator = `Use_Time_unix

      module Map = struct end
      module Set = struct end
    end
  end

  module Zone = struct
    include Zone
    module V1 = struct end
  end
end

module Table = struct end

module Zone = struct
  include Zone
  module Hash_queue = struct end
  module Hash_set = struct end
  module Map = struct end
  module Replace_polymorphic_compare = struct end
  module Set = struct end
  module Table = struct end

  let ( < ) = `Use_Time_unix
  let ( <= ) = `Use_Time_unix
  let ( <> ) = `Use_Time_unix
  let ( = ) = `Use_Time_unix
  let ( > ) = `Use_Time_unix
  let ( >= ) = `Use_Time_unix
  let __bin_read_t__ = `Use_Time_unix
  let arg_type = `Use_Time_unix
  let ascending = `Use_Time_unix
  let between = `Use_Time_unix
  let bin_read_t = `Use_Time_unix
  let bin_reader_t = `Use_Time_unix
  let bin_shape_t = `Use_Time_unix
  let bin_size_t = `Use_Time_unix
  let bin_t = `Use_Time_unix
  let bin_write_t = `Use_Time_unix
  let bin_writer_t = `Use_Time_unix
  let clamp = `Use_Time_unix
  let clamp_exn = `Use_Time_unix
  let comparator = `Use_Time_unix
  let descending = `Use_Time_unix
  let equal = `Use_Time_unix
  let find = `Use_Time_unix
  let find_exn = `Use_Time_unix
  let hash = `Use_Time_unix
  let hash_fold_t = `Use_Time_unix
  let hashable = `Use_Time_unix
  let init = `Use_Time_unix
  let initialized_zones = `Use_Time_unix
  let local = `Use_Time_unix
  let max = `Use_Time_unix
  let min = `Use_Time_unix
  let of_string = `Use_Time_unix
  let pp = `Use_Time_unix
  let t_of_sexp = `Use_Time_unix
  let to_string = `Use_Time_unix
  let validate_bound = `Use_Time_unix
  let validate_lbound = `Use_Time_unix
  let validate_ubound = `Use_Time_unix
end

let arg_type = `Use_Time_unix
let format = `Use_Time_unix
let get_sexp_zone = `Use_Time_unix
let hashable = `Use_Time_unix
let interruptible_pause = `Use_Time_unix
let of_date_ofday_zoned = `Use_Time_unix
let of_string = `Use_Time_unix
let of_string_abs = `Use_Time_unix
let of_string_fix_proto = `Use_Time_unix
let of_string_gen = `Use_Time_unix
let of_tm = `Use_Time_unix
let parse = `Use_Time_unix
let pause = `Use_Time_unix
let pause_forever = `Use_Time_unix
let pp = `Use_Time_unix
let set_sexp_zone = `Use_Time_unix
let sexp_of_t = `Use_Time_unix
let sexp_of_t_abs = `Use_Time_unix
let t_of_sexp = `Use_Time_unix
let t_of_sexp_abs = `Use_Time_unix
let to_date_ofday_zoned = `Use_Time_unix
let to_ofday_zoned = `Use_Time_unix
let to_string = `Use_Time_unix
let to_string_fix_proto = `Use_Time_unix
