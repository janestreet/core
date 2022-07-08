open! Import
open! Std_internal

module type S_kernel_without_zone = Time0_intf.S
module type S_kernel = Time_intf.S

module Zone_stable = Zone.Stable
include Time.Make (Time_float0)
include Time_float0

module Stable = struct
  include Time_float0.Stable
  module V1 = struct end
  module With_t_of_sexp_abs = struct end

  module Ofday = struct
    include Ofday
    module Zoned = struct end
  end

  module With_utc_sexp = struct
    module V1 = struct end

    module V2 = struct
      type nonrec t = t [@@deriving bin_io, compare, hash]

      let sexp_of_t t = [%sexp (to_string_abs_parts t ~zone:Zone.utc : string list)]
      let stable_witness = Stable_witness.assert_stable

      let t_of_sexp sexp =
        try
          match sexp with
          | Sexp.List [ Sexp.Atom date; Sexp.Atom ofday_and_possibly_zone ] ->
            of_string_gen
              ~default_zone:(fun () -> Zone.utc)
              ~find_zone:(fun _ ->
                of_sexp_error "Time.Stable.With_utc.V2.t_of_sexp: unknown time zone" sexp)
              (date ^ " " ^ ofday_and_possibly_zone)
          | _ -> of_sexp_error "Time.Stable.With_utc.V2.t_of_sexp" sexp
        with
        | Of_sexp_error _ as e -> raise e
        | e ->
          of_sexp_error
            (sprintf "Time.Stable.With_utc.V2.t_of_sexp: %s" (Exn.to_string e))
            sexp
      ;;

      let t_sexp_grammar = Sexplib.Sexp_grammar.coerce Sexplib.Sexp.t_sexp_grammar
      let comparator = `Use_Time_unix

      module Map = struct end
      module Set = struct end
    end
  end

  module Zone = struct
    module V1 = struct end
    include Zone_stable
  end
end

module Ofday = struct
  include Ofday

  let arg_type = `Use_Time_unix
  let now = `Use_Time_unix

  module Zoned = struct end
end

module Span = struct
  include Span

  let arg_type = `Use_Time_unix
end

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

module Exposed_for_tests = struct end
module Hash_queue = struct end
module Hash_set = struct end
module Map = struct end
module Set = struct end
module Table = struct end

let arg_type = `Use_Time_unix
let format = `Use_Time_unix
let get_sexp_zone = `Use_Time_unix
let hashable = `Use_Time_unix
let interruptible_pause = `Use_Time_unix
let of_date_ofday_zoned = `Use_Time_unix
let of_string_abs = `Use_Time_unix
let of_string_fix_proto = `Use_Time_unix
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
let to_string_fix_proto = `Use_Time_unix
