(* These checks are here rather than in their corresponding modules because we want to
   check a property of the module as it is exported in Core. *)

open Core

let%test_module _ = (module struct

  module Check = Comparable.Check_sexp_conversion

  include Check (struct
    include Time
    let examples = [ epoch ]
  end)

  include Check (struct
    include Time.Ofday
    let examples = [ start_of_day ]
  end)

  include Check (struct
    include Time.Span
    let examples = [ of_sec 13. ]
  end)

  include Check (struct
    include Month
    let examples = all
  end)

end)

(* This ensures that Time_intf.S fits inside the intersection of Time and Time_ns. *)
include ((Time    : Time_common.S) : sig end)
include ((Time_ns : Time_common.S) : sig end)
