(* These checks are here rather than in their corresponding modules because we want to
   check a property of the module as it is exported in Core.Std. *)

open Std

TEST_MODULE = struct

  module Check = Core_kernel.Std.Comparable.Check_sexp_conversion

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

end
