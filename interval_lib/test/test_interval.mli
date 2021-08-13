val make_stable_unit_tests_v1
  :  coerce:('a Interval_lib.Interval.Stable.V1.Private.t -> 'b)
  -> non_empty:(('a * 'a) * string * string) list
  -> ('b * string * string) list
