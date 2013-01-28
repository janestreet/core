open Std_internal

module Make (T : Stable_unit_test_intf.Arg) : Stable_unit_test_intf.S = struct
  TEST_UNIT "sexp" =
    List.iter T.tests
      ~f:(fun (t, sexp_as_string, _) ->
        let sexp = Sexp.of_string sexp_as_string in
        let serialized_sexp = T.sexp_of_t t in
        if serialized_sexp <> sexp then
          failwiths "sexp serialization mismatch"
            (`Expected sexp, `But_got serialized_sexp)
            (<:sexp_of< [ `Expected of Sexp.t ] * [ `But_got of Sexp.t ] >>);
        let t' = T.t_of_sexp serialized_sexp in
        if not (T.equal t t') then
          failwiths "sexp deserialization mismatch" (`Expected t, `But_got t')
            (<:sexp_of< [ `Expected of T.t ] * [ `But_got of T.t ] >>)
      )

  TEST_UNIT "bin_io" =
    List.iter T.tests
      ~f:(fun (t, _, expected_bin_io) ->
        let binable_m = (module T : Binable.S with type t = T.t) in
        let to_bin_string t = Binable.to_string binable_m t in
        let serialized_bin_io = to_bin_string t in
        if serialized_bin_io <> expected_bin_io then
          failwiths "bin_io serialization mismatch"
            (t, `Expected expected_bin_io, `But_got serialized_bin_io)
            (<:sexp_of< T.t * [ `Expected of string ] * [ `But_got of string ] >>);
        let t' = Binable.of_string binable_m serialized_bin_io in
        if not (T.equal t t') then
          failwiths "bin_io deserialization mismatch" (`Expected t, `But_got t')
            (<:sexp_of< [ `Expected of T.t ] * [ `But_got of T.t ] >>);
      )

end
