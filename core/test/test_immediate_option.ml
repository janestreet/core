open! Core
open! Import
open! Immediate_option

module Imm_opt_int : S_minimal_zero_alloc with type value = int = struct
  type t = int
  type value = int

  let none = 0
  let some v = v
  let is_none v = v = 0
  let unchecked_value v = v
end

module%test [@name "Provide_or_null_conversions"] _ = struct
  module Test_conv (Conv : sig
      val to_or_null : Imm_opt_int.t -> int or_null
      val of_or_null : int or_null -> Imm_opt_int.t
    end) =
  struct
    let%expect_test "to_or_null" =
      let print imm_opt =
        Dynamic.with_temporarily Sexplib.Conv.write_old_option_format false ~f:(fun () ->
          print_s [%sexp (Conv.to_or_null imm_opt : int Or_null.t)])
      in
      print Imm_opt_int.none;
      print (Imm_opt_int.some 1);
      print (Imm_opt_int.some 5);
      [%expect
        {|
        null
        (this 1)
        (this 5)
        |}]
    ;;

    let%expect_test "of_or_null" =
      let print or_null =
        let imm_opt = Conv.of_or_null or_null in
        if Imm_opt_int.is_none imm_opt
        then print_endline "none"
        else print_s [%sexp "some", (Imm_opt_int.unchecked_value imm_opt : int)]
      in
      print Null;
      print (This 1);
      print (This 5);
      [%expect
        {|
        none
        (some 1)
        (some 5)
        |}]
    ;;

    (* ensures functor applications below are correct *)
    let _tested_witness = ()
  end

  module%test Provide_or_null_conversions : sig
    val _tested_witness : unit
  end =
    Test_conv (Provide_or_null_conversions (Imm_opt_int))

  module%test Provide_or_null_conversions_zero_alloc : sig
    val _tested_witness : unit
  end =
    Test_conv (Provide_or_null_conversions_zero_alloc (Imm_opt_int))
end
