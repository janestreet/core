open! Core
include Expect_test_helpers_core
module Test_container = Base_test_helpers.Test_container
module Variant = Variantslib.Variant

let () = Sexp.of_int_style := `Underscores
