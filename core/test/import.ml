open! Core
include Expect_test_helpers_core
module Blit_helpers = Core_test_helpers.Blit_helpers
module Test_container = Base_test_helpers.Test_container
module Test_container_with_local = Core_test_helpers.Test_container_with_local
module Variant = Variantslib.Variant
module Mutex = Basement.Blocking_sync.Mutex [@@alert "-deprecated"]

let () = Dynamic.set_root Sexp.of_int_style `Underscores
