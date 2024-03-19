open! Core
open! Import

module M1 = struct
  module T = struct
    type t = T of string
  end

  include T

  include
    Deriving_hash.Of_deriving_hash
      (String)
      (struct
        include T

        let to_repr (T s) = s
      end)
end

module M2 = struct
  type t = M1.t [@@deriving hash]
end

let%expect_test (_ [@tags "64-bits-only"]) =
  let s = "foo" in
  print_s [%sexp (String.hash s = M2.hash (T s) : bool)];
  [%expect {| true |}]
;;
