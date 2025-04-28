open! Core
open Expect_test_helpers_core

let single_example_string = "Az1_ \000\255"
let example_chars = String.to_list single_example_string

let example_strings =
  List.init
    (String.length single_example_string + 1)
    ~f:(fun n -> String.prefix single_example_string n)
;;

module type Type = sig
  type 'a t [@@deriving bin_io, compare, sexp, stable_witness]

  val wrap : 'a -> 'a t
end

let test (module Type : Type) ~expect =
  (* Test [Type] to see conversions. *)
  print_and_check_stable_type
    (module struct
      type t = Char.Stable.V1.t Type.t [@@deriving bin_io, compare, sexp]
    end)
    (List.map ~f:Type.wrap example_chars);
  print_and_check_stable_type
    (module struct
      type t = String.Stable.V1.t Type.t [@@deriving bin_io, compare, sexp]
    end)
    (List.map ~f:Type.wrap example_strings);
  expect ();
  (* Now test without [Type] using the same [[%expect]] block to see that behavior and
     bin-shape are unchanged. *)
  print_and_check_stable_type
    (module struct
      type t = Char.Stable.V1.t [@@deriving bin_io, compare, sexp]
    end)
    example_chars;
  print_and_check_stable_type
    (module struct
      type t = String.Stable.V1.t [@@deriving bin_io, compare, sexp]
    end)
    example_strings;
  expect ()
;;

module _ : module type of struct
  include Modes.Stable
end [@remove_aliases] = struct
  module Global = struct
    module V1 = struct
      type 'a t = 'a Modes.Stable.Global.V1.t
      [@@deriving bin_io ~localize, compare ~localize, sexp, stable_witness]

      let map = Modes.Stable.Global.V1.map

      let%expect_test _ =
        test
          (module struct
            include Modes.Stable.Global.V1

            let wrap = Modes.Global.wrap
          end)
          ~expect:(fun () ->
            [%expect
              {|
              (bin_shape_digest 84610d32d63dcff5c93f1033ec8cb1d5)
              ((sexp   A)
               (bin_io A))
              ((sexp   z)
               (bin_io z))
              ((sexp   1)
               (bin_io 1))
              ((sexp   _)
               (bin_io _))
              ((sexp   " ")
               (bin_io " "))
              ((sexp   "\000")
               (bin_io "\000"))
              ((sexp   "\255")
               (bin_io "\255"))
              (bin_shape_digest d9a8da25d5656b016fb4dbdc2e4197fb)
              ((sexp   "")
               (bin_io "\000"))
              ((sexp   A)
               (bin_io "\001A"))
              ((sexp   Az)
               (bin_io "\002Az"))
              ((sexp   Az1)
               (bin_io "\003Az1"))
              ((sexp   Az1_)
               (bin_io "\004Az1_"))
              ((sexp   "Az1_ ")
               (bin_io "\005Az1_ "))
              ((sexp   "Az1_ \000")
               (bin_io "\006Az1_ \000"))
              ((sexp   "Az1_ \000\255")
               (bin_io "\007Az1_ \000\255"))
              |}]);
        [%expect {| |}]
      ;;
    end
  end
end
