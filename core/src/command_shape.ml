include Command.Shape

module Stable = struct
  open! Stable_internal
  open! Ppx_compare_lib.Builtin

  module Anons = struct
    module Grammar = struct
      module V1 = struct
        include Command.Shape.Stable.Anons.Grammar.V1

        type t = Command.Shape.Stable.Anons.Grammar.V1.t =
          | Zero
          | One of string
          | Many of t
          | Maybe of t
          | Concat of t list
          | Ad_hoc of string
        [@@deriving bin_io]

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| a17fd34ec213e508db450f6469f7fe99 |}]
        ;;
      end
    end

    module V2 = struct
      include Command.Shape.Stable.Anons.V2

      type t = Command.Shape.Stable.Anons.V2.t =
        | Usage of string
        | Grammar of Grammar.V1.t
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 081d9ec167903f8f8c49cbf8e3fb3a66 |}]
      ;;
    end
  end

  module Flag_info = struct
    module V1 = struct
      include Command.Shape.Stable.Flag_info.V1

      type t = Command.Shape.Stable.Flag_info.V1.t =
        { name : string
        ; doc : string
        ; aliases : string list
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| bd8d6fb7a662d2c0b5e0d2026c6d2d21 |}]
      ;;
    end
  end

  module Base_info = struct
    module V2 = struct
      include Command.Shape.Stable.Base_info.V2

      type t = Command.Shape.Stable.Base_info.V2.t =
        { summary : string
        ; readme : string option
        ; anons : Anons.V2.t
        ; flags : Flag_info.V1.t list
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 8faac1e8d9deb0baaa56ac8ebf85b498 |}]
      ;;
    end
  end

  module Group_info = struct
    type a = Dummy_type_because_we_cannot_digest_type_constructors_only_concrete_types
    [@@deriving bin_io]

    module V2 = struct
      include Command.Shape.Stable.Group_info.V2

      type 'a t = 'a Command.Shape.Stable.Group_info.V2.t =
        { summary : string
        ; readme : string option
        ; subcommands : (string * 'a) List.Stable.V1.t Lazy.Stable.V1.t
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: a t];
        [%expect {| 2cc3eeb58d12d8fe4400009e592d7827 |}]
      ;;
    end
  end

  module Exec_info = struct
    module V3 = struct
      include Command.Shape.Stable.Exec_info.V3

      type t = Command.Shape.Stable.Exec_info.V3.t =
        { summary : string
        ; readme : string option
        ; working_dir : string
        ; path_to_exe : string
        ; child_subcommand : string list
        }
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| c0c8256e9238cdd8f2ec1f8785e02ae0 |}]
      ;;
    end
  end

  module Fully_forced = struct
    module V1 = struct
      include Command.Shape.Stable.Fully_forced.V1

      type t = Command.Shape.Stable.Fully_forced.V1.t =
        | Basic of Base_info.V2.t
        | Group of t Group_info.V2.t
        | Exec of Exec_info.V3.t * t
      [@@deriving bin_io]

      let%expect_test _ =
        print_endline [%bin_digest: t];
        [%expect {| 981154ef3919437c6c822619882841d4 |}]
      ;;
    end
  end
end
