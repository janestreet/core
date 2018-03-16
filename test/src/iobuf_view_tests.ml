open! Core
open! Expect_test_helpers_kernel

let string =
  "`Boston is populous' is about Boston and contains `Boston'; ``Boston' is disyllabic' \
   is about `Boston' and contains ``Boston''.  ``Boston'' designates `Boston', which in \
   turn designates Boston.  To mention Boston we use `Boston' or a synonym, and to \
   mention `Boston' we use ``Boston'' or a synonym.  ``Boston'' contains six letters and \
   just one pair of quotation marks; `Boston' contains six letters and no quotation \
   marks; and Boston contains some 800,000 people."

let iobuf = Iobuf.of_string string

(* set limits and window *)
let () =
  Iobuf.advance iobuf      60;
  Iobuf.resize  iobuf ~len:67;
  Iobuf.narrow  iobuf;
  Iobuf.advance iobuf       1;
  Iobuf.resize  iobuf ~len:22

let iobuf = iobuf |> Iobuf.no_seek |> Iobuf.read_only

module T = struct type ('a, 'b) t = ('a, 'b) Iobuf.t end

let%test_module "Window" =
  (module (struct
    open Iobuf.Window

    include T

    module Hexdump = struct
      open Hexdump

      include T

      let sexp_of_t = sexp_of_t

      let%expect_test "sexp_of_t" =
        print_s [%sexp (iobuf : (_, _) t)];
        [%expect {|
          ("00000000  60 42 6f 73 74 6f 6e 27  20 69 73 20 64 69 73 79  |`Boston' is disy|"
           "00000010  6c 6c 61 62 69 63                                 |llabic|") |}]

      let to_string_hum = to_string_hum

      let%expect_test "to_string_hum" =
        print_endline (to_string_hum iobuf);
        [%expect {|
          00000000  60 42 6f 73 74 6f 6e 27  20 69 73 20 64 69 73 79  |`Boston' is disy|
          00000010  6c 6c 61 62 69 63                                 |llabic| |}]

      let to_sequence = to_sequence

      let%expect_test "to_sequence" =
        print_s [%sexp (to_sequence iobuf |> Sequence.to_list : string list)];
        [%expect {|
          ("00000000  60 42 6f 73 74 6f 6e 27  20 69 73 20 64 69 73 79  |`Boston' is disy|"
           "00000010  6c 6c 61 62 69 63                                 |llabic|") |}]
    end
  end : module type of Iobuf.Window))

let%test_module "Limits" =
  (module (struct
    open Iobuf.Limits

    include T

    module Hexdump = struct
      open Hexdump

      include T

      let sexp_of_t = sexp_of_t

      let%expect_test "sexp_of_t" =
        print_s [%sexp (iobuf : (_, _) t)];
        [%expect {|
          ("00000000  60 60 42 6f 73 74 6f 6e  27 20 69 73 20 64 69 73  |``Boston' is dis|"
           "00000010  79 6c 6c 61 62 69 63 27  20 69 73 20 61 62 6f 75  |yllabic' is abou|"
           "00000020  74 20 60 42 6f 73 74 6f  6e 27 20 61 6e 64 20 63  |t `Boston' and c|"
           "00000030  6f 6e 74 61 69 6e 73 20  60 60 42 6f 73 74 6f 6e  |ontains ``Boston|"
           "00000040  27 27 2e                                          |''.|") |}]

      let to_string_hum = to_string_hum

      let%expect_test "to_string_hum" =
        print_endline (to_string_hum iobuf);
        [%expect {|
          00000000  60 60 42 6f 73 74 6f 6e  27 20 69 73 20 64 69 73  |``Boston' is dis|
          00000010  79 6c 6c 61 62 69 63 27  20 69 73 20 61 62 6f 75  |yllabic' is abou|
          00000020  74 20 60 42 6f 73 74 6f  6e 27 20 61 6e 64 20 63  |t `Boston' and c|
          00000030  6f 6e 74 61 69 6e 73 20  60 60 42 6f 73 74 6f 6e  |ontains ``Boston|
          00000040  27 27 2e                                          |''.| |}]

      let to_sequence = to_sequence

      let%expect_test "to_sequence" =
        print_s [%sexp (to_sequence iobuf |> Sequence.to_list : string list)];
        [%expect {|
          ("00000000  60 60 42 6f 73 74 6f 6e  27 20 69 73 20 64 69 73  |``Boston' is dis|"
           "00000010  79 6c 6c 61 62 69 63 27  20 69 73 20 61 62 6f 75  |yllabic' is abou|"
           "00000020  74 20 60 42 6f 73 74 6f  6e 27 20 61 6e 64 20 63  |t `Boston' and c|"
           "00000030  6f 6e 74 61 69 6e 73 20  60 60 42 6f 73 74 6f 6e  |ontains ``Boston|"
           "00000040  27 27 2e                                          |''.|") |}]
    end
  end : module type of Iobuf.Limits))

let%test_module "Hexdump" =
  (module (struct
    open Iobuf.Hexdump

    include T

    let sexp_of_t = sexp_of_t

    let%expect_test "sexp_of_t" =
      print_s [%sexp (iobuf : (_, _) t)];
      [%expect {|
          ((window (
             "00000001  60 42 6f 73 74 6f 6e 27  20 69 73 20 64 69 73 79  |`Boston' is disy|"
             "00000011  6c 6c 61 62 69 63                                 |llabic|"))
           (limits (
             "00000000  60 60 42 6f 73 74 6f 6e  27 20 69 73 20 64 69 73  |``Boston' is dis|"
             "00000010  79 6c 6c 61 62 69 63 27  20 69 73 20 61 62 6f 75  |yllabic' is abou|"
             "00000020  74 20 60 42 6f 73 74 6f  6e 27 20 61 6e 64 20 63  |t `Boston' and c|"
             "00000030  6f 6e 74 61 69 6e 73 20  60 60 42 6f 73 74 6f 6e  |ontains ``Boston|"
             "00000040  27 27 2e                                          |''.|"))) |}]

    let to_sequence = to_sequence

    let%expect_test "to_sequence" =
      print_s [%sexp (Sequence.to_list (to_sequence iobuf) : string list)];
      [%expect {|
          (Window
           "  00000001  60 42 6f 73 74 6f 6e 27  20 69 73 20 64 69 73 79  |`Boston' is disy|"
           "  00000011  6c 6c 61 62 69 63                                 |llabic|"
           Limits
           "  00000000  60 60 42 6f 73 74 6f 6e  27 20 69 73 20 64 69 73  |``Boston' is dis|"
           "  00000010  79 6c 6c 61 62 69 63 27  20 69 73 20 61 62 6f 75  |yllabic' is abou|"
           "  00000020  74 20 60 42 6f 73 74 6f  6e 27 20 61 6e 64 20 63  |t `Boston' and c|"
           "  00000030  6f 6e 74 61 69 6e 73 20  60 60 42 6f 73 74 6f 6e  |ontains ``Boston|"
           "  00000040  27 27 2e                                          |''.|") |}]

    let to_string_hum = to_string_hum

    let%expect_test "to_string_hum" =
      print_endline (to_string_hum iobuf);
      [%expect {|
          Window
            00000001  60 42 6f 73 74 6f 6e 27  20 69 73 20 64 69 73 79  |`Boston' is disy|
            00000011  6c 6c 61 62 69 63                                 |llabic|
          Limits
            00000000  60 60 42 6f 73 74 6f 6e  27 20 69 73 20 64 69 73  |``Boston' is dis|
            00000010  79 6c 6c 61 62 69 63 27  20 69 73 20 61 62 6f 75  |yllabic' is abou|
            00000020  74 20 60 42 6f 73 74 6f  6e 27 20 61 6e 64 20 63  |t `Boston' and c|
            00000030  6f 6e 74 61 69 6e 73 20  60 60 42 6f 73 74 6f 6e  |ontains ``Boston|
            00000040  27 27 2e                                          |''.| |}]
  end : module type of Iobuf.Hexdump))

let%test_module "Debug" =
  (module (struct
    open Iobuf.Debug

    include T

    module Hexdump = struct
      open Hexdump

      include T

      let sexp_of_t = sexp_of_t

      let%expect_test "sexp_of_t" =
        print_s [%sexp (iobuf : (_, _) t)];
        [%expect {|
          ((window (
             "0000003d  60 42 6f 73 74 6f 6e 27  20 69 73 20 64 69 73 79  |`Boston' is disy|"
             "0000004d  6c 6c 61 62 69 63                                 |llabic|"))
           (limits (
             "0000003c  60 60 42 6f 73 74 6f 6e  27 20 69 73 20 64 69 73  |``Boston' is dis|"
             "0000004c  79 6c 6c 61 62 69 63 27  20 69 73 20 61 62 6f 75  |yllabic' is abou|"
             "0000005c  74 20 60 42 6f 73 74 6f  6e 27 20 61 6e 64 20 63  |t `Boston' and c|"
             "0000006c  6f 6e 74 61 69 6e 73 20  60 60 42 6f 73 74 6f 6e  |ontains ``Boston|"
             "0000007c  27 27 2e                                          |''.|"))
           (buffer (
             "00000000  60 42 6f 73 74 6f 6e 20  69 73 20 70 6f 70 75 6c  |`Boston is popul|"
             "00000010  6f 75 73 27 20 69 73 20  61 62 6f 75 74 20 42 6f  |ous' is about Bo|"
             "00000020  73 74 6f 6e 20 61 6e 64  20 63 6f 6e 74 61 69 6e  |ston and contain|"
             "00000030  73 20 60 42 6f 73 74 6f  6e 27 3b 20 60 60 42 6f  |s `Boston'; ``Bo|"
             "00000040  73 74 6f 6e 27 20 69 73  20 64 69 73 79 6c 6c 61  |ston' is disylla|"
             "00000050  62 69 63 27 20 69 73 20  61 62 6f 75 74 20 60 42  |bic' is about `B|"
             "00000060  6f 73 74 6f 6e 27 20 61  6e 64 20 63 6f 6e 74 61  |oston' and conta|"
             "00000070  69 6e 73 20 60 60 42 6f  73 74 6f 6e 27 27 2e 20  |ins ``Boston''. |"
             "00000080  20 60 60 42 6f 73 74 6f  6e 27 27 20 64 65 73 69  | ``Boston'' desi|"
             "00000090  67 6e 61 74 65 73 20 60  42 6f 73 74 6f 6e 27 2c  |gnates `Boston',|"
             "000000a0  20 77 68 69 63 68 20 69  6e 20 74 75 72 6e 20 64  | which in turn d|"
             "000000b0  65 73 69 67 6e 61 74 65  73 20 42 6f 73 74 6f 6e  |esignates Boston|"
             "000000c0  2e 20 20 54 6f 20 6d 65  6e 74 69 6f 6e 20 42 6f  |.  To mention Bo|"
             "000000d0  73 74 6f 6e 20 77 65 20  75 73 65 20 60 42 6f 73  |ston we use `Bos|"
             "000000e0  74 6f 6e 27 20 6f 72 20  61 20 73 79 6e 6f 6e 79  |ton' or a synony|"
             "000000f0  6d 2c 20 61 6e 64 20 74  6f 20 6d 65 6e 74 69 6f  |m, and to mentio|"
             "00000100  6e 20 60 42 6f 73 74 6f  6e 27 20 77 65 20 75 73  |n `Boston' we us|"
             "00000110  65 20 60 60 42 6f 73 74  6f 6e 27 27 20 6f 72 20  |e ``Boston'' or |"
             "00000120  61 20 73 79 6e 6f 6e 79  6d 2e 20 20 60 60 42 6f  |a synonym.  ``Bo|"
             "00000130  73 74 6f 6e 27 27 20 63  6f 6e 74 61 69 6e 73 20  |ston'' contains |"
             "00000140  73 69 78 20 6c 65 74 74  65 72 73 20 61 6e 64 20  |six letters and |"
             "00000150  6a 75 73 74 20 6f 6e 65  20 70 61 69 72 20 6f 66  |just one pair of|"
             "00000160  20 71 75 6f 74 61 74 69  6f 6e 20 6d 61 72 6b 73  | quotation marks|"
             "00000170  3b 20 60 42 6f 73 74 6f  6e 27 20 63 6f 6e 74 61  |; `Boston' conta|"
             "00000180  69 6e 73 20 73 69 78 20  6c 65 74 74 65 72 73 20  |ins six letters |"
             "00000190  61 6e 64 20 6e 6f 20 71  75 6f 74 61 74 69 6f 6e  |and no quotation|"
             "000001a0  20 6d 61 72 6b 73 3b 20  61 6e 64 20 42 6f 73 74  | marks; and Bost|"
             "000001b0  6f 6e 20 63 6f 6e 74 61  69 6e 73 20 73 6f 6d 65  |on contains some|"
             "000001c0  20 38 30 30 2c 30 30 30  20 70 65 6f 70 6c 65 2e  | 800,000 people.|"))) |}]

      let to_sequence = to_sequence

      let%expect_test "to_sequence" =
        print_s [%sexp (Sequence.to_list (to_sequence iobuf) : string list)];
        [%expect {|
          (Window
           "  0000003d  60 42 6f 73 74 6f 6e 27  20 69 73 20 64 69 73 79  |`Boston' is disy|"
           "  0000004d  6c 6c 61 62 69 63                                 |llabic|"
           Limits
           "  0000003c  60 60 42 6f 73 74 6f 6e  27 20 69 73 20 64 69 73  |``Boston' is dis|"
           "  0000004c  79 6c 6c 61 62 69 63 27  20 69 73 20 61 62 6f 75  |yllabic' is abou|"
           "  0000005c  74 20 60 42 6f 73 74 6f  6e 27 20 61 6e 64 20 63  |t `Boston' and c|"
           "  0000006c  6f 6e 74 61 69 6e 73 20  60 60 42 6f 73 74 6f 6e  |ontains ``Boston|"
           "  0000007c  27 27 2e                                          |''.|"
           Buffer
           "  00000000  60 42 6f 73 74 6f 6e 20  69 73 20 70 6f 70 75 6c  |`Boston is popul|"
           "  00000010  6f 75 73 27 20 69 73 20  61 62 6f 75 74 20 42 6f  |ous' is about Bo|"
           "  00000020  73 74 6f 6e 20 61 6e 64  20 63 6f 6e 74 61 69 6e  |ston and contain|"
           "  00000030  73 20 60 42 6f 73 74 6f  6e 27 3b 20 60 60 42 6f  |s `Boston'; ``Bo|"
           "  00000040  73 74 6f 6e 27 20 69 73  20 64 69 73 79 6c 6c 61  |ston' is disylla|"
           "  00000050  62 69 63 27 20 69 73 20  61 62 6f 75 74 20 60 42  |bic' is about `B|"
           "  00000060  6f 73 74 6f 6e 27 20 61  6e 64 20 63 6f 6e 74 61  |oston' and conta|"
           "  00000070  69 6e 73 20 60 60 42 6f  73 74 6f 6e 27 27 2e 20  |ins ``Boston''. |"
           "  00000080  20 60 60 42 6f 73 74 6f  6e 27 27 20 64 65 73 69  | ``Boston'' desi|"
           "  00000090  67 6e 61 74 65 73 20 60  42 6f 73 74 6f 6e 27 2c  |gnates `Boston',|"
           "  000000a0  20 77 68 69 63 68 20 69  6e 20 74 75 72 6e 20 64  | which in turn d|"
           "  000000b0  65 73 69 67 6e 61 74 65  73 20 42 6f 73 74 6f 6e  |esignates Boston|"
           "  000000c0  2e 20 20 54 6f 20 6d 65  6e 74 69 6f 6e 20 42 6f  |.  To mention Bo|"
           "  000000d0  73 74 6f 6e 20 77 65 20  75 73 65 20 60 42 6f 73  |ston we use `Bos|"
           "  000000e0  74 6f 6e 27 20 6f 72 20  61 20 73 79 6e 6f 6e 79  |ton' or a synony|"
           "  000000f0  6d 2c 20 61 6e 64 20 74  6f 20 6d 65 6e 74 69 6f  |m, and to mentio|"
           "  00000100  6e 20 60 42 6f 73 74 6f  6e 27 20 77 65 20 75 73  |n `Boston' we us|"
           "  00000110  65 20 60 60 42 6f 73 74  6f 6e 27 27 20 6f 72 20  |e ``Boston'' or |"
           "  00000120  61 20 73 79 6e 6f 6e 79  6d 2e 20 20 60 60 42 6f  |a synonym.  ``Bo|"
           "  00000130  73 74 6f 6e 27 27 20 63  6f 6e 74 61 69 6e 73 20  |ston'' contains |"
           "  00000140  73 69 78 20 6c 65 74 74  65 72 73 20 61 6e 64 20  |six letters and |"
           "  00000150  6a 75 73 74 20 6f 6e 65  20 70 61 69 72 20 6f 66  |just one pair of|"
           "  00000160  20 71 75 6f 74 61 74 69  6f 6e 20 6d 61 72 6b 73  | quotation marks|"
           "  00000170  3b 20 60 42 6f 73 74 6f  6e 27 20 63 6f 6e 74 61  |; `Boston' conta|"
           "  00000180  69 6e 73 20 73 69 78 20  6c 65 74 74 65 72 73 20  |ins six letters |"
           "  00000190  61 6e 64 20 6e 6f 20 71  75 6f 74 61 74 69 6f 6e  |and no quotation|"
           "  000001a0  20 6d 61 72 6b 73 3b 20  61 6e 64 20 42 6f 73 74  | marks; and Bost|"
           "  000001b0  6f 6e 20 63 6f 6e 74 61  69 6e 73 20 73 6f 6d 65  |on contains some|"
           "  000001c0  20 38 30 30 2c 30 30 30  20 70 65 6f 70 6c 65 2e  | 800,000 people.|") |}]

      let to_string_hum = to_string_hum

      let%expect_test "to_string_hum" =
        print_endline (to_string_hum iobuf);
        [%expect {|
          Window
            0000003d  60 42 6f 73 74 6f 6e 27  20 69 73 20 64 69 73 79  |`Boston' is disy|
            0000004d  6c 6c 61 62 69 63                                 |llabic|
          Limits
            0000003c  60 60 42 6f 73 74 6f 6e  27 20 69 73 20 64 69 73  |``Boston' is dis|
            0000004c  79 6c 6c 61 62 69 63 27  20 69 73 20 61 62 6f 75  |yllabic' is abou|
            0000005c  74 20 60 42 6f 73 74 6f  6e 27 20 61 6e 64 20 63  |t `Boston' and c|
            0000006c  6f 6e 74 61 69 6e 73 20  60 60 42 6f 73 74 6f 6e  |ontains ``Boston|
            0000007c  27 27 2e                                          |''.|
          Buffer
            00000000  60 42 6f 73 74 6f 6e 20  69 73 20 70 6f 70 75 6c  |`Boston is popul|
            00000010  6f 75 73 27 20 69 73 20  61 62 6f 75 74 20 42 6f  |ous' is about Bo|
            00000020  73 74 6f 6e 20 61 6e 64  20 63 6f 6e 74 61 69 6e  |ston and contain|
            00000030  73 20 60 42 6f 73 74 6f  6e 27 3b 20 60 60 42 6f  |s `Boston'; ``Bo|
            00000040  73 74 6f 6e 27 20 69 73  20 64 69 73 79 6c 6c 61  |ston' is disylla|
            00000050  62 69 63 27 20 69 73 20  61 62 6f 75 74 20 60 42  |bic' is about `B|
            00000060  6f 73 74 6f 6e 27 20 61  6e 64 20 63 6f 6e 74 61  |oston' and conta|
            00000070  69 6e 73 20 60 60 42 6f  73 74 6f 6e 27 27 2e 20  |ins ``Boston''. |
            00000080  20 60 60 42 6f 73 74 6f  6e 27 27 20 64 65 73 69  | ``Boston'' desi|
            00000090  67 6e 61 74 65 73 20 60  42 6f 73 74 6f 6e 27 2c  |gnates `Boston',|
            000000a0  20 77 68 69 63 68 20 69  6e 20 74 75 72 6e 20 64  | which in turn d|
            000000b0  65 73 69 67 6e 61 74 65  73 20 42 6f 73 74 6f 6e  |esignates Boston|
            000000c0  2e 20 20 54 6f 20 6d 65  6e 74 69 6f 6e 20 42 6f  |.  To mention Bo|
            000000d0  73 74 6f 6e 20 77 65 20  75 73 65 20 60 42 6f 73  |ston we use `Bos|
            000000e0  74 6f 6e 27 20 6f 72 20  61 20 73 79 6e 6f 6e 79  |ton' or a synony|
            000000f0  6d 2c 20 61 6e 64 20 74  6f 20 6d 65 6e 74 69 6f  |m, and to mentio|
            00000100  6e 20 60 42 6f 73 74 6f  6e 27 20 77 65 20 75 73  |n `Boston' we us|
            00000110  65 20 60 60 42 6f 73 74  6f 6e 27 27 20 6f 72 20  |e ``Boston'' or |
            00000120  61 20 73 79 6e 6f 6e 79  6d 2e 20 20 60 60 42 6f  |a synonym.  ``Bo|
            00000130  73 74 6f 6e 27 27 20 63  6f 6e 74 61 69 6e 73 20  |ston'' contains |
            00000140  73 69 78 20 6c 65 74 74  65 72 73 20 61 6e 64 20  |six letters and |
            00000150  6a 75 73 74 20 6f 6e 65  20 70 61 69 72 20 6f 66  |just one pair of|
            00000160  20 71 75 6f 74 61 74 69  6f 6e 20 6d 61 72 6b 73  | quotation marks|
            00000170  3b 20 60 42 6f 73 74 6f  6e 27 20 63 6f 6e 74 61  |; `Boston' conta|
            00000180  69 6e 73 20 73 69 78 20  6c 65 74 74 65 72 73 20  |ins six letters |
            00000190  61 6e 64 20 6e 6f 20 71  75 6f 74 61 74 69 6f 6e  |and no quotation|
            000001a0  20 6d 61 72 6b 73 3b 20  61 6e 64 20 42 6f 73 74  | marks; and Bost|
            000001b0  6f 6e 20 63 6f 6e 74 61  69 6e 73 20 73 6f 6d 65  |on contains some|
            000001c0  20 38 30 30 2c 30 30 30  20 70 65 6f 70 6c 65 2e  | 800,000 people.| |}]
    end
  end : module type of Iobuf.Debug))
