open! Core
open Expect_test_helpers_core

let require_same here (name1, string1) (name2, string2) =
  require
    here
    (String.equal string1 string2)
    ~if_false_then_print_s:
      (lazy
        [%message
          "output differs unexpectedly"
            ~_:((name1, string1) : string * string)
            ~_:((name2, string2) : string * string)])
;;

let test_m (type a) here (module T : Hexdump.S with type t = a) ?max_lines ?pos ?len t =
  let to_string_hum = T.Hexdump.to_string_hum ?max_lines ?pos ?len t in
  let to_sequence =
    T.Hexdump.to_sequence ?max_lines ?pos ?len t
    |> Sequence.to_list
    |> String.concat ~sep:"\n"
  in
  require_same here ("to_string_hum", to_string_hum) ("to_sequence", to_sequence);
  if List.for_all ~f:Option.is_none [ max_lines; pos; len ]
  then (
    let sexp_of_t =
      [%sexp (t : T.Hexdump.t)] |> [%of_sexp: string list] |> String.concat ~sep:"\n"
    in
    require_same here ("to_string_hum", to_string_hum) ("sexp_of_t", sexp_of_t));
  to_string_hum
;;

let test ?max_lines ?pos ?len string =
  let of_string = test_m [%here] (module String) ?max_lines ?pos ?len string in
  print_endline of_string;
  let of_bigstring =
    test_m [%here] (module Bigstring) ?max_lines ?pos ?len (Bigstring.of_string string)
  in
  require_same
    [%here]
    ("String.Hexdump.to_string_hum", of_string)
    ("Bigstring.Hexdump.to_string_hum", of_bigstring)
;;

let string =
  "Good morning. In less than an hour, aircraft from here will join others from around \
   the world. And you will be launching the largest aerial battle in the history of \
   mankind. \"Mankind.\" That word should have new meaning for all of us today. We can't \
   be consumed by our petty differences anymore. We will be united in our common \
   interests. Perhaps it's fate that today is the Fourth of July, and you will once \
   again be fighting for our freedom... not from tyranny, oppression, or persecution... \
   but from annihilation. We are fighting for our right to live. To exist. And should we \
   win the day, the Fourth of July will no longer be known as an American holiday, but \
   as the day the world declared in one voice: We will not go quietly into the night! We \
   will not vanish without a fight!  We're going to live on! We're going to survive! \
   Today we celebrate our Independence Day!"
;;

let%expect_test _ =
  for len = 0 to 32 do
    printf "%d:\n" len;
    test (String.sub string ~pos:746 ~len)
  done;
  [%expect
    {|
    0:

    1:
    00000000  57                                                |W|
    2:
    00000000  57 65                                             |We|
    3:
    00000000  57 65 20                                          |We |
    4:
    00000000  57 65 20 77                                       |We w|
    5:
    00000000  57 65 20 77 69                                    |We wi|
    6:
    00000000  57 65 20 77 69 6c                                 |We wil|
    7:
    00000000  57 65 20 77 69 6c 6c                              |We will|
    8:
    00000000  57 65 20 77 69 6c 6c 20                           |We will |
    9:
    00000000  57 65 20 77 69 6c 6c 20  6e                       |We will n|
    10:
    00000000  57 65 20 77 69 6c 6c 20  6e 6f                    |We will no|
    11:
    00000000  57 65 20 77 69 6c 6c 20  6e 6f 74                 |We will not|
    12:
    00000000  57 65 20 77 69 6c 6c 20  6e 6f 74 20              |We will not |
    13:
    00000000  57 65 20 77 69 6c 6c 20  6e 6f 74 20 76           |We will not v|
    14:
    00000000  57 65 20 77 69 6c 6c 20  6e 6f 74 20 76 61        |We will not va|
    15:
    00000000  57 65 20 77 69 6c 6c 20  6e 6f 74 20 76 61 6e     |We will not van|
    16:
    00000000  57 65 20 77 69 6c 6c 20  6e 6f 74 20 76 61 6e 69  |We will not vani|
    17:
    00000000  57 65 20 77 69 6c 6c 20  6e 6f 74 20 76 61 6e 69  |We will not vani|
    00000010  73                                                |s|
    18:
    00000000  57 65 20 77 69 6c 6c 20  6e 6f 74 20 76 61 6e 69  |We will not vani|
    00000010  73 68                                             |sh|
    19:
    00000000  57 65 20 77 69 6c 6c 20  6e 6f 74 20 76 61 6e 69  |We will not vani|
    00000010  73 68 20                                          |sh |
    20:
    00000000  57 65 20 77 69 6c 6c 20  6e 6f 74 20 76 61 6e 69  |We will not vani|
    00000010  73 68 20 77                                       |sh w|
    21:
    00000000  57 65 20 77 69 6c 6c 20  6e 6f 74 20 76 61 6e 69  |We will not vani|
    00000010  73 68 20 77 69                                    |sh wi|
    22:
    00000000  57 65 20 77 69 6c 6c 20  6e 6f 74 20 76 61 6e 69  |We will not vani|
    00000010  73 68 20 77 69 74                                 |sh wit|
    23:
    00000000  57 65 20 77 69 6c 6c 20  6e 6f 74 20 76 61 6e 69  |We will not vani|
    00000010  73 68 20 77 69 74 68                              |sh with|
    24:
    00000000  57 65 20 77 69 6c 6c 20  6e 6f 74 20 76 61 6e 69  |We will not vani|
    00000010  73 68 20 77 69 74 68 6f                           |sh witho|
    25:
    00000000  57 65 20 77 69 6c 6c 20  6e 6f 74 20 76 61 6e 69  |We will not vani|
    00000010  73 68 20 77 69 74 68 6f  75                       |sh withou|
    26:
    00000000  57 65 20 77 69 6c 6c 20  6e 6f 74 20 76 61 6e 69  |We will not vani|
    00000010  73 68 20 77 69 74 68 6f  75 74                    |sh without|
    27:
    00000000  57 65 20 77 69 6c 6c 20  6e 6f 74 20 76 61 6e 69  |We will not vani|
    00000010  73 68 20 77 69 74 68 6f  75 74 20                 |sh without |
    28:
    00000000  57 65 20 77 69 6c 6c 20  6e 6f 74 20 76 61 6e 69  |We will not vani|
    00000010  73 68 20 77 69 74 68 6f  75 74 20 61              |sh without a|
    29:
    00000000  57 65 20 77 69 6c 6c 20  6e 6f 74 20 76 61 6e 69  |We will not vani|
    00000010  73 68 20 77 69 74 68 6f  75 74 20 61 20           |sh without a |
    30:
    00000000  57 65 20 77 69 6c 6c 20  6e 6f 74 20 76 61 6e 69  |We will not vani|
    00000010  73 68 20 77 69 74 68 6f  75 74 20 61 20 66        |sh without a f|
    31:
    00000000  57 65 20 77 69 6c 6c 20  6e 6f 74 20 76 61 6e 69  |We will not vani|
    00000010  73 68 20 77 69 74 68 6f  75 74 20 61 20 66 69     |sh without a fi|
    32:
    00000000  57 65 20 77 69 6c 6c 20  6e 6f 74 20 76 61 6e 69  |We will not vani|
    00000010  73 68 20 77 69 74 68 6f  75 74 20 61 20 66 69 67  |sh without a fig|
    |}];
  test string;
  [%expect
    {|
    00000000  47 6f 6f 64 20 6d 6f 72  6e 69 6e 67 2e 20 49 6e  |Good morning. In|
    00000010  20 6c 65 73 73 20 74 68  61 6e 20 61 6e 20 68 6f  | less than an ho|
    00000020  75 72 2c 20 61 69 72 63  72 61 66 74 20 66 72 6f  |ur, aircraft fro|
    00000030  6d 20 68 65 72 65 20 77  69 6c 6c 20 6a 6f 69 6e  |m here will join|
    00000040  20 6f 74 68 65 72 73 20  66 72 6f 6d 20 61 72 6f  | others from aro|
    00000050  75 6e 64 20 74 68 65 20  77 6f 72 6c 64 2e 20 41  |und the world. A|
    00000060  6e 64 20 79 6f 75 20 77  69 6c 6c 20 62 65 20 6c  |nd you will be l|
    00000070  61 75 6e 63 68 69 6e 67  20 74 68 65 20 6c 61 72  |aunching the lar|
    00000080  67 65 73 74 20 61 65 72  69 61 6c 20 62 61 74 74  |gest aerial batt|
    00000090  6c 65 20 69 6e 20 74 68  65 20 68 69 73 74 6f 72  |le in the histor|
    000000a0  79 20 6f 66 20 6d 61 6e  6b 69 6e 64 2e 20 22 4d  |y of mankind. "M|
    000000b0  61 6e 6b 69 6e 64 2e 22  20 54 68 61 74 20 77 6f  |ankind." That wo|
    000000c0  72 64 20 73 68 6f 75 6c  64 20 68 61 76 65 20 6e  |rd should have n|
    000000d0  65 77 20 6d 65 61 6e 69  6e 67 20 66 6f 72 20 61  |ew meaning for a|
    000000e0  6c 6c 20 6f 66 20 75 73  20 74 6f 64 61 79 2e 20  |ll of us today. |
    000000f0  57 65 20 63 61 6e 27 74  20 62 65 20 63 6f 6e 73  |We can't be cons|
    00000100  75 6d 65 64 20 62 79 20  6f 75 72 20 70 65 74 74  |umed by our pett|
    00000110  79 20 64 69 66 66 65 72  65 6e 63 65 73 20 61 6e  |y differences an|
    00000120  79 6d 6f 72 65 2e 20 57  65 20 77 69 6c 6c 20 62  |ymore. We will b|
    00000130  65 20 75 6e 69 74 65 64  20 69 6e 20 6f 75 72 20  |e united in our |
    00000140  63 6f 6d 6d 6f 6e 20 69  6e 74 65 72 65 73 74 73  |common interests|
    00000150  2e 20 50 65 72 68 61 70  73 20 69 74 27 73 20 66  |. Perhaps it's f|
    00000160  61 74 65 20 74 68 61 74  20 74 6f 64 61 79 20 69  |ate that today i|
    00000170  73 20 74 68 65 20 46 6f  75 72 74 68 20 6f 66 20  |s the Fourth of |
    00000180  4a 75 6c 79 2c 20 61 6e  64 20 79 6f 75 20 77 69  |July, and you wi|
    00000190  6c 6c 20 6f 6e 63 65 20  61 67 61 69 6e 20 62 65  |ll once again be|
    000001a0  20 66 69 67 68 74 69 6e  67 20 66 6f 72 20 6f 75  | fighting for ou|
    000001b0  72 20 66 72 65 65 64 6f  6d 2e 2e 2e 20 6e 6f 74  |r freedom... not|
    000001c0  20 66 72 6f 6d 20 74 79  72 61 6e 6e 79 2c 20 6f  | from tyranny, o|
    000001d0  70 70 72 65 73 73 69 6f  6e 2c 20 6f 72 20 70 65  |ppression, or pe|
    000001e0  72 73 65 63 75 74 69 6f  6e 2e 2e 2e 20 62 75 74  |rsecution... but|
    000001f0  20 66 72 6f 6d 20 61 6e  6e 69 68 69 6c 61 74 69  | from annihilati|
    00000200  6f 6e 2e 20 57 65 20 61  72 65 20 66 69 67 68 74  |on. We are fight|
    00000210  69 6e 67 20 66 6f 72 20  6f 75 72 20 72 69 67 68  |ing for our righ|
    00000220  74 20 74 6f 20 6c 69 76  65 2e 20 54 6f 20 65 78  |t to live. To ex|
    00000230  69 73 74 2e 20 41 6e 64  20 73 68 6f 75 6c 64 20  |ist. And should |
    00000240  77 65 20 77 69 6e 20 74  68 65 20 64 61 79 2c 20  |we win the day, |
    00000250  74 68 65 20 46 6f 75 72  74 68 20 6f 66 20 4a 75  |the Fourth of Ju|
    00000260  6c 79 20 77 69 6c 6c 20  6e 6f 20 6c 6f 6e 67 65  |ly will no longe|
    00000270  72 20 62 65 20 6b 6e 6f  77 6e 20 61 73 20 61 6e  |r be known as an|
    00000280  20 41 6d 65 72 69 63 61  6e 20 68 6f 6c 69 64 61  | American holida|
    00000290  79 2c 20 62 75 74 20 61  73 20 74 68 65 20 64 61  |y, but as the da|
    000002a0  79 20 74 68 65 20 77 6f  72 6c 64 20 64 65 63 6c  |y the world decl|
    000002b0  61 72 65 64 20 69 6e 20  6f 6e 65 20 76 6f 69 63  |ared in one voic|
    000002c0  65 3a 20 57 65 20 77 69  6c 6c 20 6e 6f 74 20 67  |e: We will not g|
    000002d0  6f 20 71 75 69 65 74 6c  79 20 69 6e 74 6f 20 74  |o quietly into t|
    000002e0  68 65 20 6e 69 67 68 74  21 20 57 65 20 77 69 6c  |he night! We wil|
    000002f0  6c 20 6e 6f 74 20 76 61  6e 69 73 68 20 77 69 74  |l not vanish wit|
    00000300  68 6f 75 74 20 61 20 66  69 67 68 74 21 20 20 57  |hout a fight!  W|
    00000310  65 27 72 65 20 67 6f 69  6e 67 20 74 6f 20 6c 69  |e're going to li|
    00000320  76 65 20 6f 6e 21 20 57  65 27 72 65 20 67 6f 69  |ve on! We're goi|
    00000330  6e 67 20 74 6f 20 73 75  72 76 69 76 65 21 20 54  |ng to survive! T|
    00000340  6f 64 61 79 20 77 65 20  63 65 6c 65 62 72 61 74  |oday we celebrat|
    00000350  65 20 6f 75 72 20 49 6e  64 65 70 65 6e 64 65 6e  |e our Independen|
    00000360  63 65 20 44 61 79 21                              |ce Day!|
    |}];
  for max_lines = 0 to 10 do
    printf "%d:\n" max_lines;
    test string ~max_lines
  done;
  [%expect
    {|
    0:
    00000000  47 6f 6f 64 20 6d 6f 72  6e 69 6e 67 2e 20 49 6e  |Good morning. In|
    ...
    00000360  63 65 20 44 61 79 21                              |ce Day!|
    1:
    00000000  47 6f 6f 64 20 6d 6f 72  6e 69 6e 67 2e 20 49 6e  |Good morning. In|
    ...
    00000360  63 65 20 44 61 79 21                              |ce Day!|
    2:
    00000000  47 6f 6f 64 20 6d 6f 72  6e 69 6e 67 2e 20 49 6e  |Good morning. In|
    ...
    00000360  63 65 20 44 61 79 21                              |ce Day!|
    3:
    00000000  47 6f 6f 64 20 6d 6f 72  6e 69 6e 67 2e 20 49 6e  |Good morning. In|
    ...
    00000360  63 65 20 44 61 79 21                              |ce Day!|
    4:
    00000000  47 6f 6f 64 20 6d 6f 72  6e 69 6e 67 2e 20 49 6e  |Good morning. In|
    ...
    00000350  65 20 6f 75 72 20 49 6e  64 65 70 65 6e 64 65 6e  |e our Independen|
    00000360  63 65 20 44 61 79 21                              |ce Day!|
    5:
    00000000  47 6f 6f 64 20 6d 6f 72  6e 69 6e 67 2e 20 49 6e  |Good morning. In|
    00000010  20 6c 65 73 73 20 74 68  61 6e 20 61 6e 20 68 6f  | less than an ho|
    ...
    00000350  65 20 6f 75 72 20 49 6e  64 65 70 65 6e 64 65 6e  |e our Independen|
    00000360  63 65 20 44 61 79 21                              |ce Day!|
    6:
    00000000  47 6f 6f 64 20 6d 6f 72  6e 69 6e 67 2e 20 49 6e  |Good morning. In|
    00000010  20 6c 65 73 73 20 74 68  61 6e 20 61 6e 20 68 6f  | less than an ho|
    ...
    00000340  6f 64 61 79 20 77 65 20  63 65 6c 65 62 72 61 74  |oday we celebrat|
    00000350  65 20 6f 75 72 20 49 6e  64 65 70 65 6e 64 65 6e  |e our Independen|
    00000360  63 65 20 44 61 79 21                              |ce Day!|
    7:
    00000000  47 6f 6f 64 20 6d 6f 72  6e 69 6e 67 2e 20 49 6e  |Good morning. In|
    00000010  20 6c 65 73 73 20 74 68  61 6e 20 61 6e 20 68 6f  | less than an ho|
    00000020  75 72 2c 20 61 69 72 63  72 61 66 74 20 66 72 6f  |ur, aircraft fro|
    ...
    00000340  6f 64 61 79 20 77 65 20  63 65 6c 65 62 72 61 74  |oday we celebrat|
    00000350  65 20 6f 75 72 20 49 6e  64 65 70 65 6e 64 65 6e  |e our Independen|
    00000360  63 65 20 44 61 79 21                              |ce Day!|
    8:
    00000000  47 6f 6f 64 20 6d 6f 72  6e 69 6e 67 2e 20 49 6e  |Good morning. In|
    00000010  20 6c 65 73 73 20 74 68  61 6e 20 61 6e 20 68 6f  | less than an ho|
    00000020  75 72 2c 20 61 69 72 63  72 61 66 74 20 66 72 6f  |ur, aircraft fro|
    ...
    00000330  6e 67 20 74 6f 20 73 75  72 76 69 76 65 21 20 54  |ng to survive! T|
    00000340  6f 64 61 79 20 77 65 20  63 65 6c 65 62 72 61 74  |oday we celebrat|
    00000350  65 20 6f 75 72 20 49 6e  64 65 70 65 6e 64 65 6e  |e our Independen|
    00000360  63 65 20 44 61 79 21                              |ce Day!|
    9:
    00000000  47 6f 6f 64 20 6d 6f 72  6e 69 6e 67 2e 20 49 6e  |Good morning. In|
    00000010  20 6c 65 73 73 20 74 68  61 6e 20 61 6e 20 68 6f  | less than an ho|
    00000020  75 72 2c 20 61 69 72 63  72 61 66 74 20 66 72 6f  |ur, aircraft fro|
    00000030  6d 20 68 65 72 65 20 77  69 6c 6c 20 6a 6f 69 6e  |m here will join|
    ...
    00000330  6e 67 20 74 6f 20 73 75  72 76 69 76 65 21 20 54  |ng to survive! T|
    00000340  6f 64 61 79 20 77 65 20  63 65 6c 65 62 72 61 74  |oday we celebrat|
    00000350  65 20 6f 75 72 20 49 6e  64 65 70 65 6e 64 65 6e  |e our Independen|
    00000360  63 65 20 44 61 79 21                              |ce Day!|
    10:
    00000000  47 6f 6f 64 20 6d 6f 72  6e 69 6e 67 2e 20 49 6e  |Good morning. In|
    00000010  20 6c 65 73 73 20 74 68  61 6e 20 61 6e 20 68 6f  | less than an ho|
    00000020  75 72 2c 20 61 69 72 63  72 61 66 74 20 66 72 6f  |ur, aircraft fro|
    00000030  6d 20 68 65 72 65 20 77  69 6c 6c 20 6a 6f 69 6e  |m here will join|
    ...
    00000320  76 65 20 6f 6e 21 20 57  65 27 72 65 20 67 6f 69  |ve on! We're goi|
    00000330  6e 67 20 74 6f 20 73 75  72 76 69 76 65 21 20 54  |ng to survive! T|
    00000340  6f 64 61 79 20 77 65 20  63 65 6c 65 62 72 61 74  |oday we celebrat|
    00000350  65 20 6f 75 72 20 49 6e  64 65 70 65 6e 64 65 6e  |e our Independen|
    00000360  63 65 20 44 61 79 21                              |ce Day!|
    |}];
  test string ~pos:707 ~len:123;
  [%expect
    {|
    000002c3  57 65 20 77 69 6c 6c 20  6e 6f 74 20 67 6f 20 71  |We will not go q|
    000002d3  75 69 65 74 6c 79 20 69  6e 74 6f 20 74 68 65 20  |uietly into the |
    000002e3  6e 69 67 68 74 21 20 57  65 20 77 69 6c 6c 20 6e  |night! We will n|
    000002f3  6f 74 20 76 61 6e 69 73  68 20 77 69 74 68 6f 75  |ot vanish withou|
    00000303  74 20 61 20 66 69 67 68  74 21 20 20 57 65 27 72  |t a fight!  We'r|
    00000313  65 20 67 6f 69 6e 67 20  74 6f 20 6c 69 76 65 20  |e going to live |
    00000323  6f 6e 21 20 57 65 27 72  65 20 67 6f 69 6e 67 20  |on! We're going |
    00000333  74 6f 20 73 75 72 76 69  76 65 21                 |to survive!|
    |}];
  test string ~pos:707 ~max_lines:10;
  [%expect
    {|
    000002c3  57 65 20 77 69 6c 6c 20  6e 6f 74 20 67 6f 20 71  |We will not go q|
    000002d3  75 69 65 74 6c 79 20 69  6e 74 6f 20 74 68 65 20  |uietly into the |
    000002e3  6e 69 67 68 74 21 20 57  65 20 77 69 6c 6c 20 6e  |night! We will n|
    000002f3  6f 74 20 76 61 6e 69 73  68 20 77 69 74 68 6f 75  |ot vanish withou|
    ...
    00000323  6f 6e 21 20 57 65 27 72  65 20 67 6f 69 6e 67 20  |on! We're going |
    00000333  74 6f 20 73 75 72 76 69  76 65 21 20 54 6f 64 61  |to survive! Toda|
    00000343  79 20 77 65 20 63 65 6c  65 62 72 61 74 65 20 6f  |y we celebrate o|
    00000353  75 72 20 49 6e 64 65 70  65 6e 64 65 6e 63 65 20  |ur Independence |
    00000363  44 61 79 21                                       |Day!|
    |}];
  test string ~pos:707 ~max_lines:20;
  [%expect
    {|
    000002c3  57 65 20 77 69 6c 6c 20  6e 6f 74 20 67 6f 20 71  |We will not go q|
    000002d3  75 69 65 74 6c 79 20 69  6e 74 6f 20 74 68 65 20  |uietly into the |
    000002e3  6e 69 67 68 74 21 20 57  65 20 77 69 6c 6c 20 6e  |night! We will n|
    000002f3  6f 74 20 76 61 6e 69 73  68 20 77 69 74 68 6f 75  |ot vanish withou|
    00000303  74 20 61 20 66 69 67 68  74 21 20 20 57 65 27 72  |t a fight!  We'r|
    00000313  65 20 67 6f 69 6e 67 20  74 6f 20 6c 69 76 65 20  |e going to live |
    00000323  6f 6e 21 20 57 65 27 72  65 20 67 6f 69 6e 67 20  |on! We're going |
    00000333  74 6f 20 73 75 72 76 69  76 65 21 20 54 6f 64 61  |to survive! Toda|
    00000343  79 20 77 65 20 63 65 6c  65 62 72 61 74 65 20 6f  |y we celebrate o|
    00000353  75 72 20 49 6e 64 65 70  65 6e 64 65 6e 63 65 20  |ur Independence |
    00000363  44 61 79 21                                       |Day!|
    |}]
;;

let%expect_test "pretty" =
  let test x = print_s ([%sexp_of: String.Hexdump.Pretty.t] x) in
  test "The world is everything that is the case.";
  [%expect {| "The world is everything that is the case." |}];
  test "What\000are\255 these\001weird bytes?\xfe";
  [%expect
    {|
    ("00000000  57 68 61 74 00 61 72 65  ff 20 74 68 65 73 65 01  |What.are. these.|"
     "00000010  77 65 69 72 64 20 62 79  74 65 73 3f fe           |weird bytes?.|")
    |}]
;;
