open Core
open OUnit;;

module B = Binary_packing

let test byte_order =
  let inverses inc inc_inc pack unpack min max to_string () =
    let try_it i =
      let buf = Bytes.create 100 in
      pack ~byte_order ~buf ~pos:0 i;
      let i' = unpack ~byte_order ~buf ~pos:0 in
      if i' <> i then
        failwith (Printf.sprintf "failed on input %s; returned %s"
                    (to_string i) (to_string i'))
    in
    let rec f i inc_by =
      if i < max then begin
        try_it i;
        let i' = inc inc_by i in
        if i' > i then
          f (inc inc_by i) (inc_by + inc_inc)
      end in
    f min 1;
    try_it max
  in
  [ "Binary_packing.test" >::
    (fun () ->
       let test () =
         try
           Binary_packing.test ();
           true
         with _ -> false in
       Quickcheck_deprecated.laws_exn "\"Runs without raising\""
         1 (Quickcheck_deprecated.always ()) test
    );

    "[pack|unpack]_signed_8" >::
    inverses (+) 0
      (fun ~byte_order:_ ~buf ~pos i -> B.pack_signed_8 ~buf ~pos i)
      (fun ~byte_order:_ ~buf ~pos -> B.unpack_signed_8 ~buf ~pos)
      (-0x80) 0x7F string_of_int;
    "[pack|unpack]_signed_16" >::
    inverses (+) 0 B.pack_signed_16 B.unpack_signed_16
      (-0x8000) 0x7FFF string_of_int;
    "[pack|unpack]_signed_32" >::
    inverses (fun n -> Int32.(+) (Int32.of_int_exn n)) 1 B.pack_signed_32 B.unpack_signed_32
      (Int32.of_string "-0x80000000") (Int32.of_string "0x7FFFFFFF") Int32.to_string;
    "[pack|unpack]_signed_32_int" >::
    (if Sys.word_size = 64 then
       inverses (+) 1 B.pack_signed_32_int B.unpack_signed_32_int
         (int_of_string "-0x80000000") (int_of_string "0x7FFFFFFF") string_of_int
     else
       (fun () -> ()));
    "[pack|unpack]_signed_64" >:: (
      let buf = Bytes.make 8 'a' in
      let test name to_string p u ns () =
        List.iter ns ~f:(fun n ->
          p ~byte_order ~buf ~pos:0 n;
          let n' = u ~byte_order ~buf ~pos:0 in
          if n <> n' then
            failwith (sprintf "%s = unpack_%s (pack_%s %s)"
                        (to_string n') name name (to_string n)))
      in
      test "signed_64" Int64.to_string
        B.pack_signed_64 B.unpack_signed_64
        [-0x8000_0000_0000_0000L;
         -0x789A_BCDE_F012_3456L;
         -0xFFL;
         Int64.minus_one;
         Int64.zero;
         Int64.one;
         0x789A_BCDE_F012_3456L;
         0x7FFF_FFFF_FFFF_FFFFL]
    );
    "[pack|unpack]_float" >::
    (fun () ->
       let test_float i =
         let buf = Bytes.create 100 in
         B.pack_float ~byte_order ~buf ~pos:0 i;
         let i' = B.unpack_float ~byte_order ~buf ~pos:0 in
         i' = i in
       Quickcheck_deprecated.laws_exn "unpack_float (pack_float x) = x"
         100 Quickcheck_deprecated.fg test_float
    );
  ]

let test =
  "binary_packing" >::: (test `Big_endian @ test `Little_endian)
