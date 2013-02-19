(** While I would have preferred to put these functions with Core_int, there is a
    dependency between Core_hashtbl, where it is used, and Core_int that prevents it. *)


(** Common bit-twiddling hacks for computing floor/ceiling power of 2, without a built in
    fast bsr (bit scan reverse). As some have observed, these would return 0 for values of
    0, and do not support negative numbers *)
let non_positive_argument () =
  Core_printf.invalid_argf "argument must be strictly positive" ()

(** "ceiling power of 2" - Least power of 2 greater than or equal to x. *)
let ceil_pow2 x =
  if (x <= 0) then non_positive_argument ();
  let x = x - 1 in
  let x = x lor (x lsr 1) in
  let x = x lor (x lsr 2) in
  let x = x lor (x lsr 4) in
  let x = x lor (x lsr 8) in
  let x = x lor (x lsr 16) in
  let x = x lor (x lsr 32) in
  x + 1

(** "floor power of 2" - Largest power of 2 less than or equal to x. *)
let floor_pow2 x =
  if (x <= 0) then non_positive_argument ();
  let x = x lor (x lsr 1) in
  let x = x lor (x lsr 2) in
  let x = x lor (x lsr 4) in
  let x = x lor (x lsr 8) in
  let x = x lor (x lsr 16) in
  let x = x lor (x lsr 32) in
  x - (x lsr 1)

TEST_MODULE "int_math" = struct
  let is_pow2 v = (v land (v-1)) = 0 ;;

  let test_cases () =
    let cases = [ 0xAA; 0xAA_AA; 0xAA_AA_AA;  0x80; 0x80_08; 0x80_00_08; ]
    in
    if Sys.word_size = 64 then (* create some >32 bit values... *)
      (* We can't use literals directly because the compiler complains on 32 bits. *)
      let cases = cases @ [ (0xAA_AA lsl 16) lor 0xAA_AA;
                            (0x80_00 lsl 16) lor 0x00_08; ] in
      let added_cases = Core_list.map cases ~f:(fun x -> x lsl 16) in
      List.concat [ cases; added_cases ]
    else cases
  ;;

  TEST_UNIT "ceil_pow2" =
    Core_list.iter (test_cases ())
      ~f:(fun x -> let p2 = ceil_pow2 x in
        assert( (is_pow2 p2) && (p2 >= x && x >= (p2 / 2)) )
      );
    ;;

  TEST_UNIT "floor_pow2" =
    Core_list.iter (test_cases ())
      ~f:(fun x -> let p2 = floor_pow2 x in
        assert( (is_pow2 p2) && ((2 * p2) >= x && x >= p2) )
      );
    ;;
end
