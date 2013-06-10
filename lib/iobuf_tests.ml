open Std_internal

module Unix = Core_unix

let is_error = Result.is_error
let is_ok    = Result.is_ok

let ok_exn   = Or_error.ok_exn
let try_with = Or_error.try_with

let log string a sexp_of_a =
  Printf.eprintf "%s\n%!"
    (Sexp.to_string_hum (<:sexp_of< string * a  >> (string, a)));
;;

module type Iobuf = module type of Iobuf

module Test (Iobuf : sig
               include Iobuf
               val show_messages : bool ref
             end) : sig end = (struct
  let show_messages = Iobuf.show_messages
  let () = show_messages := false
  open Iobuf

  type nonrec ('d, 'w) t = ('d, 'w) t with sexp_of
  type nonrec no_seek = no_seek with sexp_of
  type nonrec seek = seek with sexp_of

  let strings = [ ""; "a"; "hello"; "\000"; "\000\000\000"; "\000hello" ]

  let iter_examples ~f =
    List.iter strings ~f:(fun string ->
      let len = String.length string in
      for capacity = max 1 len to len + 2 do
        for pos = 0 to 2 do
          if pos + len <= capacity then
            f (create ~len:capacity) string ~pos
        done
      done)
  ;;

  let iter_slices n ~f =
    let choices =
      [ None; Some (-1); Some 0; Some 1; Some (n / 3); Some (n - 1); Some n; Some (n + 1) ]
    in
    List.iter choices ~f:(fun pos ->
      List.iter choices ~f:(fun len ->
        let pos' = Option.value pos ~default:0 in
        let len' = Option.value len ~default:(n - pos') in
        let is_valid =
          pos' >= 0
          && pos' <= n
          && len' >= 0
          && len' <= n - pos'
        in
        if !show_messages then
          log "iter_slice" (`pos pos, `len len, `pos' pos', `len' len', `is_valid is_valid)
            (<:sexp_of< ([ `pos of int option ] * [ `len of int option ] * [ `pos' of int ]
                         * [ `len' of int ] * [ `is_valid of bool ]) >>);
        f ?pos ?len ~pos' ~len' ~is_valid ()))
  ;;

  let invariant = invariant

  TEST_UNIT =
    invariant ignore ignore (create ~len:(Random.int 100_000 + 1));
  ;;

  let create = create
  let capacity = capacity
  let length = length
  let is_empty = is_empty

  (* [create] with random capacity. *)
  TEST_MODULE = struct
    let n = Random.int 100_000 + 1 in
    assert (n > 0);
    let t = create ~len:n in
    assert (capacity t = n);
    assert (length t = n);
    assert (not (is_empty t));
  end

  (* [create] with user-supplied capacity. *)
  TEST_UNIT =
    let t = create ~len:1 in
    assert (capacity t = 1);
    assert (length t = 1);
    assert (not (is_empty t));
  ;;

  TEST = is_empty (create ~len:0)

  (* [create] with invalid capacity. *)
  TEST_UNIT =
    assert (is_error (try_with (fun () ->
      ignore (create ~len:(-1) : (_, _) t))))
  ;;

  module type Accessors = sig include module type of Unsafe val is_safe : bool end
  module Accessors (Accessors : Accessors) = struct
    open Accessors

    (* [window] *)
    TEST_UNIT =
      let t = create ~len:10 in
      let n = length t in
      assert (n = 10);
      Poke.char t ~pos:0 'a';
      Poke.char t ~pos:1 'b';
      sub t |> (fun t ->
        assert (length t = n);
        assert (Consume.char t = 'a'));
      assert (Consume.char t = 'a');
      assert (Consume.char t = 'b');
    ;;

    let in_each_window t ~f =
      let n = length t in
      for pos = 0 to n do
        for len = 0 to n - pos do
          sub t ~pos ~len |> f;
        done;
      done;
    ;;

    (* [sub ?pos ?len]  *)
    TEST_UNIT =
      let test t =
        let n = length t in
        let window_length ?pos ?len () =
          sub t ?pos ?len |> length
        in
        let window_fails ?pos ?len () =
          is_error (try_with (fun () -> ignore (sub t ?pos ?len |> ignore)))
        in
        assert (window_fails ~pos:(-1) ());
        assert (window_fails ~pos:(n + 1) ());
        assert (window_fails ~len:(-1) ());
        assert (window_fails ~len:(n + 1) ());
        assert (window_length () = n);
        for len = 0 to n do
          assert (window_length ~len () = len);
        done;
        for pos = 0 to n do
          assert (window_length ~pos () = n - pos);
          for len = 0 to n - pos do
            assert (window_length ~pos ~len () = len);
          done;
        done;
      in
      for len = 1 to 5 do
        let t = create ~len in
        test t;
        in_each_window t ~f:test;
      done;
    ;;

    module Snapshot = struct
      open Snapshot

      type nonrec t = t

      let sexp_of_t = sexp_of_t

      let restore = restore

      TEST_UNIT =
        let iobuf = create ~len:2 in
        let snapshot = snapshot iobuf in
        assert (length iobuf = 2);
        advance iobuf 1;
        assert (length iobuf = 1);
        restore snapshot iobuf;
        assert (length iobuf = 2);
        (* the same snapshot can be reused *)
        advance iobuf 1;
        assert (length iobuf = 1);
        restore snapshot iobuf;
        assert (length iobuf = 2);
        (* but can fail in combination with narrow *)
        advance iobuf 1;
        narrow iobuf;
        assert (capacity iobuf = 1);
        assert (length iobuf = 1);
        assert (is_error (try_with (fun () -> restore snapshot iobuf)));
      ;;
    end

    let snapshot = snapshot
    let rewind = rewind
    let reset = reset
    let flip = flip
    let compact = compact
    let sub = sub
    let narrow = narrow
    TEST =
      let buf = of_string "123" in
      assert (capacity buf = 3);
      assert (length buf = 3);
      advance buf 1;
      assert (capacity buf = 3);
      assert (length buf = 2);
      resize buf ~len:1;
      assert (capacity buf = 3);
      assert (length buf = 1);
      narrow buf;
      assert (capacity buf = 1);
      assert (length buf = 1);
      advance buf 1;
      assert (capacity buf = 1);
      assert (length buf = 0);
      reset buf;
      assert (capacity buf = 1);
      length buf = 1
    ;;
    let resize = resize

    TEST_UNIT =
      let test t =
        let n = length t in
        rewind t;
        assert (length t = n);
        advance t 1;
        assert (length t = n - 1);
        rewind t;
        assert (length t = n);
        advance t 1;
        assert (length t = n - 1);
        rewind t;
        assert (length t = n);
        rewind t;
      in
      test (create ~len:10);
      let t = create ~len:10 in
      sub t ~pos:1 |> test;
    ;;

    let to_string = to_string

    let of_string = of_string

    TEST_UNIT =
      List.iter strings ~f:(fun string ->
        assert (String.equal string (to_string (of_string string))))
    ;;

    let of_bigstring = of_bigstring

    TEST_UNIT =
      List.iter strings ~f:(fun string ->
        let bigstring = Bigstring.of_string string in
        iter_slices (String.length string) ~f:(fun ?pos ?len ~pos':_ ~len':_ ~is_valid () ->
          match try_with (fun () -> of_bigstring bigstring ?pos ?len) with
          | Error _ -> assert (not is_valid)
          | Ok t ->
            assert is_valid;
            assert (String.equal (to_string t) (Bigstring.to_string bigstring ?pos ?len))));
    ;;

    let advance = advance

    TEST_UNIT =
      for len = 1 to 5 do
        let t = create ~len in
        let advance_fails amount = is_error (try_with (fun () -> advance t amount)) in
        assert (advance_fails (-1));
        assert (advance_fails (len + 1));
        for amount = 0 to len do
          sub t |> (fun t ->
            advance t amount;
            assert (length t = len - amount))
        done;
      done;
    ;;

    let consume_bin_prot = consume_bin_prot
    let fill_bin_prot = fill_bin_prot

    TEST_UNIT =
      let t = create ~len:20 in
      List.iter strings
        ~f:(fun s ->
          assert (is_ok (fill_bin_prot t String.bin_writer_t s));
          flip t;
          let s' = ok_exn (consume_bin_prot t String.bin_reader_t) in
          reset t;
          assert (String.equal s s'));
    ;;

    module Intf (Intf : sig
                   include Iobuf_intf.Accessors
                   val t_pos_1
                     :  (read_write, seek) Iobuf.t
                     -> int
                     -> ('a, read_write, seek) t
                     -> 'a
                     -> string          (* corresponding buffer contents *)
                     -> ('a -> Sexp.t)
                     -> unit
                 end) = struct
      open Intf

      type nonrec ('a, 'd, 'w) t = ('a, 'd, 'w) t

      let char        = char
      let  int8       =  int8
      let uint8       = uint8
      let  int16_be   =  int16_be
      let  int16_le   =  int16_le
      let uint16_be   = uint16_be
      let uint16_le   = uint16_le
      let  int32_be   =  int32_be
      let  int32_le   =  int32_le
      let uint32_be   = uint32_be
      let uint32_le   = uint32_le
      let  int64_be   =  int64_be
      let  int64_le   =  int64_le
      let  int64_t_be =  int64_t_be
      let  int64_t_le =  int64_t_le

      let padded_fixed_string = padded_fixed_string
      let              string =              string
      let           bigstring =           bigstring

      TEST_UNIT =
        let buf = of_string "ABCDEFGHIJ" in
        t_pos_1 buf 1 int8 127 "A\127CDEFGHIJ" sexp_of_int;
        t_pos_1 buf 1 int8 (-1) "A\255CDEFGHIJ" sexp_of_int;
        t_pos_1 buf 1 int8 (-128) "A\128CDEFGHIJ" sexp_of_int;
        t_pos_1 buf 1 uint8 0 "A\000CDEFGHIJ" sexp_of_int;
        t_pos_1 buf 1 uint8 255 "A\255CDEFGHIJ" sexp_of_int;
        t_pos_1 buf 1 char 'K' "AKCDEFGHIJ" sexp_of_char;
        t_pos_1 buf 1 char '\254' "A\254CDEFGHIJ" sexp_of_char;
        t_pos_1 buf 1 int8 (-1) "A\255CDEFGHIJ" sexp_of_int;
        t_pos_1 buf 1 uint8 12 "A\012CDEFGHIJ" sexp_of_int;
        t_pos_1 buf 2 int16_be 0x0102 "A\001\002DEFGHIJ" sexp_of_int;
        t_pos_1 buf 2 int16_le 0x0304 "A\004\003DEFGHIJ" sexp_of_int;
        t_pos_1 buf 2 int16_be (-2) "A\255\254DEFGHIJ" sexp_of_int;
        t_pos_1 buf 2 int16_le (-3) "A\253\255DEFGHIJ" sexp_of_int;
        t_pos_1 buf 2 uint16_be 0xFFFE "A\255\254DEFGHIJ" sexp_of_int;
        t_pos_1 buf 2 uint16_le 0xFDFC "A\252\253DEFGHIJ" sexp_of_int;
        t_pos_1 buf 2 int16_be 0x0506 "A\005\006DEFGHIJ" sexp_of_int;
        t_pos_1 buf 2 int16_le 0x0708 "A\008\007DEFGHIJ" sexp_of_int;
        t_pos_1 buf 3 (padded_fixed_string ~padding:'p' ~len:3) "x"
          "AxppEFGHIJ" sexp_of_string;
        t_pos_1 buf 3 (string ?str_pos:None ~len:3) "123" "A123EFGHIJ" sexp_of_string;
        t_pos_1 buf 3 (bigstring ?str_pos:None ~len:3) (Bigstring.of_string "klm")
          "AklmEFGHIJ" sexp_of_bigstring;
        t_pos_1 buf 4 int32_be 0x0A0B0C0D "A\010\011\012\013FGHIJ" sexp_of_int;
        t_pos_1 buf 4 int32_le 0x01020304 "A\004\003\002\001FGHIJ" sexp_of_int;
        t_pos_1 buf 4 int32_be (-0x01020305) "A\254\253\252\251FGHIJ" sexp_of_int;
        t_pos_1 buf 4 int32_le (-0x05060709) "A\247\248\249\250FGHIJ" sexp_of_int;
IFDEF ARCH_SIXTYFOUR THEN
        t_pos_1 buf 4 uint32_be 0xF6F5F4F3 "A\246\245\244\243FGHIJ" sexp_of_int;
        t_pos_1 buf 4 uint32_le 0xFBFAF9F8 "A\248\249\250\251FGHIJ" sexp_of_int;
        t_pos_1 buf 8 int64_be 0x0102030405060708 "A\001\002\003\004\005\006\007\008J"
          sexp_of_int;
        t_pos_1 buf 8 int64_le 0x090a0b0c0d0e0f10 "A\016\015\014\013\012\011\010\009J"
          sexp_of_int;
        t_pos_1 buf 8 int64_be (-0x0102030405060709)
          "A\254\253\252\251\250\249\248\247J" sexp_of_int;
        t_pos_1 buf 8 int64_le (-0x0102030405060709)
          "A\247\248\249\250\251\252\253\254J" sexp_of_int;
ENDIF;
        t_pos_1 buf 8 int64_t_be 1L "A\000\000\000\000\000\000\000\001J" sexp_of_int64;
        t_pos_1 buf 8 int64_t_le 1L "A\001\000\000\000\000\000\000\000J" sexp_of_int64;
        t_pos_1 buf 8 int64_t_be 0x8000000000000000L
          "A\128\000\000\000\000\000\000\000J" sexp_of_int64;
        t_pos_1 buf 8 int64_t_le 0x8000000000000000L
          "A\000\000\000\000\000\000\000\128J" sexp_of_int64
    end

    module Poke = Intf (struct
      include Poke

      let t_pos_1 buf _ f arg str sexp_of_arg =
        f buf ~pos:1 arg;
        if not (String.equal str (to_string buf)) then
          (failwiths (sprintf "%S <> %S (is_safe=%b)" (to_string buf) str is_safe)
             arg
             sexp_of_arg;
           failwith "Does this trick the compiler into not tail-calling failwiths?")

      (* Static permission tests for the cases that do compile.  Since the functions all
         use essentially the same type definitions, we don't need to test all of them.
         We've already tested them on a (read_write, seek) Iobuf.t above. *)
      TEST_UNIT = char (of_string "a" : (_, no_seek) Iobuf.t) ~pos:0 'b'
      TEST_UNIT = char (of_string "a" : (_, seek) Iobuf.t) ~pos:0 'b'
      TEST_UNIT = char (of_string "a" : (read_write, _) Iobuf.t) ~pos:0 'b'
    end)

    TEST_UNIT =
      let t = create ~len:10 in
      let n = length t in
      assert (n = 10);
      Poke.char t ~pos:0 'a';
      Poke.char t ~pos:1 'b';
      sub t |> (fun t ->
        assert (length t = n);
        assert (Consume.char t = 'a'));
      assert (Consume.char t = 'a');
      assert (Consume.char t = 'b');
      let ws = (t  :> (read_write, seek) t) in
      let rs = (ws :> (read_only, seek) t) in
      ignore   (rs :> (read_only, no_seek) t);
      ignore   (ws :> (read_write, no_seek) t);
    ;;

    module Peek = Intf (struct
      include Peek

      let t_pos_1 _ _ f expected str sexp_of_res =
        let res = f (of_string str) ~pos:1 in
        if not (Pervasives.(=) res expected) then
          failwiths (sprintf "%S" str)
            (res, expected)
            (Tuple.T2.sexp_of_t sexp_of_res sexp_of_res)

      (* static permission tests; see above *)
      TEST = Char.(=) 'a' (char (of_string "a" : (_, no_seek) Iobuf.t) ~pos:0)
      TEST = Char.(=) 'a' (char (of_string "a" : (_, seek) Iobuf.t) ~pos:0)
      TEST = Char.(=) 'a' (char (of_string "a" : (read_only, _) Iobuf.t) ~pos:0)
      TEST = Char.(=) 'a' (char (of_string "a" : (read_write, _) Iobuf.t) ~pos:0)
    end)

    TEST_UNIT =
      let s = "hello" in
      let t = of_string s in
      for i = 0 to String.length s - 1 do
        assert (Char.equal (Peek.char t ~pos:i) (s.[i]));
        Poke.char t ~pos:i 'z';
        assert (Char.equal (Peek.char t ~pos:i) 'z');
      done;
      if is_safe then
        (assert (is_error (try_with (fun () -> Peek.char t ~pos:(-1))));
         assert (is_error (try_with (fun () -> Poke.char t ~pos:(-1) 'z')));
         assert (is_error (try_with (fun () -> Peek.char t ~pos:(String.length s))));
         assert (is_error (try_with (fun () ->
           Poke.char t ~pos:(String.length s) 'z'))));
    ;;

    TEST_UNIT =
      List.iter [ 0; 1 ] ~f:(fun pos ->
        let t = create ~len:10 in
        let i = 0x1234_5678_90AB_CDEF in
        Poke.int64_le t ~pos i;
        assert (Peek.int64_le t ~pos = i);
        Poke.int64_be t ~pos i;
        assert (Peek.int64_be t ~pos = i);
        let i = 0x1234_5678 in
        Poke.int32_le t ~pos i;
        assert (Peek.int32_le t ~pos = i);
        Poke.int32_be t ~pos i;
        assert (Peek.int32_be t ~pos = i);
        let i = 0x1234 in
        Poke.int32_le t ~pos i;
        assert (Peek.int32_le t ~pos = i);
        Poke.int32_be t ~pos i;
        assert (Peek.int32_be t ~pos = i);
      )
    ;;

    module Fill = Intf (struct
      include Fill

      let t_pos_1 buf n f arg str sexp_of_arg =
        rewind buf;
        advance buf 1;
        f buf arg;
        assert (Iobuf.length buf = String.length str - 1 - n);
        rewind buf;
        if not (String.equal str (to_string buf)) then
          failwiths (sprintf "%S <> %S" (to_string buf) str)
            arg
            sexp_of_arg

      (* static permission tests; see above *)
      TEST_UNIT = char (of_string "a" : (_, seek) Iobuf.t) 'b'
      TEST_UNIT = char (of_string "a" : (read_write, _) Iobuf.t) 'b'
    end)

    TEST_UNIT =
      List.iter [ ""; "a"; "ab" ] ~f:(fun s ->
        let len = String.length s in
        for capacity = max 1 len to len + 2 do
          let t = create ~len:capacity in
          for pos = 0 to capacity - len do
            sub t ~pos |> (fun t -> Fill.string t s);
            sub t ~pos ~len |> (fun t ->
              let s' = to_string t in
              assert (String.equal s s');
              assert (length t = len); (* [to_string] didn't change the length *)
            );
          done;
        done)
    ;;

    TEST =
      let src = create ~len:5 in
      let len = 3 in
      let str = "Hi." in
      assert (len >= String.length str);
      assert (capacity src >= len);
      try transfer ~src ~dst:src ~len; false
      with _ ->
        let dst = create ~len:5 in
        Fill.string src str;
        flip src;
        transfer ~src ~dst ~len;
        flip dst;
        String.equal str (Consume.string dst)
    ;;

    module Consume = Intf (struct
      include Consume

      let t_pos_1 _ n f expected str sexp_of_res =
        let buf = of_string str in
        advance buf 1;
        let res = f buf in
        assert (Iobuf.length buf = String.length str - 1 - n);
        rewind buf;
        if not (Pervasives.(=) res expected) then
          failwiths (sprintf "%S" (to_string buf))
            (res, expected)
            (Tuple.T2.sexp_of_t sexp_of_res sexp_of_res)

      (* static permission tests; see above *)
      TEST = Char.(=) 'a' (char (of_string "a" : (_, seek) Iobuf.t))
      TEST = Char.(=) 'a' (char (of_string "a" : (read_only, _) Iobuf.t))
      TEST = Char.(=) 'a' (char (of_string "a" : (read_write, _) Iobuf.t))
    end)

    TEST_UNIT =
      let t = create ~len:1 in
      let c = 'a' in
      sub t |> (fun t ->
        Fill.char t c;
        assert (is_empty t));
      assert (Char.equal (Consume.char t) c);
      assert (is_empty t);
    ;;

    TEST_UNIT =
      List.iter [ 0; 1 ] ~f:(fun pos ->
        let t = create ~len:10 in
        let i = 0x1234_5678_90AB_CDEF in
        sub t ~pos |> (fun t -> Fill.int64_le t i);
        assert (i = (sub t ~pos |> Consume.int64_le));
        let i = 0x1234_5678 in
        sub t ~pos |> (fun t -> Fill.int32_le t i);
        assert (i = (sub t ~pos |> Consume.int32_le));
        let i = 0x1234 in
        sub t ~pos |> (fun t -> Fill.int16_le t i);
        assert (i = (sub t ~pos |> Consume.int16_le));
        ()
      )
    ;;

    TEST_UNIT =
      List.iter strings ~f:(fun s ->
        let t = of_string s in
        let s' = Consume.string t in
        assert (is_empty t);
        assert (String.equal s s'));
    ;;

    let consume_into_string = consume_into_string
    let test_consume_into_str consume_into create sub_string of_string to_string =
      iter_examples ~f:(fun t string ~pos ->
        let n = String.length string in
        iter_slices n ~f:(fun ?pos:str_pos ?len ~pos' ~len' ~is_valid () ->
          let fill_result =
            Or_error.try_with (fun () ->
              sub t ~pos |> (fun t -> Fill.string t string ?str_pos ?len))
          in
          assert (Bool.equal (is_ok fill_result) is_valid);
          let str = create n in
          let consume_result =
            Or_error.try_with (fun () ->
              sub t ~pos |> (fun t ->
                consume_into ?pos:str_pos ?len t str))
          in
          begin match consume_result, is_valid with
          | Error _, false -> ()
          | Ok (), true ->
            assert (String.equal
                      (String.sub string ~pos:pos' ~len:len')
                      (sub_string str ~pos:pos' ~len:len'));
          | _, _ -> assert false
          end));
      let t = Iobuf.of_string "012345678" in
      let dst = of_string "abcdefhij" in
      consume_into ?pos:(Some 3) ?len:(Some 3) t dst;
      assert (String.equal (Iobuf.to_string t) "345678");
      String.equal (to_string dst) "abc012hij"
    ;;
    TEST = test_consume_into_str consume_into_string String.create String.sub ident ident

    let consume_into_bigstring = consume_into_bigstring
    TEST =
      test_consume_into_str consume_into_bigstring Bigstring.create
        (fun s ~pos ~len -> Bigstring.to_string s ~pos ~len)
        Bigstring.of_string
        Bigstring.to_string
    ;;

    (* [Fill.string] ranges *)
    TEST_UNIT =
      let s = "hello" in
      let len = String.length s in
      let t = create ~len:len in
      let try_write ?str_pos ?len () : _ Or_error.t =
        try_with (fun () -> sub t |> (fun t -> Fill.string t s ?str_pos ?len))
      in
      if is_safe then
        (assert (is_error (try_write ~str_pos:(-1) ()));
         assert (is_error (try_write ~str_pos:(len + 1) ()));
         assert (is_error (try_write ~str_pos:0 ~len:(len + 1) ())));
    ;;

    let transfer = transfer

    TEST_UNIT =
      let t1 = create ~len:100 in
      let t2 = create ~len:100 in
      let s = "585038473775210758" in
      Fill.string t1 s;
      flip t1;
      assert (String.equal s (Consume.string t1));
      assert (is_empty t1);
      reset t1;
      Fill.string t1 s;
      flip t1;
      transfer ~src:t1 ~dst:t2 ~len:(length t1);
      flip t2;
      assert (String.equal s (Consume.string t2));
      assert (is_empty t2);
    ;;

    let memmove = memmove

    TEST_UNIT =
      let s = "01234567899876543210" in
      let t = create ~len:(String.length s) in
      sub t |> (fun t -> Fill.string t s);
      memmove t ~src_pos:0 ~dst_pos:10 ~len:10;
      let s2 = to_string t in
      assert (String.equal s2 "01234567890123456789")
    ;;

    let read_assume_fd_is_nonblocking = read_assume_fd_is_nonblocking
    let write_assume_fd_is_nonblocking = write_assume_fd_is_nonblocking

    TEST_UNIT =
      iter_examples ~f:(fun t string ~pos ->
        let n = String.length string in
        let file, fd = Unix.mkstemp "iobuf_test" in
        protect ~finally:(fun () -> Unix.unlink file)
          ~f:(fun () ->
            sub t ~pos |> (fun t -> Fill.string t string);
            sub t ~pos ~len:n |> (fun t ->
              assert (write_assume_fd_is_nonblocking t fd = n));
            Unix.close fd;
            iter_examples ~f:(fun t _ ~pos ->
              if length t - pos >= String.length string then
                let fd = Unix.openfile ~mode:[Unix.O_RDONLY] file in
                sub t ~pos |> (fun t ->
                  assert (read_assume_fd_is_nonblocking t fd = n));
                sub t ~pos ~len:n |> (fun t ->
                  assert (String.equal (to_string t) string));
                Unix.close fd)))
    ;;

    let pread_assume_fd_is_nonblocking = pread_assume_fd_is_nonblocking
    let pwrite_assume_fd_is_nonblocking = pwrite_assume_fd_is_nonblocking

    TEST_UNIT =
      let s = "000000000011111111112222222222" in
      let n = String.length s in
      let t = of_string s in
      let file, fd = Unix.mkstemp "iobuf_test" in
      protect ~finally:(fun () -> Unix.unlink file)
        ~f:(fun () ->
          sub t ~pos:0 ~len:n |> (fun t ->
            assert (pwrite_assume_fd_is_nonblocking t fd ~offset:10 = n));
          sub t ~pos:0 ~len:10 |> (fun t ->
            assert (pread_assume_fd_is_nonblocking t fd ~offset:20 = 10));
          sub t ~pos:0 ~len:10 |> (fun t ->
            assert (String.equal (to_string t) "1111111111"));
          Unix.close fd;)
    ;;

    let recvfrom_assume_fd_is_nonblocking = recvfrom_assume_fd_is_nonblocking

    let recvmmsg_assume_fd_is_nonblocking = recvmmsg_assume_fd_is_nonblocking
    TEST_UNIT "recvmmsg smoke" =
      match recvmmsg_assume_fd_is_nonblocking with
      | Error _ -> ()
      | Ok recvmmsg ->
        let open Unix in
        let count = 10 in
        let fd = socket ~domain:PF_INET ~kind:SOCK_DGRAM ~protocol:0 in
        bind fd ~addr:(ADDR_INET (Inet_addr.bind_any, 0));
        let iobufs = Array.init count ~f:(fun _ -> create ~len:1500) in
        let srcs = Array.create ~len:count (ADDR_INET (Inet_addr.bind_any, 0)) in
        let short_srcs = Array.create ~len:(count - 1)
                           (ADDR_INET (Inet_addr.bind_any, 0)) in
        set_nonblock fd;
        (* EWOULDBLOCK/EAGAIN is a possible and expected, if atypical, result in these
           smoke tests, where we listen to a random socket.  Here, we also don't try to
           test recvmmsg's behavior, just our wrapper; Unix_error indicates that recvmmsg
           returned -1, whereas other exceptions indicate that our wrapper detected
           something wrong.  We treat the two situations separately. *)
        assert (try recvmmsg fd iobufs ~count ~srcs = 0
                with Unix_error _ -> true);
        assert (try recvmmsg fd iobufs = 0
                with Unix_error _ -> true);
        assert (try recvmmsg fd iobufs ~count:(count / 2) ~srcs = 0
                with Unix_error _ -> true);
        assert (try recvmmsg fd iobufs ~count:0 ~srcs = 0
                with Unix_error _ -> true);
        assert (try ignore (recvmmsg fd iobufs ~count:(count + 1)); false
                with Unix_error _ as e -> raise e | _ -> true);
        assert (try ignore (recvmmsg fd iobufs ~srcs:short_srcs); false
                with Unix_error _ as e -> raise e | _ -> true);
    ;;

    let send_nonblocking_no_sigpipe = send_nonblocking_no_sigpipe
    let sendto_nonblocking_no_sigpipe = sendto_nonblocking_no_sigpipe

    let sends_with_recvfrom recvfrom_assume_fd_is_nonblocking =
      match send_nonblocking_no_sigpipe (), sendto_nonblocking_no_sigpipe () with
      | Error _, _ | _, Error _ -> ()
      | Ok send_nonblocking_no_sigpipe, Ok sendto_nonblocking_no_sigpipe ->
        let socket () = Unix.(socket ~domain:PF_INET ~kind:SOCK_DGRAM ~protocol:0) in
        let fd = socket () in
        Unix.(setsockopt fd SO_REUSEADDR true);
        Unix.(bind fd ~addr:(ADDR_INET (Inet_addr.bind_any, 0)));
        Unix.set_nonblock fd;
        let port =
          match Unix.getsockname fd with
          | Unix.ADDR_INET (_, port) -> port
          | _ -> assert false
        in
        let _ : Thread.t =
          Thread.create (fun () ->
            let fd = socket () in
            let doit f =
              iter_examples ~f:(fun t string ~pos ->
                sub t ~pos |> (fun t -> Fill.string t string);
                let len = String.length string in
                match sub t ~pos ~len |> (fun t -> f t fd) with
                | None -> assert false
                | Some n -> assert (n = len))
            in
            let addr = Unix.(ADDR_INET (Inet_addr.localhost, port)) in
            doit (fun t fd -> sendto_nonblocking_no_sigpipe t fd addr);
            Unix.connect fd ~addr;
            doit send_nonblocking_no_sigpipe;
          ) ();
        in
        for _i = 0 to 1; do (* for send_* and sendto_* *)
          iter_examples ~f:(fun t string ~pos ->
            let n, _sockaddr =
              sub t ~pos |> (fun t ->
                let rec loop () =
                  match
                    Result.try_with (fun () -> recvfrom_assume_fd_is_nonblocking t fd)
                  with
                  | Ok x -> x
                  | Error (Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.EAGAIN), _, _)) ->
                    Time.pause (Span.of_ms 1.);
                    loop ()
                  | Error exn -> raise exn
                in
                loop ())
            in
            let len = String.length string in
            assert (n = len);
            assert (String.equal string (sub t ~pos ~len |> to_string));
          );
        done
    ;;
    TEST_UNIT = sends_with_recvfrom recvfrom_assume_fd_is_nonblocking
    TEST_UNIT = Result.iter recvmmsg_assume_fd_is_nonblocking ~f:(fun recvmmsg ->
      sends_with_recvfrom (fun t fd ->
        let t_len_before = length t in
        let addr_before = Unix.(ADDR_INET (Inet_addr.bind_any, 0)) in
        let srcs = Array.create ~len:1 addr_before in
        assert (recvmmsg fd ~srcs (Array.create ~len:1 t) = 1);
        (* Check that the prototype source address wasn't modified in place.  It is
           expected to have been replaced in the array by a new sockaddr. *)
        assert (not (phys_equal addr_before srcs.(0)));
        assert (Pervasives.(<>) addr_before srcs.(0));
        t_len_before - length t, srcs.(0)))

    TEST_UNIT = List.iter [ 0; 1 ] ~f:(fun pos ->
      let t = create ~len:10 in
      let i = 0xF234_5678_90AB_CDEFL in
      Poke.int64_t_le t ~pos i;
      assert (Peek.int64_t_le t ~pos = i);
      Poke.int64_t_be t ~pos i;
      assert (Peek.int64_t_be t ~pos = i);
      let i = 0x1234_5678_90AB_CDEF in
      Poke.int64_t_le t ~pos (Int64.of_int i);
      assert (Peek.int64_t_le t ~pos = Int64.of_int i);
      Poke.int64_t_be t ~pos (Int64.of_int i);
      assert (Peek.int64_t_be t ~pos = Int64.of_int i);
      Poke.int64_le t ~pos i;
      assert (Peek.int64_le t ~pos = i);
      Poke.int64_be t ~pos i;
      assert (Peek.int64_be t ~pos = i);
      let i = 0x1234_5678 in
      Poke.int32_le t ~pos i;
      assert (Peek.int32_le t ~pos = i);
      Poke.int32_be t ~pos i;
      assert (Peek.int32_be t ~pos = i);
      Poke.uint32_le t ~pos i;
      assert (Peek.uint32_le t ~pos = i);
      Poke.uint32_be t ~pos i;
      assert (Peek.uint32_be t ~pos = i);
      let i = 0x1234 in
      Poke.int16_le t ~pos i;
      assert (Peek.int16_le t ~pos = i);
      Poke.int16_be t ~pos i;
      assert (Peek.int16_be t ~pos = i);
      Poke.uint16_le t ~pos i;
      assert (Peek.uint16_le t ~pos = i);
      Poke.uint16_be t ~pos i;
      assert (Peek.uint16_be t ~pos = i);
      let i = 0x12 in
      Poke.int8 t ~pos i;
      assert (Peek.int8 t ~pos = i);
      Poke.uint8 t ~pos i;
      assert (Peek.uint8 t ~pos = i))

    TEST_UNIT =
      let t = create ~len:1024 in
      Fill.int8 t 12;
      Fill.int16_le t 1234;
      Fill.int32_le t 345678;
      Fill.char t 'x';
      flip t;
      assert (length t = 8);
      assert (Consume.int8 t = 12);
      assert (Consume.int16_le t = 1234);
      assert (Consume.int32_le t = 345678);
      assert (Consume.char t = 'x');
    ;;

    (* Create a file of binary length prefixed messages of a known format to read back using
       the Iobuf module. Part unit test, part usage example and interface exercise... *)
    let create_sample_file ?(int_size = 2) ?(be = false) ~msgcount =
      let bstr = Bigstring.create 512 in
      (* Sometimes use a bigstring buffer, sometimes a char queue, to test the latter and
         [read_to_fd] symmetrically to [write_from_fd] below. *)
      let t = create ~len:512 in
      let filename, fd = Unix.mkstemp "iobuftest" in
      for i = 0 to (msgcount - 1) do
        let s = sprintf "MESSAGE %d" i in
        let len = String.length s in
        if Random.bool () then
          let open Bigstring in
          let set_int =
            match int_size with
            | 2 -> if be then unsafe_set_int16_be else unsafe_set_int16_le
            | 4 -> if be then unsafe_set_int32_be else unsafe_set_int32_le
            | 8 -> if be then unsafe_set_int64_be else unsafe_set_int64_le
            | _ -> failwithf "Unknown int size %d" int_size ()
          in
          set_int bstr ~pos:0 len;
          blit_string_bigstring ~src:s ~dst:bstr ~dst_pos:int_size ();
          really_write fd ~len:(len + int_size) bstr
        else
          let fill_int =
            match int_size with
            | 2 -> if be then Fill.int16_be else Fill.int16_le
            | 4 -> if be then Fill.int32_be else Fill.int32_le
            | 8 -> if be then Fill.int64_be else Fill.int64_le
            | _ -> failwithf "Unknown int size %d" int_size ()
          in
          fill_int t len;
          Fill.string t s;
          flip t;
          let n = length t in
          let m = write_assume_fd_is_nonblocking t fd in
          assert (m = n);                   (* no short writes *)
          assert (is_empty t);
          reset t
      done;
      Unix.close fd;
      filename
    ;;

    (** Loop through and check all messages in the given file match the expected
        "MESSAGE %d" format *)
    let check_msgs ?(int_size = 2) ?(be = false) file =
      let msg_number = ref 0 in
      let check_message r =
        let msg = Consume.string r in
        let s = sprintf "MESSAGE %d" !msg_number in
        assert( String.equal s msg );
        msg_number := !msg_number + 1
      in
      let fd = Unix.openfile file ~perm:0o600 ~mode:[Unix.O_RDONLY] in
      let t = create ~len:512 in
      let rec drain_messages () =
        let init_len = length t in
        if init_len > int_size then
          let consume_int =
            match int_size with
            | 2 -> if be then Consume.int16_be else Consume.int16_le
            | 4 -> if be then Consume.int32_be else Consume.int32_le
            | 8 -> if be then Consume.int64_be else Consume.int64_le
            | _ -> failwithf "Unknown int size %d" int_size ()
          in
          let needed = consume_int t in
          if length t < needed then begin
            rewind t;
            advance t (length t - init_len);
            assert (length t = init_len);
          end else begin
            check_message (sub t ~len:needed);
            advance t needed;
            drain_messages ();
          end
      in
      let rec loop_file () =
        if read_assume_fd_is_nonblocking t fd > 0 then begin
          flip t;
          drain_messages ();
          compact t;
          loop_file ();
        end
      in
      loop_file ();
      Unix.close fd;
      !msg_number
    ;;

    TEST_UNIT =
      let msgcount = 10_000 in
      List.iter [2; 4; 8] ~f:(fun int_size -> List.iter [false; true] ~f:(fun be ->
        let filename = create_sample_file ~int_size ~be ~msgcount in
        protect ~f:(fun () -> assert (check_msgs  ~int_size ~be filename = msgcount))
          ~finally:(fun () -> Unix.unlink               filename)))
    ;;
  end

  include Accessors (struct
    module Consume = Consume
    module Fill = Fill
    module Peek = Peek
    module Poke = Poke
    let is_safe = true
  end)

  module Unsafe = Accessors (struct include Unsafe let is_safe = false end)
end :
  (* The signature here is to remind us to add a unit test whenever we add a function to
     [Iobuf]. *)
  Iobuf)

include Test (Iobuf_debug.Make (struct end))

(* Ensure against bugs in [Iobuf_debug].  The above tests [Iobuf_debug], with invariants
   checked, and this tests the straight [Iobuf] module. *)
include Test (struct
  include Iobuf
  let show_messages = ref true
end)
