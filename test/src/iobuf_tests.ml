open! Core

let arch_sixtyfour = Sys.word_size = 64

let is_error = Result.is_error
let is_ok    = Result.is_ok

let ok_exn   = Or_error.ok_exn
let try_with = Or_error.try_with

let log string a sexp_of_a =
  Printf.eprintf "%s\n%!"
    (Sexp.to_string_hum ([%sexp_of: string * a] (string, a)));
;;

module type Iobuf = module type of Iobuf

module Test (Iobuf : sig
    include Iobuf
    val show_messages : bool ref
  end) : sig end = (struct
  let show_messages = Iobuf.show_messages
  let () = show_messages := false
  open Iobuf

  type nonrec ('d, 'w) t = ('d, 'w) Hexdump.t [@@deriving sexp_of]
  type nonrec    seek =    seek [@@deriving sexp_of]
  type nonrec no_seek = no_seek [@@deriving sexp_of]
  module type Bound = Bound

  (* see iobuf_view_tests.ml for expect-tests of these *)
  module Window  = Window
  module Limits  = Limits
  module Debug   = Debug
  module Hexdump = Hexdump
  let to_string_hum = to_string_hum

  let read_only = read_only
  let no_seek   = no_seek

  let strings =
    [ ""; "a"; "hello"; "\000"; "\000\000\000"; "\000hello"; String.make 1000 'x' ]

  (* [large_int] creates an int that is not representable on 32-bit systems. *)
  let large_int a b c d = (a lsl 48) lor (b lsl 32) lor (c lsl 16) lor d

  type iter_examples_state =
    { string   : string
    ; len      : int
    ; capacity : int
    ; pos      : int
    }
  [@@deriving sexp]
  let iter_examples ~f =
    List.iter strings ~f:(fun string ->
      let len = String.length string in
      for capacity = max 1 len to len + 2 do
        for pos = 0 to 2 do
          if pos + len <= capacity then
            try f (create ~len:capacity) string ~pos with
            | e ->
              failwiths "iter_examples"
                ({ string; len; capacity; pos }, e)
                [%sexp_of: iter_examples_state * exn]
        done
      done
    )
  ;;

  type iter_slices_state =
    { pos      : int option
    ; len      : int option
    ; pos'     : int
    ; len'     : int
    ; is_valid : bool
    }
  [@@deriving sexp]
  let iter_slices n ~f =
    let choices =
      [ None
      ; Some (-1)
      ; Some 0
      ; Some 1
      ; Some (n / 3)
      ; Some (n - 1)
      ; Some n
      ; Some (n + 1)
      ]
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
        if !show_messages && false then
          log "iter_slice" { pos; len; pos'; len'; is_valid }
            [%sexp_of: iter_slices_state];
        try f ?pos ?len ~pos' ~len' ~is_valid ()
        with e ->
          failwiths "iter_slices"
            ( { pos; len; pos'; len'; is_valid }
            , e
            , Backtrace.Exn.most_recent ()
            )
            [%sexp_of: iter_slices_state * exn * Backtrace.t]))
  ;;

  let invariant = invariant

  let%test_unit _ =
    invariant ignore ignore (create ~len:(Random.int 100_000 + 1))
  ;;

  let create = create
  let capacity = capacity
  let length = length
  let is_empty = is_empty

  (* [create] with random capacity. *)
  let%test_module _ =
    (module struct
      let n = Random.int 100_000 + 1 in
      assert (n > 0);
      let t = create ~len:n in
      assert (capacity t = n);
      assert (length t = n);
      assert (not (is_empty t));
    end)

  (* [create] with user-supplied capacity. *)
  let%test_unit _ =
    let t = create ~len:1 in
    assert (capacity t = 1);
    assert (length t = 1);
    assert (not (is_empty t))
  ;;

  let%test _ = is_empty (create ~len:0)

  (* [create] with invalid capacity. *)
  let%test_unit _ =
    assert (is_error (try_with (fun () ->
      ignore (create ~len:(-1) : (_, _) t))))
  ;;

  let crc32 = crc32

  let%test_module _ =
    (module struct

      let str = "The quick brown fox jumps over the lazy dog"
      let len = String.length str
      let crc = Int63.of_int64_exn 0x414fa339L

      let%test_unit _ =
        [%test_result: Int63.Hex.t]
          (crc32 (of_string str))
          ~expect:crc

      let%test_unit _ =
        let t = of_string ("12345" ^ str ^ "12345") in
        advance t 5;
        resize t ~len;
        [%test_result: Int63.Hex.t]
          (crc32 t)
          ~expect:crc

    end)
  ;;

  module Accessors (Accessors : sig
      include module type of Unsafe
      val is_safe : bool
    end) =
  struct
    open Accessors

    (* [window] *)
    let%test_unit _ =
      let t = create ~len:10 in
      let n = length t in
      assert (n = 10);
      Poke.char t ~pos:0 'a';
      Poke.char t ~pos:1 'b';
      sub_shared t |> (fun t ->
        assert (length t = n);
        assert (Consume.char t = 'a'));
      assert (Consume.char t = 'a');
      assert (Consume.char t = 'b')
    ;;

    let in_each_window t ~f =
      let n = length t in
      for pos = 0 to n do
        for len = 0 to n - pos do
          sub_shared t ~pos ~len |> f;
        done;
      done;
    ;;

    (* [sub_shared ?pos ?len]  *)
    let%test_unit _ =
      let test t =
        let n = length t in
        let window_length ?pos ?len () =
          sub_shared t ?pos ?len |> length
        in
        let window_fails ?pos ?len () =
          is_error (try_with (fun () -> ignore (sub_shared t ?pos ?len |> ignore)))
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
      done
    ;;

    module Lo_bound = struct
      type t = Lo_bound.t [@@deriving compare]

      let sexp_of_t = Lo_bound.sexp_of_t

      let window, restore = Lo_bound.(window, restore)
      let%test _ =
        let iobuf = create ~len:2 in
        let snapshot = window iobuf in
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
        is_error (try_with (fun () -> restore snapshot iobuf))

      let limit = Lo_bound.limit
      let%test _ =
        let buf = of_string "123abcDEF" in
        advance buf 3;
        let lo_min = limit buf in
        resize buf ~len:3;
        restore lo_min buf;
        String.equal "123abc" (to_string buf)
    end

    module Hi_bound = struct
      type t = Hi_bound.t [@@deriving compare]

      let sexp_of_t = Hi_bound.sexp_of_t

      let window, restore = Hi_bound.(window, restore)
      let%test _ =
        let iobuf = create ~len:2 in
        let snapshot = window iobuf in
        assert (length iobuf = 2);
        resize iobuf ~len:1;
        assert (length iobuf = 1);
        restore snapshot iobuf;
        assert (length iobuf = 2);
        (* the same snapshot can be reused *)
        resize iobuf ~len:1;
        assert (length iobuf = 1);
        restore snapshot iobuf;
        assert (length iobuf = 2);
        (* but can fail in combination with narrow *)
        resize iobuf ~len:1;
        narrow iobuf;
        assert (capacity iobuf = 1);
        assert (length iobuf = 1);
        is_error (try_with (fun () -> restore snapshot iobuf))

      let limit = Hi_bound.limit
      let%test _ =
        let buf = of_string "123abcDEF" in
        resize buf ~len:3;
        let hi_max = limit buf in
        advance buf 3;
        restore hi_max buf;
        String.equal "abcDEF" (to_string buf)
    end

    let rewind = rewind
    let reset = reset
    let flip_lo = flip_lo
    let bounded_flip_lo = bounded_flip_lo
    let%test _ =
      let buf = of_string "123abcDEF" in
      Iobuf.advance buf 3;
      let lo = Lo_bound.window buf in
      Iobuf.advance buf 3;
      bounded_flip_lo buf lo;
      String.equal "abc" (Iobuf.to_string buf)
    ;;
    let flip_hi = flip_hi
    let%test _ =
      let buf = of_string "123abcDEF" in
      Iobuf.resize buf ~len:3;
      Iobuf.flip_hi buf;
      String.equal "abcDEF" (Iobuf.to_string buf)
    ;;
    let%test _ =
      let buf = of_string "123abcDEF" in
      Iobuf.resize buf ~len:6;
      Iobuf.narrow buf;
      Iobuf.resize buf ~len:3;
      Iobuf.flip_hi buf;
      String.equal "abc" (Iobuf.to_string buf)
    ;;
    let bounded_flip_hi = bounded_flip_hi
    let%test _ =
      let buf = of_string "123abcDEF" in
      let hi = Hi_bound.window buf in
      Iobuf.advance buf 3;
      Iobuf.resize buf ~len:3;
      bounded_flip_hi buf hi;
      String.equal "DEF" (Iobuf.to_string buf)
    ;;

    let compact = compact
    let sub_shared = sub_shared
    let set_bounds_and_buffer_sub = set_bounds_and_buffer_sub
    let set_bounds_and_buffer = set_bounds_and_buffer
    let narrow = narrow
    let narrow_lo = narrow_lo
    let narrow_hi = narrow_hi
    let%test _ =
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
    let%test _ =
      let src = of_string "123abcDEF" in
      let dst = Iobuf.create ~len:0 in
      set_bounds_and_buffer ~src ~dst;
      src = dst
    ;;
    let%test _ =
      let src = of_string "123abcDEF" in
      let dst = Iobuf.create ~len:0 in
      set_bounds_and_buffer_sub ~src ~dst ~len:(length src) ~pos:0;
      src = dst
    ;;
    let%test _ =
      let src = of_string "123abcDEF" in
      let src = Iobuf.sub_shared ~pos:1 ~len:5 src in
      let dst = Iobuf.create ~len:0 in
      set_bounds_and_buffer ~src ~dst;
      src = dst
    ;;
    let%test _ =
      let src = of_string "123abcDEF" in
      let src = Iobuf.sub_shared ~pos:1 ~len:5 src in
      let dst = Iobuf.create ~len:0 in
      set_bounds_and_buffer_sub ~src ~dst ~len:(length src) ~pos:0;
      src = dst
    ;;
    let%test _ =
      let src = of_string "123abcDEF" in
      let src_sub = Iobuf.sub_shared ~pos:1 ~len:5 src in
      let dst = Iobuf.create ~len:0 in
      set_bounds_and_buffer_sub ~pos:2 ~len:2 ~src:src_sub ~dst;
      let src_sub' = Iobuf.sub_shared ~pos:3 ~len:2 src in
      src_sub' = dst
    ;;
    let%test _ =
      let buf1 = of_string "123abcDEF" in
      let buf2 = of_string "123abcDEF" in
      Iobuf.set_bounds_and_buffer ~src:buf1 ~dst:buf1;
      buf1 = buf2
    ;;

    let bounded_compact = bounded_compact
    let%test _ =
      let buf = of_string "123abcDEFghiJKL" in
      advance buf 3;
      let lo = Lo_bound.window buf in
      Iobuf.resize buf ~len:9;
      let hi = Hi_bound.window buf in
      advance buf 6;
      bounded_compact buf lo hi;
      assert (String.equal "DEFghi" (Iobuf.to_string buf));
      Iobuf.reset buf;
      String.equal "123ghiDEFghiJKL" (Iobuf.to_string buf)
    ;;
    let resize = resize
    let unsafe_resize = unsafe_resize

    let%test_unit _ =
      List.iter [ resize,true ; unsafe_resize,false ]
        ~f:(fun (resize, test_invalid_access) ->
          let buf = of_string "123abcDEF" in
          if test_invalid_access then begin
            let resize_fails amount =
              is_error (try_with (fun () -> resize buf ~len:amount))
            in
            assert (resize_fails (-1));
            assert (resize_fails (Iobuf.length buf + 1))
          end;
          let sub = ref "" in
          let f (buf : (read, seek) t) =
            advance buf 3;
            resize buf ~len:3;
            sub := to_string buf;
          in
          Iobuf.protect_window_and_bounds buf ~f;
          assert (String.equal (to_string buf) "123abcDEF" && String.equal !sub "abc"))
    ;;

    let%test _ =
      let buf = of_string "123abcDEF" in
      let sub = ref "" in
      let f (buf : (read, seek) t) =
        advance buf 3;
        resize buf ~len:3;
        sub := to_string buf;
        raise Caml.Not_found;
      in
      try
        Nothing.unreachable_code (Iobuf.protect_window_and_bounds buf ~f)
      with
      | Not_found_s _ | Caml.Not_found ->
        String.equal (to_string buf) "123abcDEF" && String.equal !sub "abc"
    ;;

    let protect_window_and_bounds = protect_window_and_bounds
    let%test_unit _ =
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
      sub_shared t ~pos:1 |> test
    ;;

    let to_string = to_string

    let of_string = of_string

    let%test_unit _ =
      List.iter strings ~f:(fun string ->
        assert (String.equal string (to_string (of_string string))))
    ;;

    let of_bigstring = of_bigstring

    let%test_unit _ =
      List.iter strings ~f:(fun string ->
        let bigstring = Bigstring.of_string string in
        iter_slices (String.length string)
          ~f:(fun ?pos ?len ~pos':_ ~len':_ ~is_valid () ->
            match try_with (fun () -> of_bigstring bigstring ?pos ?len) with
            | Error _ -> assert (not is_valid)
            | Ok t ->
              assert is_valid;
              assert (String.equal
                        (to_string t)
                        (Bigstring.to_string bigstring ?pos ?len))))
    ;;

    let advance = advance
    let unsafe_advance = unsafe_advance

    let%test_unit _ =
      List.iter [advance, true;
                 unsafe_advance, false]
        ~f:(fun (advance, test_invalid_access) ->
          for len = 1 to 5 do
            let t = create ~len in
            let advance_fails amount =
              is_error (try_with (fun () -> advance t amount))
            in
            if test_invalid_access then begin
              assert (advance_fails (-1));
              assert (advance_fails (len + 1));
            end;
            for amount = 0 to len do
              sub_shared t |> (fun t ->
                advance t amount;
                assert (length t = len - amount))
            done;
          done;
        )
    ;;

    let bin_prot_length_prefix_bytes = bin_prot_length_prefix_bytes
    let consume_bin_prot = consume_bin_prot
    let fill_bin_prot = fill_bin_prot

    let%test_unit _ =
      let t = create ~len:2000 in
      List.iter strings
        ~f:(fun s ->
          assert (is_ok (fill_bin_prot t String.bin_writer_t s));
          flip_lo t;
          let s' = ok_exn (consume_bin_prot t String.bin_reader_t) in
          reset t;
          assert (String.equal s s'))
    ;;

    module type Accessee = sig
      type t [@@deriving sexp_of]
      include Stringable with type t := t
      val equal : t -> t -> bool
    end

    module Intf (Intf : sig
        include Iobuf_intf.Accessors

        val accessor_pos_1
          :  without_value : (read_write, seek) Iobuf.t
          -> value_len : int
          -> ('a, read_write, seek) t
          -> value : 'a
          -> with_value : string
          -> (module Accessee with type t = 'a)
          -> unit

        val bin_prot_char : (char, 'd, 'w) t
      end) = struct
      open Intf

      type nonrec ('a, 'd, 'w) t = ('a, 'd, 'w) t

      let char            = char
      let  int8           =  int8
      let uint8           = uint8
      let  int16_be       =  int16_be
      let  int16_le       =  int16_le
      let uint16_be       = uint16_be
      let uint16_le       = uint16_le
      let  int32_be       =  int32_be
      let  int32_le       =  int32_le
      let uint32_be       = uint32_be
      let uint32_le       = uint32_le
      let  int64_be       =  int64_be
      let uint64_le       = uint64_le
      let uint64_be       = uint64_be
      let  int64_le       =  int64_le
      let  int64_t_be     =  int64_t_be
      let  int64_t_le     =  int64_t_le
      let  int64_be_trunc =  int64_be_trunc
      let  int64_le_trunc =  int64_le_trunc

      let tail_padded_fixed_string  = tail_padded_fixed_string
      let head_padded_fixed_string  = head_padded_fixed_string
      let                   bytes   =                   bytes
      let                   string  =                   string
      let                bigstring  =                bigstring
      let                   byteso  =                    byteso
      let                   stringo =                   stringo
      let                bigstringo =                bigstringo
      let bin_prot                  = bin_prot

      module type String_accessee = sig
        include Accessee
        val subo : (t, t) Base.Blit.subo
      end

      module Bigstringable : String_accessee with type t = bigstring = struct
        include Bigstring
        let to_string t = to_string t (* drop optional args *)
        let of_string s = of_string s (* drop optional args *)
      end

      let%test_unit _ =
        let buf = of_string "ABCDEFGHIJ" in
        let int_pos_1 n a v s =
          accessor_pos_1 ~without_value:buf ~value_len:n a ~value:v ~with_value:s
            (module Int)
        in
        int_pos_1 1 int8  127    "A\127CDEFGHIJ";
        int_pos_1 1 int8  (-1)   "A\255CDEFGHIJ";
        int_pos_1 1 int8  (-128) "A\128CDEFGHIJ";
        int_pos_1 1 uint8 0      "A\000CDEFGHIJ";
        int_pos_1 1 uint8 255    "A\255CDEFGHIJ";
        let char_pos_1  a v s =
          accessor_pos_1 ~without_value:buf ~value_len:1 a ~value:v ~with_value:s
            (module Char)
        in
        char_pos_1 char 'K' "AKCDEFGHIJ";
        char_pos_1 char '\254' "A\254CDEFGHIJ";
        int_pos_1 1 int8  (-1) "A\255CDEFGHIJ";
        int_pos_1 1 uint8 12   "A\012CDEFGHIJ";
        char_pos_1 bin_prot_char 'x' "AxCDEFGHIJ";
        int_pos_1 2 int16_be  0x0102 "A\001\002DEFGHIJ";
        int_pos_1 2 int16_le  0x0304 "A\004\003DEFGHIJ";
        int_pos_1 2 int16_be  (-2)   "A\255\254DEFGHIJ";
        int_pos_1 2 int16_le  (-3)   "A\253\255DEFGHIJ";
        int_pos_1 2 uint16_be 0xFFFE "A\255\254DEFGHIJ";
        int_pos_1 2 uint16_le 0xFDFC "A\252\253DEFGHIJ";
        int_pos_1 2 int16_be  0x0506 "A\005\006DEFGHIJ";
        int_pos_1 2 int16_le  0x0708 "A\008\007DEFGHIJ";
        let metastring_pos_1 (type v)
              (module Value : String_accessee with type t = v)
              value_len accessor ~skip value with_value =
          accessor_pos_1 ~without_value:buf ~value_len accessor
            ~value:(Value.of_string value)
            ~with_value
            (module struct
              include Value

              (* The prefix is garbage if [str_pos <> 0]. *)
              let equal t u = equal (subo ~pos:skip t) (subo ~pos:skip u)
            end)
        in
        let string_pos_1    = metastring_pos_1 (module String)        in
        let bigstring_pos_1 = metastring_pos_1 (module Bigstringable) in
        let padded_string_pos_1 ~len str ~head ~tail =
          accessor_pos_1 ~without_value:buf ~value_len:len
            (tail_padded_fixed_string ~padding:'p' ~len)
            ~value:str ~with_value:tail (module String);
          accessor_pos_1 ~without_value:buf ~value_len:len
            (head_padded_fixed_string ~padding:'p' ~len)
            ~value:str ~with_value:head (module String)
        in
        padded_string_pos_1 ~len:3 "x"   ~head:"AppxEFGHIJ" ~tail:"AxppEFGHIJ";
        padded_string_pos_1 ~len:3 "xxx" ~head:"AxxxEFGHIJ" ~tail:"AxxxEFGHIJ";
        padded_string_pos_1 ~len:3 "xpx" ~head:"AxpxEFGHIJ" ~tail:"AxpxEFGHIJ";
        padded_string_pos_1 ~len:3 ""    ~head:"ApppEFGHIJ" ~tail:"ApppEFGHIJ";
        string_pos_1    3 (string     ~str_pos:0    ~len:3) ~skip:0 "123"    "A123EFGHIJ";
        bigstring_pos_1 3 (bigstring  ~str_pos:0    ~len:3) ~skip:0 "klm"    "AklmEFGHIJ";
        string_pos_1    2 (string     ~str_pos:1    ~len:2) ~skip:1 "\00023" "A23mEFGHIJ";
        bigstring_pos_1 2 (bigstring  ~str_pos:1    ~len:2) ~skip:1 "\000lm" "AlmmEFGHIJ";
        string_pos_1    3 (stringo    ?str_pos:None ~len:3) ~skip:0 "123"    "A123EFGHIJ";
        bigstring_pos_1 3 (bigstringo ?str_pos:None ~len:3) ~skip:0 "klm"    "AklmEFGHIJ";
        string_pos_1    3 (stringo    ~str_pos:0    ~len:3) ~skip:0 "123"    "A123EFGHIJ";
        bigstring_pos_1 3 (bigstringo ~str_pos:0    ~len:3) ~skip:0 "klm"    "AklmEFGHIJ";
        string_pos_1    2 (stringo    ~str_pos:1    ~len:2) ~skip:1 "\00023" "A23mEFGHIJ";
        bigstring_pos_1 2 (bigstringo ~str_pos:1    ~len:2) ~skip:1 "\000lm" "AlmmEFGHIJ";
        int_pos_1 4 int32_be 0x0A0B0C0D    "A\010\011\012\013FGHIJ";
        int_pos_1 4 int32_le 0x01020304    "A\004\003\002\001FGHIJ";
        int_pos_1 4 int32_be (-0x01020305) "A\254\253\252\251FGHIJ";
        int_pos_1 4 int32_le (-0x05060709) "A\247\248\249\250FGHIJ";
        if arch_sixtyfour
        then begin
          int_pos_1 4 uint32_be (large_int 0 0 0xF6F5 0xF4F3) "A\246\245\244\243FGHIJ";
          int_pos_1 4 uint32_le (large_int 0 0 0xFBFA 0xF9F8) "A\248\249\250\251FGHIJ";
          int_pos_1 8 int64_be (large_int 0x0102 0x0304 0x0506 0x0708)
            "A\001\002\003\004\005\006\007\008J";
          int_pos_1 8 int64_le (large_int 0x090a 0x0b0c 0x0d0e 0x0f10)
            "A\016\015\014\013\012\011\010\009J";
          int_pos_1 8 int64_be (-(large_int 0x0102 0x0304 0x0506 0x0709))
            "A\254\253\252\251\250\249\248\247J";
          int_pos_1 8 int64_le (-(large_int 0x0102 0x0304 0x0506 0x0709))
            "A\247\248\249\250\251\252\253\254J";
          int_pos_1 8 int64_be_trunc (large_int 0x0102 0x0304 0x0506 0x0708)
            "A\001\002\003\004\005\006\007\008J";
          int_pos_1 8 int64_le_trunc (large_int 0x090a 0x0b0c 0x0d0e 0x0f10)
            "A\016\015\014\013\012\011\010\009J";
          int_pos_1 8 int64_be_trunc (-(large_int 0x0102 0x0304 0x0506 0x0709))
            "A\254\253\252\251\250\249\248\247J";
          int_pos_1 8 int64_le_trunc (-(large_int 0x0102 0x0304 0x0506 0x0709))
            "A\247\248\249\250\251\252\253\254J"
        end;
        let int64_pos_1 f value with_value =
          accessor_pos_1 ~without_value:buf ~value_len:8 f ~value ~with_value
            (module Int64)
        in
        int64_pos_1 int64_t_be 1L                  "A\000\000\000\000\000\000\000\001J";
        int64_pos_1 int64_t_le 1L                  "A\001\000\000\000\000\000\000\000J";
        int64_pos_1 int64_t_be 0x8000000000000000L "A\128\000\000\000\000\000\000\000J";
        int64_pos_1 int64_t_le 0x8000000000000000L "A\000\000\000\000\000\000\000\128J"
    end

    let cases_for_testing_decimal =
      [ Int.min_value; (Int.min_value + 1); -100; -9; 0; 10; 15; 45; 120; 987; 2814;
        10000; 100_000; 1_000_000; Int.max_value ]
      @ List.init 1_000
          ~f:(fun _ -> Random.int (1 lsl 29) lsl Random.int 35)
    ;;

    module Poke = struct
      include Intf (struct
          include Poke
          type 'a bin_prot = 'a Bin_prot.Type_class.writer

          let accessor_pos_1 (type a)
                ~without_value:buf ~value_len:_ f ~value:arg ~with_value:str
                (module Arg : Accessee with type t = a) =
            f buf ~pos:1 arg;
            let buf_str = to_string buf in
            if String.(<>) str buf_str then
              raise_s [%message "Poke accessor_pos_1 failed"
                                  (str     : string)
                                  (buf_str : string)
                                  (arg     : Arg.t)]

          let bin_prot_char t ~pos a = bin_prot Char.bin_writer_t t ~pos a

          (* Static permission tests for the cases that do compile.  Since the functions
             all use essentially the same type definitions, we don't need to test all of
             them.  We've already tested them on a (read_write, seek) Iobuf.t above. *)
          let%test_unit _ =
            char (of_string "a" : (_,          no_seek) Iobuf.t) ~pos:0 'b';
            char (of_string "a" : (_,          seek)    Iobuf.t) ~pos:0 'b';
            char (of_string "a" : (read_write, _)       Iobuf.t) ~pos:0 'b'
        end)

      let decimal = Poke.decimal
      let%test_unit "Poke.decimal" =
        let pos = 1 in
        let t = create ~len:(20 + pos) in
        List.iter cases_for_testing_decimal ~f:(fun x ->
          Iobuf.reset t;
          let len = decimal t ~pos x in
          [%test_eq: string]
            (Int.to_string x)
            (Iobuf.to_string (Iobuf.sub_shared ~pos t) ~len))
    end

    let%test_unit _ =
      let t = create ~len:10 in
      let n = length t in
      assert (n = 10);
      Poke.char t ~pos:0 'a';
      Poke.char t ~pos:1 'b';
      sub_shared t |> (fun t ->
        assert (length t = n);
        assert (Consume.char t = 'a'));
      assert (Consume.char t = 'a');
      assert (Consume.char t = 'b');
      let ws = (t  :> (read_write, seek) t) in
      let rs = (ws :> (read, seek) t) in
      ignore   (rs :> (read, no_seek) t);
      ignore   (ws :> (read_write, no_seek) t)
    ;;

    let test_peek_to (blito : (Peek.src, _) Core_kernel.Core_kernel_private.Std_kernel.Blit.blito)
          create sub_string of_string to_string =
      iter_examples ~f:(fun t string ~pos ->
        let n = String.length string in
        iter_slices n ~f:(fun ?pos:str_pos ?len ~pos' ~len' ~is_valid () ->
          let fill_result =
            Or_error.try_with (fun () ->
              sub_shared t ~pos |> (fun t -> Fill.stringo t string ?str_pos ?len))
          in
          [%test_eq: bool] (is_ok fill_result) is_valid;
          let str = create n in
          let consume_result =
            Or_error.try_with (fun () ->
              sub_shared (read_only (no_seek t)) ~pos
              |> (fun t ->
                blito ~src:t ~src_pos:0 ~src_len:len' ~dst:str ?dst_pos:str_pos ()))
          in
          begin match consume_result, is_valid with
          | Error _, false -> ()
          | Ok (), true ->
            [%test_eq: string] (String.sub string ~pos:pos' ~len:len')
              (sub_string str ~pos:pos' ~len:len')
          | _, _ ->
            failwiths "test_peek_to"
              ( (consume_result, `is_valid is_valid)
              , Backtrace.Exn.most_recent ()
              )
              [%sexp_of: (unit Or_error.t * [ `is_valid of bool ]) * Backtrace.t]
          end
        )
      );
      let t = Iobuf.of_string "012345678" in
      let dst = of_string "abcdefhij" in
      blito ~src:t ~src_len:3 ~dst ~dst_pos:3 ();
      [%test_eq: string] (Iobuf.to_string t) "012345678";
      [%test_eq: string] (to_string dst) "abc012hij"
    ;;
    let test_peek_to_string blito =
      test_peek_to blito Bytes.create Bytes.To_string.sub Bytes.of_string Bytes.to_string
    let test_peek_to_bigstring blito =
      test_peek_to blito Bigstring.create
        (fun s ~pos ~len -> Bigstring.to_string s ~pos ~len)
        Bigstring.of_string
        Bigstring.to_string
    ;;

    module Peek = struct
      include Intf (struct
          include Peek
          type 'a bin_prot = 'a Bin_prot.Type_class.reader

          let accessor_pos_1 (type a)
                ~without_value:_ ~value_len:_ f ~value:expected ~with_value:str
                (module Res : Accessee with type t = a) =
            let res = f (of_string str) ~pos:1 in
            if not (Res.equal res expected) then
              raise_s [%message "Peek accessor_pos_1 failed"
                                  (res      : Res.t)
                                  (expected : Res.t)
                                  (str      : string)]

          let bin_prot_char t ~pos = bin_prot Char.bin_reader_t t ~pos

          (* static permission tests; see above *)
          let char_a_pos_0 iobuf = Char.(=) 'a' (char iobuf ~pos:0)
          let%test _ = char_a_pos_0 (of_string "a" : (_,          no_seek) Iobuf.t)
          let%test _ = char_a_pos_0 (of_string "a" : (_,          seek)    Iobuf.t)
          let%test _ = char_a_pos_0 (of_string "a" : (read,       _)       Iobuf.t)
          let%test _ = char_a_pos_0 (of_string "a" : (read_write, _)       Iobuf.t)
        end)

      open Peek

      let index = index

      module To_bytes = struct
        open Peek.To_bytes
        let blito = blito
        let%test_unit _ = test_peek_to_string blito
        (* Mostly rely on the [Blit_tests.Make_distinct] testing. *)
        let blit, unsafe_blit, sub, subo = blit, unsafe_blit, sub, subo
      end
      module To_bigstring = struct
        open Peek.To_bigstring
        let blito = blito
        let%test_unit _ = test_peek_to_bigstring blito
        (* Mostly rely on the [Blit_tests.Make_distinct] testing. *)
        let blit, unsafe_blit, sub, subo = blit, unsafe_blit, sub, subo
      end
      module To_string = struct
        let blit, blito, unsafe_blit = Peek.To_bytes.(blit, blito, unsafe_blit)
        let sub, subo = Peek.To_string.(sub, subo)
      end
      type nonrec src = src
    end

    let%test_unit _ =
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
           Poke.char t ~pos:(String.length s) 'z'))))
    ;;

    let%test_unit _ =
      List.iter [ 0; 1 ] ~f:(fun pos ->
        let t = create ~len:10 in
        if arch_sixtyfour then begin
          let i = large_int 0x1234 0x5678 0x90AB 0xCDEF in
          Poke.int64_le t ~pos i;
          assert (Peek.int64_le t ~pos = i);
          Poke.int64_be t ~pos i;
          assert (Peek.int64_be t ~pos = i)
        end;
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

    module Fill = struct
      include Intf (struct
          include Fill
          type 'a bin_prot = 'a Bin_prot.Type_class.writer

          let accessor_pos_1 (type a)
                ~without_value:buf ~value_len:n f ~value:arg ~with_value:str
                (module Arg : Accessee with type t = a) =
            rewind buf;
            advance buf 1;
            f buf arg;
            if Iobuf.length buf <> String.length str - 1 - n then
              raise_s [%message "Fill accessor_pos_1 post length check failed"
                                  (buf : (_, _) Iobuf.Hexdump.t)
                                  (str : string)
                                  (n   : int)
                                  (arg : Arg.t)];
            rewind buf;
            let buf_str = to_string buf in
            if String.(<>) buf_str str then
              raise_s [%message "Fill accessor_pos_1 post rewind check failed"
                                  (buf_str : string)
                                  (str     : string)
                                  (n       : int)
                                  (arg     : Arg.t)]

          let bin_prot_char t a = bin_prot Char.bin_writer_t t a

          let%test_unit _ =
            let t = of_string "abc" in
            bin_prot Char.bin_writer_t t 'd';
            bin_prot Char.bin_writer_t t 'e';
            [%test_eq: string] "c" (to_string t);
            flip_lo t;
            assert (try bin_prot String.bin_writer_t t "fgh"; false with _ -> true);
            [%test_eq: string] "de" (to_string t);
            reset t;
            [%test_eq: string] "dec" (to_string t);
            bin_prot Char.bin_writer_t t 'i';
            bin_prot Char.bin_writer_t t 'j';
            bin_prot Char.bin_writer_t t 'k';
            assert (is_empty t);
            flip_lo t;
            [%test_eq: string] "ijk" (to_string t)

          (* static permission tests; see above *)
          let%test_unit _ = char (of_string "a" : (_, seek) Iobuf.t) 'b'
          let%test_unit _ = char (of_string "a" : (read_write, _) Iobuf.t) 'b'
        end)

      let decimal = Fill.decimal
      let%test_unit "Fill.decimal" =
        let t = create ~len:20 in
        List.iter cases_for_testing_decimal ~f:(fun x ->
          Iobuf.reset t;
          decimal t x;
          Iobuf.flip_lo t;
          [%test_eq: string] (Int.to_string x) (Iobuf.to_string t))
    end

    let%test_unit _ =
      List.iter [ ""; "a"; "ab" ] ~f:(fun s ->
        let len = String.length s in
        for capacity = max 1 len to len + 2 do
          let t = create ~len:capacity in
          for pos = 0 to capacity - len do
            sub_shared t ~pos |> (fun t -> Fill.stringo t s);
            sub_shared t ~pos ~len |> (fun t ->
              let s' = to_string t in
              assert (String.equal s s');
              assert (length t = len); (* [to_string] didn't change the length *)
            );
          done;
        done)
    ;;

    let%test_unit _ =
      let src = create ~len:5 in
      let len = 3 in
      let str = "Hi." in
      assert (len >= String.length str);
      assert (capacity src >= len);
      Poke.stringo src str ~pos:0;
      Blit.blit ~src ~src_pos:0 ~dst:src ~dst_pos:2 ~len;
      [%test_result: string] (to_string src) ~expect:"HiHi."
    ;;

    let%test_unit _ =
      let src = create ~len:5 in
      let len = 3 in
      let str = "Hi." in
      assert (len >= String.length str);
      assert (capacity src >= len);
      let dst = create ~len:5 in
      Fill.stringo src str;
      flip_lo src;
      Blit_consume_and_fill.blit ~src ~dst ~len;
      flip_lo dst;
      [%test_result: string] (Consume.stringo dst) ~expect:str
    ;;

    let test_consume_to (blito : (Consume.src, _) Iobuf_intf.consuming_blito)
          create sub_string of_string to_string =
      iter_examples ~f:(fun t string ~pos ->
        let n = String.length string in
        iter_slices n ~f:(fun ?pos:str_pos ?len ~pos' ~len' ~is_valid () ->
          let fill_result =
            Or_error.try_with (fun () ->
              sub_shared t ~pos |> (fun t -> Fill.stringo t string ?str_pos ?len))
          in
          [%test_eq: bool] (is_ok fill_result) is_valid;
          let str = create n in
          let consume_result =
            Or_error.try_with (fun () ->
              sub_shared (read_only t) ~pos
              |> (fun t -> blito ~src:t ~src_len:len' ~dst:str ?dst_pos:str_pos ()))
          in
          begin match consume_result, is_valid with
          | Error _, false -> ()
          | Ok (), true ->
            [%test_eq: string] (String.sub string ~pos:pos' ~len:len')
              (sub_string str ~pos:pos' ~len:len');
          | _, _ ->
            failwiths "test_consume_to"
              (consume_result, is_valid, Backtrace.Exn.most_recent ())
              [%sexp_of: unit Or_error.t * bool * Backtrace.t]
          end));
      let t = Iobuf.of_string "012345678" in
      let dst = of_string "abcdefhij" in
      blito ~src:t ~src_len:3 ~dst ~dst_pos:3 ();
      [%test_eq: string] (Iobuf.to_string t) "345678";
      [%test_eq: string] (to_string dst) "abc012hij"
    ;;
    let test_consume_to_string blito =
      test_consume_to blito Bytes.create Bytes.To_string.sub Bytes.of_string Bytes.to_string
    let test_consume_to_bigstring blito =
      test_consume_to blito Bigstring.create
        (fun s ~pos ~len -> Bigstring.to_string s ~pos ~len)
        Bigstring.of_string
        Bigstring.to_string
    ;;

    module Consume = struct
      include Intf (struct
          include Consume
          type 'a bin_prot = 'a Bin_prot.Type_class.reader

          let accessor_pos_1 (type r)
                ~without_value:_ ~value_len:n f ~value:expected ~with_value:str
                (module Res : Accessee with type t = r) =
            let buf = of_string str in
            advance buf 1;
            let res = f buf in
            if Iobuf.length buf <> String.length str - 1 - n then
              raise_s [%message "Consume accessor_pos_1 post length check failed"
                                  (buf : (_, _) Iobuf.Hexdump.t)
                                  (str : string)
                                  (n   : int)
                                  (res : Res.t)];
            rewind buf;
            if not (Res.equal res expected) then
              raise_s [%message "Consume accessor_pos_1 post rewind check failed"
                                  (res      : Res.t)
                                  (expected : Res.t)
                                  (str      : string)
                                  (n        : int)]

          let bin_prot_char t = bin_prot Char.bin_reader_t t

          (* static permission tests; see above *)
          let%test _ = Char.(=) 'a' (char (of_string "a" : (_,          seek) Iobuf.t))
          let%test _ = Char.(=) 'a' (char (of_string "a" : (read,       _)    Iobuf.t))
          let%test _ = Char.(=) 'a' (char (of_string "a" : (read_write, _)    Iobuf.t))
        end)

      open Consume

      module To_bytes = struct
        open To_bytes
        let blito = blito
        let%test_unit _ = test_consume_to_string blito
        (* Mostly rely on the [Blit_tests.Make_distinct] testing. *)
        let blit, unsafe_blit, sub, subo = blit, unsafe_blit, sub, subo
      end
      module To_bigstring = struct
        open To_bigstring
        let blito = blito
        let%test_unit _ = test_consume_to_bigstring blito
        (* Mostly rely on the [Blit_tests.Make_distinct] testing. *)
        let blit, unsafe_blit, sub, subo = blit, unsafe_blit, sub, subo
      end
      module To_string = struct
        let blit, blito, unsafe_blit = To_bytes.(blit, blito, unsafe_blit)
        let sub, subo = To_string.(sub, subo)
      end
      type nonrec src = src
    end

    let%test_unit _ =
      let t = create ~len:1 in
      let c = 'a' in
      sub_shared t |> (fun t ->
        Fill.char t c;
        assert (is_empty t));
      assert (Char.equal (Consume.char t) c);
      assert (is_empty t)
    ;;

    let%test_unit _ =
      List.iter [ 0; 1 ] ~f:(fun pos ->
        let t = create ~len:10 in
        if arch_sixtyfour then begin
          let i = large_int 0x1234 0x5678 0x90AB 0xCDEF in
          sub_shared t ~pos |> (fun t -> Fill.int64_le t i);
          assert (i = (sub_shared t ~pos |> Consume.int64_le))
        end;
        let i = 0x1234_5678 in
        sub_shared t ~pos |> (fun t -> Fill.int32_le t i);
        assert (i = (sub_shared t ~pos |> Consume.int32_le));
        let i = 0x1234 in
        sub_shared t ~pos |> (fun t -> Fill.int16_le t i);
        assert (i = (sub_shared t ~pos |> Consume.int16_le));
        ()
      )
    ;;

    let%test_unit _ =
      List.iter strings ~f:(fun s ->
        let t = of_string s in
        let s' = Consume.stringo t in
        assert (is_empty t);
        assert (String.equal s s'))
    ;;

    (* [Fill.string] ranges *)
    let%test_unit _ =
      let s = "hello" in
      let len = String.length s in
      let t = create ~len:len in
      let try_write ?str_pos ?len () : _ Or_error.t =
        try_with (fun () -> sub_shared t |> (fun t -> Fill.stringo t s ?str_pos ?len))
      in
      if is_safe then
        (assert (is_error (try_write ~str_pos:(-1) ()));
         assert (is_error (try_write ~str_pos:(len + 1) ()));
         assert (is_error (try_write ~str_pos:0 ~len:(len + 1) ())))
    ;;

    module Blit_consume_and_fill = struct
      open Blit_consume_and_fill

      let blit = blit
      let blito = blito
      let unsafe_blit = unsafe_blit

      let blit_via_blito ~src ~dst ~len =
        blito ()
          ~src
          ~dst
          ?src_len:(if len = length src then None else Some len)

      let%test_unit _ =
        List.iter [ blit ; unsafe_blit ; blit_via_blito ] ~f:(fun blit ->
          let t1 = create ~len:100 in
          let t2 = create ~len:100 in
          let s = "585038473775210758" in
          Fill.stringo t1 s;
          flip_lo t1;
          assert (String.equal s (Consume.stringo t1));
          assert (is_empty t1);
          reset t1;
          Fill.stringo t1 s;
          flip_lo t1;
          blit ~src:t1 ~dst:t2 ~len:(length t1);
          flip_lo t2;
          assert (String.equal s (Consume.stringo t2));
          assert (is_empty t2))
      ;;

    end
    ;;

    module Blit_fill = struct
      open Blit_fill

      let blit = blit
      let blito = blito
      let unsafe_blit = unsafe_blit

      let blit_via_blito ~src ~src_pos ~dst ~len =
        blito ()
          ~src ?src_pos:(if src_pos = 0 then None else Some src_pos)
          ~dst
          ?src_len:(if len = length src - src_pos then None else Some len)

      let%test_unit _ =
        List.iter [ blit ; unsafe_blit ; blit_via_blito ] ~f:(fun blit ->
          let t1 = of_string "0123456789" in
          let t2 = of_string "qwertyuiop" in
          let t3 = of_string "!@#$%^&*()" in
          [%test_result: string] (to_string t1) ~expect:"0123456789";
          [%test_result: string] (to_string t2) ~expect:"qwertyuiop";
          [%test_result: string] (to_string t3) ~expect:"!@#$%^&*()";
          blit ~dst:t1 ~src:t2 ~src_pos:0 ~len:(length t2);
          [%test_result: string] (to_string t1) ~expect:"";
          reset t1;
          [%test_result: string] (to_string t1) ~expect:"qwertyuiop";
          [%test_result: string] (to_string t2) ~expect:"qwertyuiop";
          [%test_result: string] (to_string t3) ~expect:"!@#$%^&*()";
          blit ~dst:t1 ~src:t3 ~src_pos:4 ~len:4;
          [%test_result: string] (to_string t1) ~expect:"tyuiop";
          reset t1;
          [%test_result: string] (to_string t1) ~expect:"%^&*tyuiop";
          [%test_result: string] (to_string t2) ~expect:"qwertyuiop";
          [%test_result: string] (to_string t3) ~expect:"!@#$%^&*()")
      ;;

    end
    ;;

    module Blit_consume = struct
      open Blit_consume

      let blit = blit
      let blito = blito
      let unsafe_blit = unsafe_blit

      let blit_via_blito ~src ~dst ~dst_pos ~len =
        blito ()
          ~src
          ~dst ?dst_pos:(if dst_pos = 0 then None else Some dst_pos)
          ?src_len:(if len = length src then None else Some len)

      let%test_unit _ =
        List.iter [ blit ; unsafe_blit ; blit_via_blito ] ~f:(fun blit ->
          let t1 = of_string "0123456789" in
          let t2 = of_string "qwertyuiop" in
          let t3 = of_string "!@#$%^&*()" in
          [%test_result: string] (to_string t1) ~expect:"0123456789";
          [%test_result: string] (to_string t2) ~expect:"qwertyuiop";
          [%test_result: string] (to_string t3) ~expect:"!@#$%^&*()";
          blit ~dst:t1 ~dst_pos:0 ~src:t2 ~len:(length t2);
          [%test_result: string] (to_string t2) ~expect:"";
          reset t2;
          [%test_result: string] (to_string t1) ~expect:"qwertyuiop";
          [%test_result: string] (to_string t2) ~expect:"qwertyuiop";
          [%test_result: string] (to_string t3) ~expect:"!@#$%^&*()";
          blit ~dst:t1 ~dst_pos:2 ~src:t3 ~len:4;
          [%test_result: string] (to_string t3) ~expect:"%^&*()";
          reset t3;
          [%test_result: string] (to_string t1) ~expect:"qw!@#$uiop";
          [%test_result: string] (to_string t2) ~expect:"qwertyuiop";
          [%test_result: string] (to_string t3) ~expect:"!@#$%^&*()")
      ;;

      let sub = sub
      let subo = subo

      let sub_via_subo src ~len =
        subo src
          ?len:(if len = length src then None else Some len)

      let%test_unit _ =
        List.iter [ sub ; sub_via_subo ] ~f:(fun sub ->
          let t1 = of_string "0123456789" in
          let t2 = sub t1 ~len:(length t1) in
          [%test_result: string] (to_string t1) ~expect:"";
          reset t1;
          let t3 = sub t1 ~len:4 in
          [%test_result: string] (to_string t1) ~expect:"456789";
          reset t1;
          [%test_result: string] (to_string t1) ~expect:"0123456789";
          [%test_result: string] (to_string t2) ~expect:"0123456789";
          [%test_result: string] (to_string t3) ~expect:"0123";
          Poke.stringo t1 ~pos:0 "qwertyuiop";
          Poke.stringo t2 ~pos:0 "!@#$%^&*()";
          Poke.stringo t3 ~pos:0 "asdf";
          [%test_result: string] (to_string t1) ~expect:"qwertyuiop";
          [%test_result: string] (to_string t2) ~expect:"!@#$%^&*()";
          [%test_result: string] (to_string t3) ~expect:"asdf")
      ;;

    end
    ;;

    module Blit = struct
      open Blit
      type nonrec 'rw t_no_seek = 'rw t_no_seek

      let blit = blit
      let blito = blito
      let unsafe_blit = unsafe_blit

      let blit_via_blito ~src ~src_pos ~dst ~dst_pos ~len =
        blito ()
          ~src ?src_pos:(if src_pos = 0 then None else Some src_pos)
          ~dst ?dst_pos:(if dst_pos = 0 then None else Some dst_pos)
          ?src_len:(if len = length src - src_pos then None else Some len)

      let%test_unit _ =
        List.iter [ blit ; unsafe_blit ; blit_via_blito ] ~f:(fun blit ->
          let t1 = of_string "0123456789" in
          let t2 = of_string "qwertyuiop" in
          let t3 = of_string "!@#$%^&*()" in
          [%test_result: string] (to_string t1) ~expect:"0123456789";
          [%test_result: string] (to_string t2) ~expect:"qwertyuiop";
          [%test_result: string] (to_string t3) ~expect:"!@#$%^&*()";
          blit ~dst:t1 ~dst_pos:0 ~src:t2 ~src_pos:0 ~len:(length t2);
          [%test_result: string] (to_string t1) ~expect:"qwertyuiop";
          [%test_result: string] (to_string t2) ~expect:"qwertyuiop";
          [%test_result: string] (to_string t3) ~expect:"!@#$%^&*()";
          blit ~dst:t1 ~dst_pos:2 ~src:t3 ~src_pos:4 ~len:4;
          [%test_result: string] (to_string t1) ~expect:"qw%^&*uiop";
          [%test_result: string] (to_string t2) ~expect:"qwertyuiop";
          [%test_result: string] (to_string t3) ~expect:"!@#$%^&*()")
      ;;

      let%test_unit _ =
        List.iter [ blit ; unsafe_blit ; blit_via_blito ] ~f:(fun blit ->
          let s = "01234567899876543210" in
          let t = create ~len:(String.length s) in
          Iobuf.sub_shared t |> (fun t -> Fill.stringo t s);
          blit ~src:t ~dst:t ~src_pos:0 ~dst_pos:10 ~len:10;
          let s2 = to_string t in
          assert (String.equal s2 "01234567890123456789"))
      ;;

      let sub = sub
      let subo = subo

      let sub_via_subo src ~pos ~len =
        subo src
          ?pos:(if pos = 0 then None else Some pos)
          ?len:(if len = length src - pos then None else Some len)

      let%test_unit _ =
        List.iter [ sub ; sub_via_subo ] ~f:(fun sub ->
          let t1 = of_string "0123456789" in
          let t2 = sub t1 ~pos:0 ~len:(length t1) in
          let t3 = sub t1 ~pos:3 ~len:4 in
          [%test_result: string] (to_string t1) ~expect:"0123456789";
          [%test_result: string] (to_string t2) ~expect:"0123456789";
          [%test_result: string] (to_string t3) ~expect:   "3456";
          Poke.stringo t1 ~pos:0 "qwertyuiop";
          Poke.stringo t2 ~pos:0 "!@#$%^&*()";
          Poke.stringo t3 ~pos:0 "asdf";
          [%test_result: string] (to_string t1) ~expect:"qwertyuiop";
          [%test_result: string] (to_string t2) ~expect:"!@#$%^&*()";
          [%test_result: string] (to_string t3) ~expect:"asdf")
      ;;

    end
    ;;

    module Expert = struct
      let buf    = Expert.buf
      let hi_max = Expert.hi_max
      let hi     = Expert.hi
      let lo     = Expert.lo
      let lo_min = Expert.lo_min

      let to_bigstring_shared       = Expert.to_bigstring_shared
      let to_iovec_shared           = Expert.to_iovec_shared
      let reinitialize_of_bigstring = Expert.reinitialize_of_bigstring
      let set_bounds_and_buffer     = Expert.set_bounds_and_buffer
      let set_bounds_and_buffer_sub = Expert.set_bounds_and_buffer_sub

      let%test_unit _ =
        let to_bigstring_shared_via_iovec ?pos ?len iobuf =
          let iovec = to_iovec_shared ?pos ?len iobuf in
          Bigstring.sub_shared iovec.buf ~pos:iovec.pos ~len:iovec.len
        in
        List.iter
          [ to_bigstring_shared
          ; to_bigstring_shared_via_iovec
          ]
          ~f:(fun to_bstr ->
            let iobuf = Iobuf.of_string "0123456789" in
            let bstr0 = to_bstr iobuf in
            [%test_result: Bigstring.t] bstr0 ~expect:(Bigstring.of_string "0123456789");
            Iobuf.Poke.char iobuf ~pos:0 'X';
            [%test_result: Bigstring.t] bstr0 ~expect:(Bigstring.of_string "X123456789");
            let bstr1 = to_bstr iobuf ~pos:1 ~len:8 in
            [%test_result: Bigstring.t] bstr1 ~expect:(Bigstring.of_string "12345678");
            Iobuf.Poke.char iobuf ~pos:1 'X';
            [%test_result: Bigstring.t] bstr1 ~expect:(Bigstring.of_string "X2345678");
            [%test_result: Bigstring.t] bstr0 ~expect:(Bigstring.of_string "XX23456789"))
      ;;

      let%test_unit "reinitialize_of_bigstring" =
        let iobuf = Iobuf.of_string "1234" in
        let bstr0 = buf iobuf in
        [%test_result: Bigstring.t] bstr0 ~expect:(Bigstring.of_string "1234");
        let bstr1 = Bigstring.of_string "abcd" in
        reinitialize_of_bigstring iobuf bstr1 ~len:(Bigstring.length bstr1) ~pos:0;
        assert (phys_equal bstr1 (buf iobuf));
        Iobuf.Poke.char iobuf ~pos:0 'A';
        [%test_result: Bigstring.t] bstr0 ~expect:(Bigstring.of_string "1234");
        [%test_result: Bigstring.t] bstr1 ~expect:(Bigstring.of_string "Abcd");
        [%test_result: string] (Iobuf.to_string iobuf) ~expect:"Abcd"
      ;;

      let%test "set_bounds_and_buffer from ro" =
        let src = read_only (of_string "123abcDEF") in
        let dst = Iobuf.create ~len:0 in
        Expert.set_bounds_and_buffer ~src ~dst;
        src = dst
      ;;
    end

    type nonrec ok_or_eof = ok_or_eof = Ok | Eof [@@deriving compare, sexp_of]

    module Io_test(Ch : sig
        type in_
        val create_in : string -> in_
        val close_in : in_ -> unit
        val read : ([> write], seek) Iobuf.t -> in_ -> ok_or_eof
        type out_
        val create_out : Unix.File_descr.t -> out_
        val close_out : out_ -> unit
        val write : ([> read], seek) Iobuf.t -> out_ -> unit
      end) = struct

      let%test_unit _ =
        iter_examples ~f:(fun t string ~pos ->
          let len = String.length string in
          let file, fd = Unix.mkstemp "iobuf_test" in
          protect ~finally:(fun () -> Unix.unlink file) ~f:(fun () ->
            let ch = Ch.create_out fd in
            Poke.stringo t string ~pos;
            advance t pos;
            resize t ~len;
            Ch.write t ch;
            Ch.close_out ch;
            reset t;
            for pos = 0 to length t - 1 do
              Poke.char t '\000' ~pos
            done;
            let ch = Ch.create_in file in
            advance t pos;
            let lo = Lo_bound.window t in
            (match Ch.read t ch with
             | Ok  -> assert (len > 0 || Iobuf.is_empty t)
             | Eof -> assert (len = 0 && not (Iobuf.is_empty t)));
            bounded_flip_lo t lo;
            [%test_result: string] (to_string t) ~expect:string;
            reset t;
            (match Ch.read t ch with
             | Eof -> ()
             | Ok -> assert false);
            Ch.close_in ch))
      ;;
    end

    let output = output
    let input = input
    include Io_test(struct
        type in_ = In_channel.t
        let create_in file = In_channel.create file
        let close_in = In_channel.close
        type out_ = Out_channel.t
        let create_out = Unix.out_channel_of_descr
        let close_out = Out_channel.close
        let write = output
        let read = input
      end)

    let read = read
    let write = write
    include Io_test(struct
        type in_ = Unix.File_descr.t
        type out_ = in_
        let create_in file = Unix.openfile ~mode:[Unix.O_RDONLY] file
        let close_in fd = Unix.close fd
        let create_out = Fn.id
        let close_out = close_in
        let read = read
        let write = write
      end)

    let read_assume_fd_is_nonblocking  = read_assume_fd_is_nonblocking
    let write_assume_fd_is_nonblocking = write_assume_fd_is_nonblocking

    let%test_unit _ =
      iter_examples ~f:(fun t string ~pos ->
        let n = String.length string in
        let file, fd = Unix.mkstemp "iobuf_test" in
        protect ~finally:(fun () -> Unix.unlink file)
          ~f:(fun () ->
            sub_shared t ~pos |> (fun t -> Fill.stringo t string);
            sub_shared t ~pos ~len:n |> (fun t ->
              let len_before = Iobuf.length t in
              write_assume_fd_is_nonblocking t fd;
              [%test_result: int] (len_before - Iobuf.length t) ~expect:n
            );
            Unix.close fd;
            iter_examples ~f:(fun t _ ~pos ->
              if length t - pos >= String.length string then
                let fd = Unix.openfile ~mode:[Unix.O_RDONLY] file in
                sub_shared t ~pos |> (fun t ->
                  let len_before = Iobuf.length t in
                  match
                    Unix.Syscall_result.Unit.to_result
                      (read_assume_fd_is_nonblocking t fd)
                  with
                  | Ok () | Error (EAGAIN | EINTR | EWOULDBLOCK) ->
                    [%test_result: int] (len_before - Iobuf.length t)
                      ~expect:n
                  | Error e -> raise (Unix.Unix_error (e, "read", ""))
                );
                sub_shared t ~pos ~len:n |> (fun t ->
                  assert (String.equal (to_string t) string));
                Unix.close fd
            )
          )
      )
    ;;

    let pread_assume_fd_is_nonblocking = pread_assume_fd_is_nonblocking
    let pwrite_assume_fd_is_nonblocking = pwrite_assume_fd_is_nonblocking

    let%test_unit _ =
      let s = "000000000011111111112222222222" in
      let n = String.length s in
      let t = of_string s in
      let file, fd = Unix.mkstemp "iobuf_test" in
      protect ~finally:(fun () -> Unix.unlink file)
        ~f:(fun () ->
          sub_shared t ~pos:0 ~len:n |> (fun t ->
            pwrite_assume_fd_is_nonblocking t fd ~offset:10;
            assert (Iobuf.is_empty t));
          sub_shared t ~pos:0 ~len:10 |> (fun t ->
            pread_assume_fd_is_nonblocking t fd ~offset:20;
            assert (Iobuf.is_empty t));
          sub_shared t ~pos:0 ~len:10 |> (fun t ->
            assert (String.equal (to_string t) "1111111111"));
          Unix.close fd;)
    ;;

    let recvfrom_assume_fd_is_nonblocking = recvfrom_assume_fd_is_nonblocking

    module Recvmmsg_context = Recvmmsg_context
    let recvmmsg_assume_fd_is_nonblocking = recvmmsg_assume_fd_is_nonblocking

    let send_nonblocking_no_sigpipe = send_nonblocking_no_sigpipe
    let sendto_nonblocking_no_sigpipe = sendto_nonblocking_no_sigpipe

    let rec retry_until_ready thunk =
      try thunk ()
      with Unix.Unix_error ((EWOULDBLOCK | EAGAIN | EINTR), _, _) ->
        Thread.yield ();
        retry_until_ready thunk
    ;;

    let sendto_and_recvfrom recvfrom recv_fd sendto ~sendto_name =
      let port =
        match Unix.getsockname recv_fd with
        | Unix.ADDR_INET (_, port) -> port
        | _ -> assert false
      in
      let addr = Unix.(ADDR_INET (Inet_addr.localhost, port)) in
      Result.iter sendto ~f:(fun sendto ->
        let sender =
          Thread.create
            (fun () ->
               let send_fd = Unix.(socket ~domain:PF_INET ~kind:SOCK_DGRAM ~protocol:0) in
               iter_examples ~f:(fun t string ~pos:_ ->
                 Fill.stringo t string;
                 Iobuf.flip_lo t;
                 retry_until_ready
                   (fun () ->
                      Unix.Syscall_result.Unit.ok_or_unix_error_exn (sendto t send_fd addr)
                        ~syscall_name:sendto_name);
                 [%test_pred: (_, _) Iobuf.Hexdump.t] Iobuf.is_empty t
               ))
            ()
        in
        iter_examples ~f:(fun t string ~pos:_ ->
          ignore (retry_until_ready (fun () -> recvfrom t recv_fd) : Unix.sockaddr);
          Iobuf.flip_lo t;
          [%test_result: string] ~expect:string (Iobuf.to_string t)
        );
        Thread.join sender
      )
    ;;
    let sends_with_recvfrom recvfrom =
      let fd = Unix.(socket ~domain:PF_INET ~kind:SOCK_DGRAM ~protocol:0) in
      Unix.(setsockopt fd SO_REUSEADDR true);
      Unix.(bind fd ~addr:(ADDR_INET (Inet_addr.bind_any, 0)));
      Unix.set_nonblock fd;
      sendto_and_recvfrom recvfrom fd ~sendto_name:"send"
        (Or_error.map (send_nonblocking_no_sigpipe ())
           ~f:(fun send -> fun t fd addr -> Unix.connect fd ~addr; send t fd));
      sendto_and_recvfrom recvfrom fd ~sendto_name:"sendto"
        (sendto_nonblocking_no_sigpipe ())
    ;;
    let%test_unit _ = sends_with_recvfrom recvfrom_assume_fd_is_nonblocking

    let%test_unit _ =
      List.iter [ 0; 1 ]
        ~f:(fun pos ->
          let t = create ~len:10 in
          let i = 0xF234_5678_90AB_CDEFL in
          Poke.int64_t_le t ~pos i;
          assert (Peek.int64_t_le t ~pos = i);
          Poke.int64_t_be t ~pos i;
          assert (Peek.int64_t_be t ~pos = i);
          if arch_sixtyfour then
            begin
              let i = large_int 0x1234 0x5678 0x90AB 0xCDEF in
              Poke.int64_t_le t ~pos (Int64.of_int i);
              assert (Peek.int64_t_le t ~pos = Int64.of_int i);
              Poke.int64_t_be t ~pos (Int64.of_int i);
              assert (Peek.int64_t_be t ~pos = Int64.of_int i);
              Poke.int64_le t ~pos i;
              assert (Peek.int64_le t ~pos = i);
              Poke.int64_be t ~pos i;
              assert (Peek.int64_be t ~pos = i)
            end;
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

    let%test_unit _ =
      let t = create ~len:1024 in
      Fill.int8 t 12;
      Fill.int16_le t 1234;
      Fill.int32_le t 345678;
      Fill.char t 'x';
      flip_lo t;
      assert (length t = 8);
      assert (Consume.int8 t = 12);
      assert (Consume.int16_le t = 1234);
      assert (Consume.int32_le t = 345678);
      assert (Consume.char t = 'x')
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
          Bigstring.From_string.blito ~src:s ~dst:bstr ~dst_pos:int_size ();
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
          Fill.stringo t s;
          flip_lo t;
          write_assume_fd_is_nonblocking t fd;
          [%test_pred: (_, _) t] Iobuf.is_empty t; (* no short writes *)
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
        let msg = Consume.stringo r in
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
            check_message (sub_shared t ~len:needed);
            advance t needed;
            drain_messages ();
          end
      in
      let rec loop_file () =
        let len_before = length t in
        begin
          match
            read_assume_fd_is_nonblocking t fd
            |> Unix.Syscall_result.Unit.to_result (* doesn't allocate *)
          with
          | Error (EAGAIN | EINTR | EWOULDBLOCK) -> ()
          | Error e -> raise (Unix.Unix_error (e, "read", ""))
          | Ok () -> ()
        end;
        if len_before > length t then (
          flip_lo t;
          drain_messages ();
          compact t;
          loop_file ()
        )
      in
      loop_file ();
      Unix.close fd;
      !msg_number
    ;;

    let%test_unit _ =
      let msgcount = 10_000 in
      List.iter [2; 4; 8] ~f:(fun int_size -> List.iter [false; true] ~f:(fun be ->
        let filename = create_sample_file ~int_size ~be ~msgcount in
        protect ~f:(fun () -> assert (check_msgs  ~int_size ~be filename = msgcount))
          ~finally:(fun () -> Unix.unlink               filename)))
    ;;
  end

  include Accessors (struct
      module Consume = Consume
      module Fill    = Fill
      module Peek    = Peek
      module Poke    = Poke

      let is_safe = true
    end)

  module Unsafe = Accessors (struct include Unsafe let is_safe = false end)
  (* The signature here is to remind us to add a unit test whenever we add a function to
     [Iobuf]. *)
end : Iobuf)

let%test_module _ = (module Test (Iobuf_debug.Make ()))

(* Ensure against bugs in [Iobuf_debug].  The above tests [Iobuf_debug], with invariants
   checked, and this tests the straight [Iobuf] module. *)
let%test_module _ = (module Test (struct
    include Iobuf
    let show_messages = ref true
  end))

let%test_module "allocation" =
  (module struct
    open Expect_test_helpers

    let%expect_test "set_bounds_and_buffer_sub" =
      let src = Iobuf.of_string "123abcDEF" in
      let dst = Iobuf.create ~len:0 in
      require_no_allocation [%here] (fun () ->
        Iobuf.set_bounds_and_buffer_sub ~src ~dst ~len:(Iobuf.length src) ~pos:0);
      [%expect {| |}]

    let%expect_test "Fill.string" =
      let str = "123" in
      (* The labeled arguments mustn't be constant or they don't test anything - optional
         constant arguments can always be preallocated. *)
      let str_pos = Random.int (String.length str) in
      let len = Random.int (String.length str - str_pos) in
      let dst = Iobuf.create ~len in
      require_no_allocation [%here] (fun () -> Iobuf.Fill.string dst str ~str_pos ~len);
      [%expect {| |}]
  end)

let%test_unit "SEGV bug repro" =
  let tmp = Iobuf.create ~len:10000000 in
  Iobuf.advance tmp (Iobuf.length tmp - 5);
  Iobuf.protect_window_and_bounds tmp ~f:(fun tmp ->
    let tmp2 = Iobuf.create ~len:5 in
    Iobuf.set_bounds_and_buffer ~src:tmp2 ~dst:tmp);
  ignore (Iobuf.Consume.To_bytes.subo (Iobuf.read_only tmp) : Bytes.t)
