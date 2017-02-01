open! Import

external crc32 : string -> Int63.t = "core_crc_string_crc32"
external unsafe_bigstring_crc32 : Bigstring.t -> pos:int -> len:int -> Int63.t
  = "core_crc_bigstring_crc32"

let bigstring_crc32 bstr ~pos ~len =
  let length = Bigstring.length bstr in
  Ordered_collection_common.check_pos_len_exn ~pos ~len ~length;
  unsafe_bigstring_crc32 bstr ~pos ~len

let crc32hex s = Printf.sprintf "%08LX" (Int63.to_int64 (crc32 s))

let%test_module _ =
  (module struct

    let str = "The quick brown fox jumps over the lazy dog"
    let len = String.length str
    let crc = Int63.of_int64_exn 0x414fa339L

    let bstr = Bigstring.of_string str

    let%test_unit _ =
      [%test_result: Int63.Hex.t]
        (crc32 str)
        ~expect:crc

    let%test_unit _ =
      [%test_result: Int63.Hex.t]
        (bigstring_crc32 bstr ~pos:0 ~len)
        ~expect:crc

    let%test_unit _ =
      [%test_result: Int63.Hex.t]
        (bigstring_crc32 (Bigstring.of_string ("12345" ^ str ^ "12345")) ~pos:5 ~len)
        ~expect:crc

    let%test _ = does_raise (fun () -> bigstring_crc32 bstr ~pos:0       ~len:(-1))
    let%test _ = does_raise (fun () -> bigstring_crc32 bstr ~pos:0       ~len:(len+1))
    let%test _ = does_raise (fun () -> bigstring_crc32 bstr ~pos:(-1)    ~len:0)
    let%test _ = does_raise (fun () -> bigstring_crc32 bstr ~pos:(len+1) ~len:0)

  end)
