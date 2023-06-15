open! Import
include Bin_prot

module Writer = struct
  type 'a t = 'a Bin_prot.Type_class.writer =
    { size : 'a Size.sizer
    ; write : 'a Write.writer
    }

  let to_bigstring t v =
    let len = t.size v in
    let buf = Bigstring.create len in
    let pos = t.write buf ~pos:0 v in
    assert (pos = Bigstring.length buf);
    buf
  ;;

  let to_string t v =
    let buf = to_bigstring t v in
    let str = Bigstring.to_string buf in
    Bigstring.unsafe_destroy buf;
    str
  ;;

  let to_bytes t v =
    let buf = to_bigstring t v in
    let str = Bigstring.to_bytes buf in
    Bigstring.unsafe_destroy buf;
    str
  ;;
end

module Reader = struct
  type 'a t = 'a Bin_prot.Type_class.reader =
    { read : 'a Read.reader
    ; vtag_read : (int -> 'a) Read.reader
    }

  let of_bigstring t buf =
    let pos_ref = ref 0 in
    let v = t.read buf ~pos_ref in
    assert (!pos_ref = Bigstring.length buf);
    v
  ;;

  let of_bigstring_unsafe_destroy t buf =
    let v = of_bigstring t buf in
    Bigstring.unsafe_destroy buf;
    v
  ;;

  let of_string t string = Bigstring.of_string string |> of_bigstring_unsafe_destroy t
  let of_bytes t bytes = Bigstring.of_bytes bytes |> of_bigstring_unsafe_destroy t
end
