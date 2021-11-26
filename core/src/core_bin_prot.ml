open! Import
include Bin_prot

module Writer = struct
  type 'a t = 'a Bin_prot.Type_class.writer =
    { size : 'a Size.sizer
    ; write : 'a Write.writer
    }

  let to_string t v =
    let len = t.size v in
    let buf = Bigstring.create len in
    let pos = t.write buf ~pos:0 v in
    assert (pos = Bigstring.length buf);
    let str = Bigstring.to_string buf in
    Bigstring.unsafe_destroy buf;
    str
  ;;

  let to_bytes t v =
    let len = t.size v in
    let buf = Bigstring.create len in
    let pos = t.write buf ~pos:0 v in
    assert (pos = Bigstring.length buf);
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

  let of_string t string =
    let buf = Bigstring.of_string string in
    let pos_ref = ref 0 in
    let v = t.read buf ~pos_ref in
    assert (!pos_ref = Bigstring.length buf);
    Bigstring.unsafe_destroy buf;
    v
  ;;

  let of_bytes t bytes =
    let buf = Bigstring.of_bytes bytes in
    let pos_ref = ref 0 in
    let v = t.read buf ~pos_ref in
    assert (!pos_ref = Bigstring.length buf);
    Bigstring.unsafe_destroy buf;
    v
  ;;
end
