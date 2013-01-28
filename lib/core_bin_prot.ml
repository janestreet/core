include Bin_prot

module Writer = struct
  type 'a t = 'a Bin_prot.Type_class.writer
    = { size : 'a Size.sizer;
        write : 'a Write_ml.writer;
        unsafe_write : 'a Unsafe_write_c.writer;
      }

  let to_string t v =
    let len = t.size v in
    let buf = Bigstring.create len in
    let pos = t.write buf ~pos:0 v in
    assert (pos = Bigstring.length buf);
    Bigstring.to_string buf
  ;;
end
