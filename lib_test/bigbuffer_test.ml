open Unix
open OUnit;;
open Core.Std
open Bigstring_test

let io_test ~n =
  fdpair_test ~n socketpair
    (fun buf fd ->
      let oc = out_channel_of_descr fd in
      Bigbuffer.output_buffer oc buf;
      flush oc)
    (fun ~n:_ orig_buf fd ->
      let ic = in_channel_of_descr fd in
      let buf = Bigbuffer.create 0 in
      Bigbuffer.add_channel buf ic (Bigbuffer.length orig_buf);
      "channel" @? (Bigbuffer.contents orig_buf = Bigbuffer.contents buf))

let test =
  "Bigbuffer" >:::
    [
      "adding/extracting data" >::
        (fun () ->
          let buf = Bigbuffer.create 100 in
          Bigbuffer.add_char buf 'x';
          Bigbuffer.add_char buf 'y';
          Bigbuffer.add_string buf "asdf";
          Bigbuffer.add_substring buf "fdsa" 1 2;
          Bigbuffer.add_buffer buf buf;
          let str = "xyasdfds" in
          "contents" @? (Bigbuffer.contents buf = str ^ str);
          "big_contents" @?
            (Bigstring.to_string (Bigbuffer.big_contents buf) = str ^ str );
          "sub" @? (Bigbuffer.sub buf 5 5 = "fdsxy");
          io_test ~n:"" buf
        );

    ]
