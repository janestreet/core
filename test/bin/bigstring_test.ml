open Core;;
open OUnit;;
open Quickcheck_deprecated;;

(***************************************************************************)
(** Simple bigstring testing utilties *)

(** bigstring generator *)
let bsg ?(size=nng) ?(char=cg) () =
  let len = size () in
  let bs = Bigstring.create len in
  for i = 0 to len - 1 do
    bs.{i} <- char ()
  done;
  bs
;;

let png () = nng () + 1

let bs_of_s = Bigstring.of_string

(** function for getting a short representation of a bigstring *)
let repr bs =
  if Bigstring.length bs > 30 then
    let s = Bytes.create 30 in
    Bigstring.To_bytes.blito ~src:bs ~src_len:30 ~dst:s ();
    sprintf "<bs:%d:%s>" (Bigstring.length bs) (Bytes.to_string s)
  else
    sprintf "<bs:%s>" (Bigstring.to_string bs)
;;

(***************************************************************************)

(** suport code for individual tests *)

let blit_test ~n ~src_pos ~dst_pos ~len (s1,s2) =
  let s1_orig = s1 and s2_orig = s2 in
  let s1 = Bytes.of_string s1 and s2 = Bytes.of_string s2 in
  let s_result =
    try
      Bytes.blit ~src_pos ~dst_pos ~len ~src:s1 ~dst:s2;
      `Success (Bytes.to_string s2)
    with
      e -> `Failure (Exn.to_string e)
  in
  let bs_result =
    try
      let bs1 = Bigstring.of_bytes s1 in
      let bs2 = Bigstring.of_bytes s2 in
      Bigstring.blito ~src:bs1 ~src_pos ~src_len:len ~dst:bs2 ~dst_pos ();
      `Success (Bigstring.to_string bs2)
    with
      e -> `Failure (Exn.to_string e)
  in
  let prefix = sprintf "blit %s: %s,%s - " n s1_orig s2_orig in
  match s_result, bs_result with
  | `Success rval, `Success rval' ->
    (prefix ^ "success") @? (rval = rval')
  | `Success _, `Failure err ->
    assert_failure (prefix ^ "string worked, bigstring failed: " ^ err)
  | `Failure err, `Success _ ->
    assert_failure (prefix ^ "bigstring worked, string failed: " ^ err)
  | `Failure _, `Failure _ ->
    ()
;;

(** takes a string as an argument, and blits it back and forth to a newly created
    bigstring *)
let simple_conversion_test ~n s =
  let len = String.length s in
  let bs = Bigstring.create len in
  Bigstring.From_string.blito ~src:s ~dst:bs ();
  let s' = Bytes.create len in
  Bigstring.To_bytes.blito ~src:bs ~dst:s' ();
  (sprintf "%s: %s" n s) @? (Bytes.to_string s' = s)
;;

let really_output outc bs =
  let pos = ref 0 in
  let len = Bigstring.length bs in
  while !pos < len do
    let bytes = Bigstring.output ~pos:!pos outc bs in
    pos := !pos + bytes
  done

(** takes a bigstring, writes it to a file, then opens it as an inchannel and passes it to
    the test function *)
let inchan_test ~n test orig =
  let (fname,outc) = Filename.open_temp_file "bigstring_test" ".txt" in
  protect ~f:(fun () ->
    really_output outc orig;
    Out_channel.close outc;
    let inc = In_channel.create fname in
    test ~n orig inc)
    ~finally:(fun () -> Unix.unlink fname)
;;

(** like inchan test, but it passes a file descriptor to the test function *)
let fd_test ~n test s =
  inchan_test ~n
    (fun ~n orig inc -> test ~n orig (Unix.descr_of_in_channel inc))
    s
;;

let really_read_test ~n bs fd =
  let len = Bigstring.length bs in
  let bs' = Bigstring.create len in
  Bigstring.really_read fd ~pos:0 ~len bs';
  (sprintf "%s: %s" n (repr bs)) @? (bs = bs')
;;

let socketpair () =
  Unix.socketpair ~domain:Unix.PF_UNIX ~kind:Unix.SOCK_STREAM ~protocol:0

let fdpair_test ~n fdpair sender receiver bs =
  try
    let (read,write) = fdpair () in
    let sth = Thread.create
                (fun () ->
                   try sender bs write
                   with e -> eprintf "ERROR: %s" (Exn.to_string e))
                ()
    in
    receiver ~n bs read;
    Thread.join sth;
    Unix.close read;
    Unix.close write
  with
    e -> assert_failure (sprintf "%s: receive exception: %s" n (Exn.to_string e))


let write_read_test ~n fdpair bs =
  fdpair_test ~n fdpair
    (fun bs fd ->
       Bigstring.really_write fd bs;
    )
    (fun ~n bs fd ->
       let bs' = Bigstring.create (Bigstring.length bs) in
       Bigstring.really_read fd bs';
       (sprintf "send/recv %s: %s,%s" n (repr bs) (repr bs')) @? (bs = bs'))
    bs

let output_input_test ?(runs = 2) ~n fdpair bs =
  let ic = ref In_channel.stdin in
  let oc = ref Out_channel.stdout in
  fdpair_test ~n fdpair
    (fun bs fd ->
       if !oc = stdout then oc := Unix.out_channel_of_descr fd;
       for _ = 1 to runs do
         Bigstring.really_output !oc bs
       done;
       Out_channel.flush !oc
    )
    (fun ~n bs fd ->
       if !ic = In_channel.stdin then ic := Unix.in_channel_of_descr fd;
       let bs' = Bigstring.create (Bigstring.length bs) in
       for _ = 1 to runs do
         Bigstring.really_input !ic bs'
       done;
       (sprintf "output/input %s: %s,%s" n (repr bs) (repr bs')) @? (bs = bs'))
    bs

let test =
  "bigstring" >:::
  ["simple conversion" >::
   (fun () ->
      simple_conversion_test ~n:"empty" "";
      simple_conversion_test ~n:"simple" "0123434aslekX";
      simple_conversion_test ~n:"single" "1";
      repeat 50 (simple_conversion_test ~n:"random") sg;
   );

   "input" >::
   (fun () ->
      fd_test really_read_test  ~n:"single" (bs_of_s "X");
      fd_test really_read_test  ~n:"simple" (bs_of_s "normal length string");
      repeat 100 (fd_test really_read_test ~n:"random") (bsg ~size:png);
      repeat 100 (fd_test really_read_test ~n:"random big")
        (bsg ~size:(fun () -> 100 * png ()));
   );

   "destruction" >::
   (fun () ->
      let n = 100 in
      let bstr = Bigstring.create n in
      bstr.{0} <- 'x';
      "initial size" @? (Bigstring.length bstr = n);
      "initial access" @? (bstr.{0} = 'x');
      Bigstring.unsafe_destroy bstr;
      "destroyed size" @? (Bigstring.length bstr = 0);
      "destroyed access" @? begin
        try ignore (bstr.{0} = 'x'); false
        with Invalid_argument "index out of bounds" -> true
      end;
      "double destroy" @? begin
        try Bigstring.unsafe_destroy bstr; false
        with Failure _ -> true
      end;
   );

   "blit" >::
   (fun () ->
      blit_test ~n:"empty" ~src_pos:0 ~dst_pos:0 ~len:0 ("","");
      blit_test ~n:"simple" ~src_pos:0 ~dst_pos:0 ~len:5 ("01234","     ");
      blit_test ~n:"shortdst" ~src_pos:0 ~dst_pos:0 ~len:5 ("01234","    ");
      blit_test ~n:"shortsrc" ~src_pos:0 ~dst_pos:0 ~len:5 ("0234","     ");
      blit_test ~n:"middle" ~src_pos:5 ~dst_pos:0 ~len:5 ("1234554321","     ");
      repeat 5000 (fun (s1,s2,src_pos,dst_pos,len) ->
        blit_test ~n:"random" ~src_pos ~dst_pos ~len (s1,s2))
        (fun () -> (sg (), sg(),nng (), nng (), nng ()))
   );
   "really write/read pipe" >::
   (fun () ->
      let write_read_test = write_read_test Unix.pipe in
      (* write_read_test ~n:"empty" (bs_of_s ""); *)
      write_read_test ~n:"simple" (bs_of_s "A simple short string");
      repeat 500 (write_read_test ~n:"random") (bsg ~size:png);
      repeat 500 (write_read_test ~n:"random big")
        (bsg ~size:(fun () -> 100 * png ()));
   );

   "really write/read socketpair" >::
   (fun () ->
      let write_read_test = write_read_test socketpair in
      (* write_read_test ~n:"empty" (bs_of_s ""); *)
      write_read_test ~n:"simple" (bs_of_s "A simple short string");
      repeat 500 (write_read_test ~n:"random") (bsg ~size:png);
      repeat 500 (write_read_test ~n:"random big")
        (bsg ~size:(fun () -> 100 * png ()));
   );

   "output/input socketpair" >::
   (fun () ->
      let output_input_test ?runs = output_input_test ?runs socketpair in
      (* output_input_test ~n:"empty" (bs_of_s ""); *)
      repeat 5000 (output_input_test ~runs:100 ~n:"simple")
        (fun () -> bs_of_s "A simple short string");
      repeat 500 (output_input_test ~n:"random") (bsg ~size:png);
      repeat 500 (output_input_test ~n:"random big")
        (bsg ~size:(fun () -> 100 * png ()));
   );

   "sub" >::
   (fun () ->
      let original = Bigstring.of_string "catfish" in
      match Bigstring.to_string (Bigstring.subo ~pos:3 original) with
      | "fish" -> ()
      | other -> failwithf "Expected fish, got: %s" other ());

   "sub_shared" >::
   (fun () ->
      let original = Bigstring.of_string "catfish" in
      match Bigstring.to_string (Bigstring.sub_shared ~pos:3 original) with
      | "fish" -> ()
      | other -> failwithf "Expected fish, got: %s" other ());
  ]
