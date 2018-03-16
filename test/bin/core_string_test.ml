open Core
open OUnit;;
module S = String

let str1 = "1234567890"

let test =
  "core_string" >:::
  [ "slice" >::
    (fun () ->
       "all" @? (S.slice str1 0 0 = str1);
       "ordinary" @? (S.slice str1 1 3= "23");
       "neg1" @? (S.slice str1 0 (-1) = "123456789");
       "neg2" @? (S.slice str1 (-1) 0 = "0");
       "neg3" @? (S.slice str1 (-5) (-4) = "6";)
    );
    "nget" >::
    (fun () ->
       "neg" @? (S.nget str1 (-3) = '8');
       "pos" @? (S.nget str1 3 = str1.[3]);
       "invalid" @?
       (try ignore (S.nget str1 (-100)); false
        with Invalid_argument _ -> true | _ -> false)
    );
    "lsplit2_exn" >::
    (fun () ->
       "none" @? (try ignore (S.lsplit2_exn str1 ~on:' '); false
                  with Not_found_s _ | Caml.Not_found -> true | _ -> false);
       "some1" @? (S.lsplit2_exn str1 ~on:'5' = ("1234","67890"));
       "some2" @? (S.lsplit2_exn "123.456.789" ~on:'.' = ("123","456.789"));
    );
    "lsplit2" >::
    (fun () ->
       "none" @? (S.lsplit2 str1 ~on:' ' = None);
       "some1" @? (S.lsplit2 str1 ~on:'5' = Some ("1234","67890"));
       "some2" @? (S.lsplit2 "123.456.789" ~on:'.' = Some ("123","456.789"));
    );
    "rsplit2_exn" >::
    (fun () ->
       "none" @? (try ignore (S.rsplit2_exn str1 ~on:' '); false
                  with Not_found_s _ | Caml.Not_found -> true | _ -> false);
       "some1" @? (S.rsplit2_exn str1 ~on:'5' = ("1234","67890"));
       "some2" @? (S.rsplit2_exn "123.456.789" ~on:'.' = ("123.456","789"));
    );
    "rsplit2" >::
    (fun () ->
       "none" @? (S.rsplit2 str1 ~on:' ' = None);
       "some1" @? (S.rsplit2 str1 ~on:'5' = Some ("1234","67890"));
       "some2" @? (S.rsplit2 "123.456.789" ~on:'.' = Some ("123.456","789"));
    );
    "strip" >::
    (fun () ->
       "both ends" @? (S.strip "  123  " = "123");
       "all white" @? (S.strip "\n\t \n" = "");
       "no white" @? (S.strip "as \t\ndf" = "as \t\ndf");
       "just left" @? (S.strip " a" = "a");
       "just right" @? (S.strip "a " = "a");
    );
    "lstrip" >::
    (fun () ->
       "left" @? (S.lstrip " \t\r\n123  \t\n" = "123  \t\n");
       "all white" @? (S.lstrip " \t \n\n\r " = "");
       "no white on the left" @? (S.lstrip "foo Bar \n " = "foo Bar \n ");
    );
    "rstrip" >::
    (fun () ->
       "right" @? (S.rstrip " \t\r\n123  \t\n\r" = " \t\r\n123");
       "all white" @? (S.rstrip " \t \n\n\r " = "");
       "no white on the right" @? (S.rstrip " \n foo Bar" = " \n foo Bar");
    );
    "map" >::
    (fun () ->
       "empty" @? (S.map ~f:(fun x -> x) "" = "");
       "1" @? (S.map ~f:(function 'a' -> 'b' | 'b' -> 'a' | x -> x)
         "faboo" = "fbaoo");
    );
    "split" >::
    (fun () ->
       "empty" @? (S.split "" ~on:'c' = [""]);
       "1" @? (S.split "c" ~on:'c' = ["";""]);
       "end" @? (S.split "fooc" ~on:'c' = ["foo";""]);
       "begin" @? (S.split "cfoo" ~on:'c' = ["";"foo"]);
       "beginend" @? (S.split "cfooc" ~on:'c' = ["";"foo";""]);
       "consecutive_delims" @? (S.split "bocci ball" ~on:'c'
                                = ["bo"; ""; "i ball"]);
    );
    "split_on_chars" >::
    (fun () ->
       "empty" @? (S.split_on_chars "" ~on:['c'] = [""]);
       "1" @? (S.split_on_chars "c" ~on:['c'] = ["";""]);
       "1-grouped" @? (S.split_on_chars "chr" ~on:['h';'c';'r'] = ["";"";"";""]);
       "end" @? (S.split_on_chars "fooc" ~on:['c'] = ["foo";""]);
       "end-grouped" @? (S.split_on_chars "fooc" ~on:['c';'o'] = ["f";"";"";""]);
       "begin" @? (S.split_on_chars "cfoo" ~on:['c'] = ["";"foo"]);
       "begin-grouped" @? (S.split_on_chars "cfoo" ~on:['c';'f'] = ["";"";"oo"]);
       "consecutive_delims" @? (S.split_on_chars "bocci ball" ~on:['c']
                                = ["bo"; ""; "i ball"]);
       "consecutive_delims-grouped" @?
       (S.split_on_chars "bocci ball" ~on:['c';' ';'i'] =
        ["bo"; ""; ""; ""; "ball"]);
    );
    "fold" >::
    (fun () ->
       let to_list s = S.fold ~f:(fun acc c -> c::acc) ~init:[] s in
       "empty" @? (to_list "" = []);
       "singleton" @? (to_list "H" = ['H']);
       "simple" @? (to_list "Hello" = ['o';'l';'l';'e';'H']);
    );
    "is_suffix" >::
    (fun () ->
       "empty" @? (S.is_suffix "" ~suffix:"a" = false);
       "empty_empty_suffix" @? (S.is_suffix "" ~suffix:"" = true);
       "simple_empty_suffix" @? (S.is_suffix "Foo" ~suffix:"" = true);
       "singleton" @? (S.is_suffix "H" ~suffix:"H" = true);
       "simple" @? (S.is_suffix "Hello" ~suffix:"lo" = true);
       "simplefalse" @? (S.is_suffix "HelloFoo" ~suffix:"lo" = false);
    );
    "is_prefix" >::
    (fun () ->
       "empty" @? (S.is_prefix "" ~prefix:"a" = false);
       "empty_empty_prefix" @? (S.is_prefix "" ~prefix:"" = true);
       "simple_empty_prefix" @? (S.is_prefix "Foo" ~prefix:"" = true);
       "singleton" @? (S.is_prefix "H" ~prefix:"H" = true);
       "simple" @? (S.is_prefix "Hello" ~prefix:"He" = true);
       "simplefalse" @? (S.is_prefix "HelloFoo" ~prefix:"lo" = false);
    );
    "concat_array" >::
    (fun () ->
       "empty" @? (S.concat_array ~sep:":" [||] = "");
       "empty singleton" @? (S.concat_array ~sep:":" [|""|] = "");
       "singleton" @? (S.concat_array ~sep:":" [|"Hello"|] = "Hello");
       "Words" @? (S.concat_array ~sep:" " [|"Hello"; "World"; "!"|] = "Hello World !");
    );
    "suffix" >::
    (fun () ->
       let base = "0123456789" in
       let test len res =
         (sprintf "%d" len) @? (S.suffix base len = res)
       in
       test 0 "";
       test 1 "9";
       test 2 "89";
       test 10 base;
       test 20 base;
       assert_raises ~msg:"-1"
         (Invalid_argument "suffix expecting nonnegative argument")
         (fun () -> test (-1) "");
    );
    "prefix" >::
    (fun () ->
       let base = "0123456789" in
       let test len res =
         (sprintf "%d" len) @? (S.prefix base len = res)
       in
       test 0 "";
       test 1 "0";
       test 2 "01";
       test 10 base;
       test 20 base;
       assert_raises ~msg:"-1"
         (Invalid_argument "prefix expecting nonnegative argument")
         (fun () -> test (-1) "");
    );
    "drop_suffix" >::
    (fun () ->
       let base = "0123456789" in
       let test len res =
         (sprintf "%d" len) @? (S.drop_suffix base len = res)
       in
       test 0 base;
       test 1 "012345678";
       test 2 "01234567";
       test 10 "";
       test 20 "";
       assert_raises ~msg:"-1"
         (Invalid_argument "drop_suffix expecting nonnegative argument")
         (fun () -> test (-1) "");
    );
    "drop_prefix" >::
    (fun () ->
       let base = "0123456789" in
       let test len res =
         (sprintf "%d" len) @? (S.drop_prefix base len = res)
       in
       test 0 base;
       test 1 "123456789";
       test 2 "23456789";
       test 10 "";
       test 20 "";
       assert_raises ~msg:"-1"
         (Invalid_argument "drop_prefix expecting nonnegative argument")
         (fun () -> test (-1) "");
    );
    "chop_suffix_exn" >::
    (fun () ->
       "simple" @? (S.chop_suffix_exn str1 ~suffix:"7890" = "123456");
       "end" @? (S.chop_suffix_exn str1 ~suffix:"" = "1234567890");
       assert_raises
         ~msg:"not a suffix"
         (Invalid_argument "String.chop_suffix_exn \"1234567890\" \"abc\"")
         (fun () -> S.chop_suffix_exn str1 ~suffix:"abc")
    );
    "chop_prefix_exn" >::
    (fun () ->
       "simple" @? (S.chop_prefix_exn str1 ~prefix:"123" = "4567890");
       "end" @? (S.chop_prefix_exn str1 ~prefix:"" = "1234567890");
       assert_raises
         ~msg:"not a prefix"
         (Invalid_argument "String.chop_prefix_exn \"1234567890\" \"abc\"")
         (fun () -> S.chop_prefix_exn str1 ~prefix:"abc")
    );
    "chop_suffix" >::
    (fun () ->
       "simple" @? (S.chop_suffix str1 ~suffix:"7890" = Some "123456");
       "end" @? (S.chop_suffix str1 ~suffix:"" = Some "1234567890");
       "not a suffix" @? (S.chop_suffix str1 ~suffix:"abc" = None)
    );
    "chop_prefix" >::
    (fun () ->
       "simple" @? (S.chop_prefix str1 ~prefix:"123" = Some "4567890");
       "end" @? (S.chop_prefix str1 ~prefix:"" = Some "1234567890");
       "not a prefix" @? (S.chop_prefix str1 ~prefix:"abc" = None)
    );
    "to_list_and_to_list_rev" >::
    (fun () ->
       let seaweed = "bladderwrack" in
       assert (S.to_list seaweed
               = ['b';'l';'a';'d';'d';'e';'r';'w';'r';'a';'c';'k']);
       assert (S.to_list_rev seaweed
               = ['k';'c';'a';'r';'w';'r';'e';'d';'d';'a';'l';'b']);
       let empty = "" in
       assert (S.to_list empty = []);
       assert (S.to_list_rev empty = [])
    );
    ("mem" >::: [
       ("default" >:: fun () ->
          let test t c b =
            (sprintf "%c in %s = %b" c t b) @? (S.mem t c = b)
          in
          (let t = "abc" in
           test t 'a' true;
           test t 'b' true;
           test t 'c' true;
           test t 'x' false);
          (let t = "ab" in
           test t 'a' true;
           test t 'b' true;
           test t 'x' false);
          (let t = "a" in
           test t 'a' true;
           test t 'x' false);
          (let t = "" in
           test t 'x' false);
       );
     ]);
  ]
