open! Core
open! Expect_test_helpers_kernel

module Inet_addr = Unix.Inet_addr
module Cidr      = Unix.Cidr

open Cidr

let does_match ?(expected = true) cidr inet_addr =
  let matches = does_match cidr inet_addr in
  require [%here] (Bool.equal expected matches)
    ~if_false_then_print_s:
      (lazy [%message
        "[Cidr.does_match] produced unexpected result"
          (cidr      : t)
          (inet_addr : Inet_addr.t)
          (matches   : bool)
          (expected  : bool)])
;;

let match_strings ?expected c a =
  does_match ?expected (of_string c) (Inet_addr.of_string a)
;;

let is_multicast ?expected a =
  does_match ?expected multicast (Inet_addr.of_string a)
;;

let invariant ?(expected = Ok ()) string =
  match Or_error.try_with (fun () -> invariant (of_string string)), expected with
  | Ok _, Ok _ | Error _, Error _ -> ()
  | Ok _, Error _ ->
    print_cr [%here] [%message
      "[of_string] + [invariant] succeeded unexpectedly"
        (string : string)]
  | Error error, Ok _ ->
    print_cr [%here] [%message
      "[of_string] + [invariant] failed unexpectedly"
        (string : string)
        (error  : Error.t)]
;;

let test_all_matching_addresses cidr_string expected_strings =
  let cidr = of_string cidr_string in
  let addresses = all_matching_addresses cidr |> Sequence.to_list in
  let expected = List.map expected_strings ~f:Inet_addr.of_string in
  require [%here]
    (List.equal ~equal:Inet_addr.equal addresses expected)
    ~if_false_then_print_s:
      (lazy [%message
        "[all_matching_addresses] gave unexpected result"
          (cidr      : Cidr.t)
          (addresses : Inet_addr.t list)
          (expected  : Inet_addr.t list)])
;;

let same str1 str2 =
  require [%here] (equal (of_string str1) (of_string str2))
    ~if_false_then_print_s:(lazy [%message "should be equal"
                                             (str1 : string)
                                             (str2 : string)])
;;

let diff str1 str2 =
  require [%here] (not (equal (of_string str1) (of_string str2)))
    ~if_false_then_print_s:(lazy [%message "should NOT be equal"
                                             (str1 : string)
                                             (str2 : string)])
;;

(* Can we parse some random correct netmasks? *)
let%expect_test _ = invariant "10.0.0.0/8"
let%expect_test _ = invariant "172.16.0.0/12"
let%expect_test _ = invariant "172.25.42.0/18"
let%expect_test _ = invariant "192.168.0.0/16"
let%expect_test _ = invariant "192.168.13.0/24"

(* Do we properly fail on some nonsense? *)
let%expect_test _ = invariant ~expected:(Error ()) "172.25.42.0"
let%expect_test _ = invariant ~expected:(Error ()) "172.25.42.0/35"
let%expect_test _ = invariant ~expected:(Error ()) "172.25.42.0/sandwich"
let%expect_test _ = invariant ~expected:(Error ()) "sandwich/sandwich"
let%expect_test _ = invariant ~expected:(Error ()) "sandwich/39"
let%expect_test _ = invariant ~expected:(Error ()) "sandwich/16"
let%expect_test _ = invariant ~expected:(Error ()) "sandwich"
let%expect_test _ = invariant ~expected:(Error ()) "172.52.43/16"
let%expect_test _ = invariant ~expected:(Error ()) "172.52.493/16"

(* Basic match tests *)
let%expect_test _ = match_strings "127.0.0.1/32" (Inet_addr.to_string Inet_addr.localhost)
let%expect_test _ = match_strings "127.0.0.0/8"  (Inet_addr.to_string Inet_addr.localhost)
let%expect_test _ = match_strings "0.0.0.0/32"   (Inet_addr.to_string Inet_addr.bind_any)
let%expect_test _ = match_strings "0.0.0.0/0"    (Inet_addr.to_string Inet_addr.bind_any)

let%expect_test _ = match_strings "10.0.0.0/8"     "10.0.0.1"
let%expect_test _ = match_strings "10.0.0.0/8"     "10.34.67.1"
let%expect_test _ = match_strings "10.0.0.0/8"     "10.255.255.255"
let%expect_test _ = match_strings "172.16.0.0/12"  "172.16.0.0"
let%expect_test _ = match_strings "172.16.0.0/12"  "172.31.255.254"
let%expect_test _ = match_strings "172.25.42.0/24" "172.25.42.1"
let%expect_test _ = match_strings "172.25.42.0/24" "172.25.42.255"
let%expect_test _ = match_strings "172.25.42.0/24" "172.25.42.0"
let%expect_test _ = match_strings "172.25.42.0/16" "172.25.0.1"
let%expect_test _ = match_strings "172.25.42.0/16" "172.25.255.254"
let%expect_test _ = match_strings "172.25.42.0/16" "172.25.42.1"
let%expect_test _ = match_strings "172.25.42.0/16" "172.25.105.237"

(* And some that should fail *)
let%expect_test _ = match_strings "10.0.0.0/8"     "9.255.255.255"  ~expected:false
let%expect_test _ = match_strings "10.0.0.0/8"     "11.0.0.1"       ~expected:false
let%expect_test _ = match_strings "172.16.0.0/12"  "172.15.255.255" ~expected:false
let%expect_test _ = match_strings "172.25.42.0/24" "172.26.42.47"   ~expected:false
let%expect_test _ = match_strings "172.25.42.0/24" "172.26.42.208"  ~expected:false

(* Subset tests *)
let is_subset_strings ?(expected=true) t ~of_ =
  let is_subset = is_subset (of_string t) ~of_:(of_string of_) in
  require [%here] (Bool.equal expected is_subset)
    ~if_false_then_print_s:
      (lazy [%message
        "[Cidr.is_subset] produced unexpected result"
          t
          (of_ : string)
          (is_subset : bool)
          (expected  : bool)])
;;

let%expect_test _ = is_subset_strings "10.0.0.0/8"    ~of_:"0.0.0.0/0"
let%expect_test _ = is_subset_strings "10.0.0.0/8"    ~of_:"10.0.0.0/8"
let%expect_test _ = is_subset_strings "10.1.2.0/24"   ~of_:"10.0.0.0/8"
let%expect_test _ = is_subset_strings "10.1.2.0/24"   ~of_:"10.1.0.0/16"
let%expect_test _ = is_subset_strings "10.1.2.123/32" ~of_:"10.1.2.0/1"
let%expect_test _ = is_subset_strings "10.1.2.123/32" ~of_:"10.1.2.0/7"
let%expect_test _ = is_subset_strings "10.1.2.123/32" ~of_:"10.1.2.0/14"
let%expect_test _ = is_subset_strings "10.1.2.123/32" ~of_:"10.1.2.0/24"

let%expect_test _ = is_subset_strings "10.0.0.0/8"    ~of_:"5.0.0.0/8"     ~expected:false
let%expect_test _ = is_subset_strings "10.1.2.0/24"   ~of_:"11.0.0.0/8"    ~expected:false
let%expect_test _ = is_subset_strings "10.1.2.0/24"   ~of_:"10.0.0.0/16"   ~expected:false
let%expect_test _ = is_subset_strings "10.1.2.123/32" ~of_:"10.1.2.124/32" ~expected:false
let%expect_test _ = is_subset_strings "10.0.0.0/8"    ~of_:"10.0.0.0/9"    ~expected:false

(* Multicast tests *)
let%expect_test _ = is_multicast "0.0.0.0"         ~expected:false
let%expect_test _ = is_multicast "127.0.0.1"       ~expected:false
let%expect_test _ = is_multicast "155.246.1.20"    ~expected:false
let%expect_test _ = is_multicast "223.0.0.1"       ~expected:false
let%expect_test _ = is_multicast "223.255.255.255" ~expected:false
let%expect_test _ = is_multicast "224.0.0.0"
let%expect_test _ = is_multicast "224.0.0.1"
let%expect_test _ = is_multicast "226.128.255.16"
let%expect_test _ = is_multicast "233.128.255.16"
let%expect_test _ = is_multicast "239.0.0.1"
let%expect_test _ = is_multicast "239.255.255.255"
let%expect_test _ = is_multicast "240.0.0.0"       ~expected:false
let%expect_test _ = is_multicast "240.0.0.1"       ~expected:false
let%expect_test _ = is_multicast "255.255.255.255" ~expected:false

(* [all_matching_addresses] tests *)
let%expect_test _ =
  test_all_matching_addresses "172.16.0.8/32"
    [ "172.16.0.8" ]
;;

let%expect_test _ =
  test_all_matching_addresses "172.16.0.8/30"
    [ "172.16.0.8" ; "172.16.0.9" ; "172.16.0.10" ; "172.16.0.11" ]
;;

let%expect_test _ =
  test_all_matching_addresses "172.16.0.8/24"
    (List.init 256 ~f:(fun i -> sprintf "172.16.0.%d" i))
;;

(* example from .mli *)
let%expect_test _ =
  print_s [%message "" ~_:(to_string (of_string "192.168.1.101/24") : string)];
  [%expect {| 192.168.1.0/24 |}];
;;

let%expect_test "hash function consistency" =
  let open Int.Replace_polymorphic_compare in
  let t_list =
    let addr_list =
      ["0.0.0.0"; "255.0.0.0"; "255.255.0.0"; "255.255.255.0"; "255.255.255.255"]
    in
    let bits_list = [ 8; 16; 24; 32 ] in
    List.concat_map addr_list ~f:(fun addr ->
      let base_address = Inet_addr.of_string addr in
      List.map bits_list ~f:(fun bits ->
        create ~base_address ~bits))
  in
  List.iter t_list ~f:(fun t1 ->
    List.iter t_list ~f:(fun t2 ->
      if Cidr.compare t1 t2 = 0 then
        require [%here] ((hash t1) = (hash t2))))
;;

(* differentiate bit counts *)

let%expect_test _ = same "0.0.0.0/32" "0.0.0.0/32"
let%expect_test _ = diff "0.0.0.0/32" "0.0.0.0/24"
let%expect_test _ = diff "0.0.0.0/32" "0.0.0.0/26"
let%expect_test _ = diff "0.0.0.0/32" "0.0.0.0/8"
let%expect_test _ = diff "0.0.0.0/32" "0.0.0.0/0"
let%expect_test _ = diff "0.0.0.0/24" "0.0.0.0/0"
let%expect_test _ = diff "0.0.0.0/16" "0.0.0.0/0"
let%expect_test _ = diff "0.0.0.0/8" "0.0.0.0/0"
let%expect_test _ = same "0.0.0.0/0" "0.0.0.0/0"

(* normalize base addresses *)

let%expect_test _ = diff "0.0.0.0/32" "0.0.0.1/32"
let%expect_test _ = same "0.0.0.0/31" "0.0.0.1/31"

let%expect_test _ = diff "0.0.0.0/25" "0.0.0.255/25"
let%expect_test _ = same "0.0.0.0/24" "0.0.0.255/24"

let%expect_test _ = diff "0.0.0.0/17" "0.0.255.255/17"
let%expect_test _ = same "0.0.0.0/16" "0.0.255.255/16"

let%expect_test _ = diff "0.0.0.0/9" "0.255.255.255/9"
let%expect_test _ = same "0.0.0.0/8" "0.255.255.255/8"

let%expect_test _ = diff "0.0.0.0/1" "255.255.255.255/1"
let%expect_test _ = same "0.0.0.0/0" "255.255.255.255/0"
