open! Core
module Unix = Core_unix
open! Unix.Inet_addr

let%bench_fun "inet4_addr_of_int32" =
  fun () -> inet4_addr_of_int32 0l

let%bench_fun "inet4_addr_to_int32_exn" =
  fun () -> inet4_addr_to_int32_exn localhost

let%bench_fun "inet4_addr_of_int63" =
  fun () -> inet4_addr_of_int63 (Int63.of_int 0)

let%bench_fun "inet4_addr_to_int63_exn" =
  fun () -> inet4_addr_to_int63_exn localhost

let%bench_fun "Inet_addr.inet4_addr_of_int32 0.0.0.0" =
  let int32 = 0l in
  fun () -> inet4_addr_of_int32 int32

let%bench_fun "inet4_addr_of_int32 255.255.255.255" =
  let int32 = 0xffff_ffffl in
  fun () -> inet4_addr_of_int32 int32

let%bench_fun "inet4_addr_to_int32_exn 0.0.0.0" =
  let inet_addr = of_string "0.0.0.0" in
  fun () -> inet4_addr_to_int32_exn inet_addr

let%bench_fun "inet4_addr_to_int32_exn 255.255.255.255" =
  let inet_addr = of_string "255.255.255.255" in
  fun () -> inet4_addr_to_int32_exn inet_addr

let%bench_fun "of_string 0.0.0.0" =
  let string = "0.0.0.0" in
  fun () -> of_string string

let%bench_fun "of_string 255.255.255.255" =
  let string = "255.255.255.255" in
  fun () -> of_string string

let%bench_fun "to_string 0.0.0.0" =
  let inet_addr = of_string "0.0.0.0" in
  fun () -> to_string inet_addr

let%bench_fun "to_string 255.255.255.255" =
  let inet_addr = of_string "255.255.255.255" in
  fun () -> to_string inet_addr
