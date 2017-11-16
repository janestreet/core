open Core_kernel.Core_kernel_stable

module Stable = struct
  module String_style = struct
    module V1 = struct
      type t = Dash | Dot | Colon
      [@@deriving bin_io, compare, enumerate, sexp]
    end

    module Latest = V1
  end

  module V1 = struct
    module Without_containers = struct
      type t = Int63.V1.t [@@deriving bin_io, compare]

      module Deriving_hash : sig
        type t [@@deriving hash]
      end with type t := t = struct
        type t = Core_kernel.Int63.t [@@deriving hash]
      end

      include Deriving_hash

      let invalid_hex_char char =
        Core_kernel.raise_s [%message "invalid hex digit" (char : char)]

      let invalid_hex_digit int =
        Core_kernel.raise_s [%message "invalid hex digit" (int : int)]

      (* convert a hex char to a int *)
      let int_of_hex c =
        match c with
        | '0' .. '9' -> int_of_char c - int_of_char '0'
        | 'a' .. 'f' -> int_of_char c - int_of_char 'a' + 10
        | 'A' .. 'F' -> int_of_char c - int_of_char 'A' + 10
        | _          -> invalid_hex_char c

      (* convert a 4-bit number to a hex character *)
      let hex_of_int i =
        if i >= 0x0 && i <= 0x9
        then char_of_int (int_of_char '0' + i)
        else if i >= 0xa && i <= 0xf
        then char_of_int (int_of_char 'a' + (i - 0xa))
        else invalid_hex_digit i

      let int63_of_int = Core_kernel.Int63.of_int
      let int_of_int63 = Core_kernel.Int63.to_int_exn

      let int63_of_hex c     = int63_of_int (int_of_hex   c)
      let hex_of_int63 int63 = hex_of_int   (int_of_int63 int63)

      let bad_string s =
        Core_kernel.raise_s [%message
          "invalid MAC address"
            ~_:(s : string)]

      let get = Core_kernel.String.unsafe_get
      let set = Core_kernel.Bytes.unsafe_set

      include struct
        open Core_kernel.Int63

        let (lxor) = bit_xor
        let (land) = bit_and
        let (lsl)  = shift_left
        let (lsr)  = shift_right_logical
      end

      let of_string_quad s c : t =
        if not (Char.equal c (get s 4))
        || not (Char.equal c (get s 9))
        then bad_string s;
        (int63_of_hex (get s  0) lsl (4 * 11)) lxor
        (int63_of_hex (get s  1) lsl (4 * 10)) lxor
        (int63_of_hex (get s  2) lsl (4 *  9)) lxor
        (int63_of_hex (get s  3) lsl (4 *  8)) lxor
        (int63_of_hex (get s  5) lsl (4 *  7)) lxor
        (int63_of_hex (get s  6) lsl (4 *  6)) lxor
        (int63_of_hex (get s  7) lsl (4 *  5)) lxor
        (int63_of_hex (get s  8) lsl (4 *  4)) lxor
        (int63_of_hex (get s 10) lsl (4 *  3)) lxor
        (int63_of_hex (get s 11) lsl (4 *  2)) lxor
        (int63_of_hex (get s 12) lsl (4 *  1)) lxor
        (int63_of_hex (get s 13) lsl (4 *  0))

      let to_string_quad (t : t) c =
        let s = Core_kernel.Bytes.make 14 c in
        set s  0 (hex_of_int63 ((t lsr (4 * 11)) land int63_of_int 0xf));
        set s  1 (hex_of_int63 ((t lsr (4 * 10)) land int63_of_int 0xf));
        set s  2 (hex_of_int63 ((t lsr (4 *  9)) land int63_of_int 0xf));
        set s  3 (hex_of_int63 ((t lsr (4 *  8)) land int63_of_int 0xf));
        set s  5 (hex_of_int63 ((t lsr (4 *  7)) land int63_of_int 0xf));
        set s  6 (hex_of_int63 ((t lsr (4 *  6)) land int63_of_int 0xf));
        set s  7 (hex_of_int63 ((t lsr (4 *  5)) land int63_of_int 0xf));
        set s  8 (hex_of_int63 ((t lsr (4 *  4)) land int63_of_int 0xf));
        set s 10 (hex_of_int63 ((t lsr (4 *  3)) land int63_of_int 0xf));
        set s 11 (hex_of_int63 ((t lsr (4 *  2)) land int63_of_int 0xf));
        set s 12 (hex_of_int63 ((t lsr (4 *  1)) land int63_of_int 0xf));
        set s 13 (hex_of_int63 ((t lsr (4 *  0)) land int63_of_int 0xf));
        Core_kernel.Bytes.unsafe_to_string ~no_mutation_while_string_reachable:s

      let of_string_pair s c : t =
        if not (Char.equal c (get s  2))
        || not (Char.equal c (get s  5))
        || not (Char.equal c (get s  8))
        || not (Char.equal c (get s 11))
        || not (Char.equal c (get s 14))
        then bad_string s;
        (int63_of_hex (get s  0) lsl (4 * 11)) lxor
        (int63_of_hex (get s  1) lsl (4 * 10)) lxor
        (int63_of_hex (get s  3) lsl (4 *  9)) lxor
        (int63_of_hex (get s  4) lsl (4 *  8)) lxor
        (int63_of_hex (get s  6) lsl (4 *  7)) lxor
        (int63_of_hex (get s  7) lsl (4 *  6)) lxor
        (int63_of_hex (get s  9) lsl (4 *  5)) lxor
        (int63_of_hex (get s 10) lsl (4 *  4)) lxor
        (int63_of_hex (get s 12) lsl (4 *  3)) lxor
        (int63_of_hex (get s 13) lsl (4 *  2)) lxor
        (int63_of_hex (get s 15) lsl (4 *  1)) lxor
        (int63_of_hex (get s 16) lsl (4 *  0))

      let to_string_pair (t : t) c =
        let s = Core_kernel.Bytes.make 17 c in
        set s  0 (hex_of_int63 ((t lsr (4 * 11)) land int63_of_int 0xf));
        set s  1 (hex_of_int63 ((t lsr (4 * 10)) land int63_of_int 0xf));
        set s  3 (hex_of_int63 ((t lsr (4 *  9)) land int63_of_int 0xf));
        set s  4 (hex_of_int63 ((t lsr (4 *  8)) land int63_of_int 0xf));
        set s  6 (hex_of_int63 ((t lsr (4 *  7)) land int63_of_int 0xf));
        set s  7 (hex_of_int63 ((t lsr (4 *  6)) land int63_of_int 0xf));
        set s  9 (hex_of_int63 ((t lsr (4 *  5)) land int63_of_int 0xf));
        set s 10 (hex_of_int63 ((t lsr (4 *  4)) land int63_of_int 0xf));
        set s 12 (hex_of_int63 ((t lsr (4 *  3)) land int63_of_int 0xf));
        set s 13 (hex_of_int63 ((t lsr (4 *  2)) land int63_of_int 0xf));
        set s 15 (hex_of_int63 ((t lsr (4 *  1)) land int63_of_int 0xf));
        set s 16 (hex_of_int63 ((t lsr (4 *  0)) land int63_of_int 0xf));
        Core_kernel.Bytes.unsafe_to_string ~no_mutation_while_string_reachable:s

      let of_string s : t =
        match Core_kernel.String.length s with
        | 14 -> of_string_quad s '.'
        | 17 ->
          (match get s 2 with
           | ':' -> of_string_pair s ':'
           | '-' -> of_string_pair s '-'
           | _   -> bad_string s)
        | _ -> bad_string s

      let to_string_with_style (t : t) ~style =
        match (style : String_style.V1.t) with
        | Dot   -> to_string_quad t '.'
        | Colon -> to_string_pair t ':'
        | Dash  -> to_string_pair t '-'

      let to_string (x : t) = to_string_with_style x ~style:Dash

      include Sexpable.Of_stringable.V1 (struct
          type nonrec t = t
          let to_string = to_string
          let of_string = of_string
        end)

      include Comparator.V1.Make (struct
          type nonrec t = t [@@deriving compare, sexp_of]
        end)
    end

    include Without_containers
    include Comparable.V1.Make (Without_containers)
    include Hashable.V1.Make   (Without_containers)

    include Unit_test (struct
        include Without_containers
        let equal x y = (compare x y = 0)

        let make = Core_kernel.Int63.of_int64_exn

        let tests =
          [ make 0x0000_0000_0000L, "00-00-00-00-00-00", "\000"
          ; make 0x0123_4567_89abL, "01-23-45-67-89-ab", "\252\171\137gE#\001\000\000"
          ; make 0xfedc_ba98_7654L, "fe-dc-ba-98-76-54", "\252Tv\152\186\220\254\000\000"
          ; make 0xffff_ffff_ffffL, "ff-ff-ff-ff-ff-ff", "\252\255\255\255\255\255\255\000\000"
          ]
      end)
  end

  module Latest = V1
end

open Core_kernel

module String_style = struct
  module Stable = Stable.String_style
  include Stable.Latest

  include Sexpable.To_stringable (Stable.Latest)

  let arg =
    Command.Arg_type.of_alist_exn (List.map all ~f:(fun t ->
      String.lowercase (to_string t), t))
end

include Stable.Latest.Without_containers

let broadcast = Int63.of_int64_exn 0xffff_ffff_ffffL
let any = Int63.zero

let to_int63 = Fn.id

let of_int63_exn mac_address =
  if mac_address land broadcast <> mac_address then begin
    raise_s [%message
      "MAC address does not fit in 48 bits"
        (mac_address : Int63.Hex.t)]
  end;
  mac_address

let of_int_exn int = of_int63_exn (Int63.of_int int)

let to_int_exn t = Int63.to_int_exn (to_int63 t)

let gen = Int63.gen_incl any broadcast
let obs = Int63.obs
let shrinker = Int63.shrinker

let%test_module "Mac_address" =
  (module struct

    let gen_hex_char =
      Quickcheck.Generator.of_list
        [ '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'
        ; 'a'; 'b'; 'c'; 'd'; 'e'; 'f'
        ; 'A'; 'B'; 'C'; 'D'; 'E'; 'F'
        ]

    let gen_dash =
      let open Quickcheck.Generator.Let_syntax in
      let two_hex_chars = String.gen_with_length 2 gen_hex_char in
      let%bind pairs = List.gen_with_length 6 two_hex_chars in
      return (String.concat ~sep:"-" pairs)

    let gen_dotted =
      let open Quickcheck.Generator.Let_syntax in
      let four_hex_chars = String.gen_with_length 4 gen_hex_char in
      let%bind quads = List.gen_with_length 3 four_hex_chars in
      return (String.concat ~sep:"." quads)

    let gen_colon =
      let open Quickcheck.Generator.Let_syntax in
      let two_hex_chars = String.gen_with_length 2 gen_hex_char in
      let%bind pairs = List.gen_with_length 6 two_hex_chars in
      return (String.concat ~sep:":" pairs)

    let seed = `Deterministic "mac tests"
    let trials = 10_000

    let%test_unit "(of_string . to_string) = id[t -> t]" =
      Quickcheck.test ~seed ~trials gen ~f:(fun expect ->
        let actual = of_string (to_string expect) in
        [%test_result: t] ~expect actual)

    let%test_unit "(of_int63_exn . to_int63) = id[t -> t]" =
      Quickcheck.test ~seed ~trials gen ~f:(fun expect ->
        let actual = of_int63_exn (to_int63 expect) in
        [%test_result: t] ~expect actual)

    let%test_unit "(to_string . of_string) = id[string -> string]" =
      Quickcheck.test ~seed ~trials gen_dash ~f:(fun expect ->
        let actual = to_string_with_style ~style:Dash (of_string expect) in
        [%test_result: string] ~expect:(String.lowercase expect) actual);
      Quickcheck.test ~seed ~trials gen_dotted ~f:(fun expect ->
        let actual = to_string_with_style ~style:Dot (of_string expect) in
        [%test_result: string] ~expect:(String.lowercase expect) actual);
      Quickcheck.test ~seed ~trials gen_colon ~f:(fun expect ->
        let actual = to_string_with_style ~style:Colon (of_string expect) in
        [%test_result: string] ~expect:(String.lowercase expect) actual)

    let%test_unit "broadcast to_string" =
      [%test_result: string] ~expect:"ff-ff-ff-ff-ff-ff"
        (to_string_with_style ~style:Dash broadcast);
      [%test_result: string] ~expect:"ffff.ffff.ffff"
        (to_string_with_style ~style:Dot broadcast);
      [%test_result: string] ~expect:"ff:ff:ff:ff:ff:ff"
        (to_string_with_style ~style:Colon broadcast)

    let%test_unit "any to_string" =
      [%test_result: string] ~expect:"00-00-00-00-00-00"
        (to_string_with_style ~style:Dash any);
      [%test_result: string] ~expect:"0000.0000.0000"
        (to_string_with_style ~style:Dot any);
      [%test_result: string] ~expect:"00:00:00:00:00:00"
        (to_string_with_style ~style:Colon any)
  end)

let%bench_module "Mac_address" =
  (module struct
    let%bench_fun "of_string / dot" =
      let s = "001c.7351.42a8" in
      fun () -> of_string s

    let%bench_fun "of_string / dash" =
      let s = "00-1c-73-51-42-a8" in
      fun () -> of_string s

    let%bench_fun "of_string / colon" =
      let s = "00:1c:73:51:42:a8" in
      fun () -> of_string s

    let%bench_fun "to_string / dot" =
      let t = of_string "001c.7351.42a8" in
      fun () -> to_string_with_style t ~style:Dot

    let%bench_fun "to_string / dash" =
      let t = of_string "00-1c-73-51-42-a8" in
      fun () -> to_string_with_style t ~style:Dash

    let%bench_fun "to_string / colon" =
      let t = of_string "00:1c:73:51:42:a8" in
      fun () -> to_string_with_style t ~style:Colon
  end)

include Identifiable.Make_using_comparator (struct
    include Stable.Latest
    let module_name = "Core.Mac_address"
  end)
