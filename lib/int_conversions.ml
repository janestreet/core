type 'a int_spec = {
  name : string;
  num_bits : int;
  max : 'a;
  min : 'a;
  to_string : 'a -> string;
  compare : 'a -> 'a -> int;
}

let convert a b a_to_b b_to_a =
  if a.num_bits <= b.num_bits then
    ((fun i -> Some (a_to_b i)), a_to_b)
  else
    let { min = b_min; max = b_max; name = b_name; _ } = b in
    let min = b_to_a b_min in
    let max = b_to_a b_max in
    let compare = a.compare in
    let is_in_range i = compare min i <= 0 && compare i max <= 0 in
    let convert i = if is_in_range i then Some (a_to_b i) else None in
    let { name = a_name; to_string = a_to_string; _ } = a in
    let convert_exn i =
      if is_in_range i then
        a_to_b i
      else
        Core_printf.failwithf
          "conversion from %s to %s failed: %s is out of range"
          a_name b_name (a_to_string i) ()
    in
    convert, convert_exn
;;

let compare_int (x : int) y = compare x y

let int = {
  name = "int";
  num_bits = Word_size.num_bits Word_size.word_size - 1;
  max = max_int;
  min = min_int;
  to_string = string_of_int;
  compare = compare_int;
}

let int32 = {
  name = "int32";
  num_bits = 32;
  max = Int32.max_int;
  min = Int32.min_int;
  to_string = Int32.to_string;
  compare = Int32.compare;
}

let int64 = {
  name = "int64";
  num_bits = 64;
  max = Int64.max_int;
  min = Int64.min_int;
  to_string = Int64.to_string;
  compare = Int64.compare;
}

let nativeint = {
  name = "nativeint";
  num_bits = Word_size.num_bits Word_size.word_size;
  max = Nativeint.max_int;
  min = Nativeint.min_int;
  to_string = Nativeint.to_string;
  compare = Nativeint.compare;
}

let (int_to_int32, int_to_int32_exn) =
  convert int int32 Int32.of_int Int32.to_int
let (int32_to_int, int32_to_int_exn) =
  convert int32 int Int32.to_int Int32.of_int

let int_to_int64 = Int64.of_int
let (int64_to_int, int64_to_int_exn) =
  convert int64 int Int64.to_int Int64.of_int

let int_to_nativeint = Nativeint.of_int
let (nativeint_to_int, nativeint_to_int_exn) =
  convert nativeint int Nativeint.to_int Nativeint.of_int

let int32_to_int64 = Int64.of_int32
let (int64_to_int32, int64_to_int32_exn) =
  convert int64 int32 Int64.to_int32 Int64.of_int32

let int32_to_nativeint = Nativeint.of_int32
let (nativeint_to_int32, nativeint_to_int32_exn) =
  convert nativeint int32 Nativeint.to_int32 Nativeint.of_int32
;;

let (int64_to_nativeint, int64_to_nativeint_exn) =
  convert int64 nativeint Int64.to_nativeint Int64.of_nativeint
;;
let nativeint_to_int64 = Int64.of_nativeint

(**
   Takes an int represented as a string ((-|+)?[0-9]+) and puts delimiter
   every 3 digits starting from the right.
*)
let insert_delimiter input ~delimiter =
  let chars_per_delimiter = 3 in
  let input_length = String.length input in
  if input_length <= chars_per_delimiter then
    input
  else begin
    let has_sign = match input.[0] with '+' | '-' -> true | _ -> false in
    let num_digits = if has_sign then input_length - 1 else input_length in
    let num_delimiters = (num_digits - 1) / chars_per_delimiter in
    let output_length = input_length + num_delimiters in
    let output = String.create output_length in
    let input_pos = ref (input_length - 1) in
    let output_pos = ref (output_length - 1) in
    let num_chars_until_delimiter = ref chars_per_delimiter in
    let first_digit_pos = if has_sign then 1 else 0 in
    while !input_pos >= first_digit_pos do
      if !num_chars_until_delimiter = 0 then begin
        output.[!output_pos] <- delimiter;
        decr output_pos;
        num_chars_until_delimiter := chars_per_delimiter;
      end;
      output.[!output_pos] <- input.[!input_pos];
      decr input_pos;
      decr output_pos;
      decr num_chars_until_delimiter;
    done;
    if has_sign then output.[0] <- input.[0];
    output;
  end
;;

(**
   Takes an int represented as a string ((-|+)?[0-9]+) and puts underscores
   every 3 digits starting from the right.
*)
let insert_underscores input =
  insert_delimiter input ~delimiter:'_'
;;

let sexp_of_int_style : [ `Underscores | `No_underscores ] ref =
  ref `No_underscores
;;

module Make (I : sig
  type t
  val to_string : t -> string
end) = struct

  let to_string_hum t = insert_underscores (I.to_string t)

  let sexp_of_t t =
    let s = I.to_string t in
    Sexplib.Sexp.Atom
      (match !sexp_of_int_style with
      | `Underscores -> insert_underscores s
      | `No_underscores -> s)
  ;;
end
