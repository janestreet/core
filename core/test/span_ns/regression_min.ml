open Core

module Span = Time_ns.Span

let check label passed =
  if not passed then failwith label

let check_unit label expected span =
  check label (Poly.equal (Span.to_unit_of_time span) expected)

let () =
  let open Unit_of_time in
  let smallest = Span.min_value_representable in
  let parts = Span.to_parts smallest in
  check "minimum span has a negative sign" (Poly.equal parts.sign Sign.Neg);
  check "minimum span has exact components"
    (Poly.equal
       (parts.hr, parts.min, parts.sec, parts.ms, parts.us, parts.ns)
       (1_281_023, 53, 38, 427, 387, 904));
  check_unit "minimum span uses days" Day smallest;
  check "minimum span short string"
    (String.equal (Span.to_short_string smallest) "-53375d");

  List.iter
    [ "day", Day, Hour
    ; "hour", Hour, Minute
    ; "minute", Minute, Second
    ; "second", Second, Millisecond
    ; "millisecond", Millisecond, Microsecond
    ; "microsecond", Microsecond, Nanosecond
    ]
    ~f:(fun (label, at_boundary, below_boundary) ->
      let boundary = Span.to_int63_ns (Span.of_unit_of_time at_boundary) in
      let below = Int63.pred boundary in
      check_unit (label ^ ": positive boundary") at_boundary
        (Span.of_int63_ns boundary);
      check_unit (label ^ ": negative boundary") at_boundary
        (Span.of_int63_ns (Int63.neg boundary));
      check_unit (label ^ ": just below positive boundary") below_boundary
        (Span.of_int63_ns below);
      check_unit (label ^ ": just below negative boundary magnitude") below_boundary
        (Span.of_int63_ns (Int63.neg below)));
  print_endline "Minimum span and unit boundary checks passed"
