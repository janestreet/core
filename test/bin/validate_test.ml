open Core
open OUnit
module V = Validate

let test =
  "validate" >::: [
    "basic" >:: (fun () ->
      "bounds" @? (
        let res = V.name_list "foo" [
          V.name "ok" (Int.validate_bound ~min:(Incl 0) ~max:(Incl 100) 5);
          V.name "incl_lower" (Int.validate_bound ~min:(Incl 0) ~max:(Incl 100) (-1));
          V.name "incl_lower" (Int.validate_bound ~min:(Incl 0) ~max:(Incl 100) (0));
          V.name "incl_upper" (Int.validate_bound ~min:(Incl 0) ~max:(Incl 100) (101));
          V.name "incl_upper" (Int.validate_bound ~min:(Incl 0) ~max:(Incl 100) (100));
          V.name "excl_lower" (Int.validate_bound ~min:(Excl 0) ~max:(Excl 100) (0));
          V.name "excl_upper" (Int.validate_bound ~min:(Excl 0) ~max:(Excl 100) (100));
          V.name "excl_lower" (Int.validate_bound ~min:(Excl 0) ~max:(Excl 100) (1));
          V.name "excl_upper" (Int.validate_bound ~min:(Excl 0) ~max:(Excl 100) (99));
        ]
        in
        let expected =
          [ "(foo.incl_lower \"value -1 < bound 0\")"
          ; "(foo.incl_upper \"value 101 > bound 100\")"
          ; "(foo.excl_lower \"value 0 <= bound 0\")"
          ; "(foo.excl_upper \"value 100 >= bound 100\")"
          ]
        in
        List.sort ~compare:Poly.ascending (V.errors res)
        = List.sort ~compare:Poly.ascending expected;
      );

      "inf/nan" @? (
        let res = V.name_list "bar" [
          V.name "ok" (Float.validate_bound ~min:(Incl 0.) ~max:(Incl 100.) 5.);
          V.name "nan" (Float.validate_bound ~min:(Incl 0.) ~max:(Incl 100.) Float.nan);
          V.name "inf" (Float.validate_bound ~min:(Incl 0.) ~max:(Incl 100.) Float.infinity);
        ]
        in
        let expected =
          [ "(bar.nan \"value is NaN\")"
          ; "(bar.inf \"value is infinite\")"
          ]
        in
        List.sort ~compare:Poly.ascending (V.errors res)
        = List.sort ~compare:Poly.ascending expected;
      );

      "nesting" @? (
        let res = V.name_list "nesting" [
          V.name "ok" (Float.validate_bound ~min:(Incl 0.) ~max:(Incl 100.) 5.);
          V.name "top" (Float.validate_ordinary Float.nan);
          V.name_list "sub0" [
            V.name "sub1" (Float.validate_ordinary Float.nan);
            V.name "sub2" (Float.validate_ordinary Float.nan);
            V.name_list "sub3" [
              V.name "sub4" (Float.validate_ordinary Float.nan);
              V.name "sub5" (Float.validate_ordinary Float.nan);
            ]]]
        in
        let expected =
          [ "(nesting.top \"value is NaN\")"
          ; "(nesting.sub0.sub1 \"value is NaN\")"
          ; "(nesting.sub0.sub2 \"value is NaN\")"
          ; "(nesting.sub0.sub3.sub4 \"value is NaN\")"
          ; "(nesting.sub0.sub3.sub5 \"value is NaN\")"
          ]
        in
        List.sort ~compare:Poly.ascending (V.errors res)
        = List.sort ~compare:Poly.ascending expected;
      );

      "empty" @? (
        let res = V.name_list "" [
          V.name "ok1" (Float.validate_bound ~min:(Incl 0.) ~max:(Incl 100.) 5.);
          V.name "ok2" (Float.validate_bound ~min:(Incl 0.) ~max:(Incl 100.) 6.);
          V.name_list "sub" [
            V.name "ok3" (Float.validate_bound ~min:(Incl 0.) ~max:(Incl 100.) 22.);
            V.name "ok4" (Float.validate_bound ~min:(Incl 0.) ~max:(Incl 100.) 36.);
          ]]
        in
        let expected = [] in
        List.sort ~compare:Poly.ascending (V.errors res) =
        List.sort ~compare:Poly.ascending expected
      );
    )
  ]
