open OUnit
open Core
open Blang

let to_string t = Sexp.to_string (Blang.sexp_of_t String.sexp_of_t t)

let (===) b1 b2 =
  if b1 <> b2 then
    failwithf "%s <> %s" (to_string b1) (to_string b2) ()

let same_sexp b1 b2 =
  let s1 = to_string b1
  and s2 = to_string b2
  in
  if s1 <> s2 then
    failwithf "%s <> %s" s1 s2 ()

let test =
  "blang" >:::
  [ "simplification" >::: [
      (**)
      "not1" >:: (fun () -> not_ true_ === false_);
      "not2" >:: (fun () -> not_ false_ === true_);
      (**)
      "and1" >:: (fun () -> and_ [] === true_);
      "and2" >:: (fun () -> and_ [true_] === true_);
      "and3" >:: (fun () -> and_ [true_; base "A"] === base "A");
      "and4" >:: (fun () -> and_ [base "A"; true_] === base "A");
      "and5" >:: (fun () ->
        and_ [base "A"; base "B"; true_] === and_ [base "A"; base "B"]);
      "and6" >:: (fun () ->
        and_ [base "A"; true_; base "B"] === and_ [base "A"; base "B"]);
      "and7" >:: (fun () ->
        and_ [true_; base "A"; base "B"] === and_ [base "A"; base "B"]);
      "and8" >:: (fun () -> and_ [false_] === false_);
      "and9" >:: (fun () -> and_ [false_; base "A"] === false_);
      "and10" >:: (fun () -> and_ [base "A"; false_] === false_);
      "and11" >:: (fun () -> and_ [base "A"; base "B"; false_] === false_);
      "and12" >:: (fun () -> and_ [base "A"; false_; base "B"] === false_);
      "and13" >:: (fun () -> and_ [false_; base "A"; base "B"] === false_);
      (**)
      "or1" >:: (fun () -> or_ [] === false_);
      "or2" >:: (fun () -> or_ [false_] === false_);
      "or3" >:: (fun () -> or_ [false_; base "A"] === base "A");
      "or4" >:: (fun () -> or_ [base "A"; false_] === base "A");
      "or5" >:: (fun () ->
        or_ [base "A"; base "B"; false_] === or_ [base "A"; base "B"]);
      "or6" >:: (fun () ->
        or_ [base "A"; false_; base "B"] === or_ [base "A"; base "B"]);
      "or7" >:: (fun () ->
        or_ [false_; base "A"; base "B"] === or_ [base "A"; base "B"]);
      "or8" >:: (fun () -> or_ [true_] === true_);
      "or9" >:: (fun () -> or_ [true_; base "A"] === true_);
      "or10" >:: (fun () -> or_ [base "A"; true_] === true_);
      "or11" >:: (fun () -> or_ [base "A"; base "B"; true_] === true_);
      "or12" >:: (fun () -> or_ [base "A"; true_; base "B"] === true_);
      "or13" >:: (fun () -> or_ [true_; base "A"; base "B"] === true_);
      (**)
      "if1" >:: (fun () -> if_ true_ (base "A") (base "B") === base "A");
      "if2" >:: (fun () -> if_ false_ (base "A") (base "B") === base "B");
      "if3" >:: (fun () ->
        if_ (base "A") true_ (base "B") === or_ [base "A"; base "B"]);
      "if4" >:: (fun () ->
        if_ (base "A") (base "B") false_ === and_ [base "A"; base "B"]);
      "if5" >:: (fun () ->
        if_ (base "A") (base "B") true_ === or_ [not_ (base "A"); base "B"]);
      "if6" >:: (fun () ->
        if_ (base "A") false_ (base "B") === and_ [not_ (base "A"); base "B"]);
      (**)
      "ifnot1" >:: (fun () -> if_ (not_ false_) (base "A") (base "B") === base "A");
      "ifnot2" >:: (fun () -> if_ (not_ true_) (base "A") (base "B") === base "B");
    ];
    "flattening" >:::
    [
      "and" >:: (fun () ->
        same_sexp
          (and_ [base "A"; and_ [base "B"; base "C"]])
          (and_ [base "A"; base "B"; base "C"]));
      "or" >:: (fun () ->
        same_sexp
          (or_ [base "A"; or_ [base "B"; base "C"]])
          (or_ [base "A"; base "B"; base "C"]));
    ];
    "substitution" >::: [
      "base" >:: (fun () ->
        bind (base "A") ~f:(fun x -> and_ [base x; base x])
        === and_ [base "A"; base "A"]);
      "and" >:: (fun () ->
        bind (and_ [base "A"; base "B"]) ~f:(fun x -> or_ [base x; base x])
        === and_ [or_ [base "A"; base "A"]; or_ [base "B"; base "B"]]);
    ];
    "values" >:: (fun () ->
      let blang1 =
        and_ [
          if_ (and_ []) (base "this") (base "that"); 
          or_ [
            if_ (not_ (and_ [])) (base "something") (base "other");
            base "yet another thing";
          ]
        ]
      in
      let values = Blang.values blang1 in
      if ["this"; "other"; "yet another thing"] <> values then
        failwithf "Got: %s" (String.concat ~sep:"," values) ());
  ]

