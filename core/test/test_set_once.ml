open! Core
open! Import
open! Set_once

module type Format = sig
  type 'a t = 'a Set_once.t [@@deriving bin_io, sexp]
end

let test_serialization (module Format : Format) =
  let module T = struct
    type t = int Format.t [@@deriving bin_io, sexp]

    let compare t1 t2 = Option.compare [%compare: int] (get t1) (get t2)
  end
  in
  let t1 = create () in
  let t2 = create () in
  set_exn t2 [%here] 13;
  print_and_check_stable_type [%here] (module T) [ t1; t2 ]
;;

let%expect_test "[Stable.V1] serialization" =
  test_serialization (module Stable.V1);
  [%expect
    {|
    (bin_shape_digest 8b7c356301db5206ab98e334f4886c11)
    ((sexp ()) (bin_io "\000"))
    ((sexp (13)) (bin_io "\001\r"))
    |}]
;;

let%expect_test "[Unstable] serialization" =
  test_serialization (module Unstable);
  [%expect
    {|
    (bin_shape_digest 8b7c356301db5206ab98e334f4886c11)
    ((sexp ()) (bin_io "\000"))
    ((sexp (13)) (bin_io "\001\r"))
    |}]
;;

type t = int Set_once.t [@@deriving sexp_of]

let hide_positions = true

let show t =
  print_s ~hide_positions [%message "" ~_:(t : t)];
  invariant ignore t
;;

let%expect_test "[sexp_of_t]" =
  let t = create () in
  show t;
  [%expect {| unset |}];
  set_exn t [%here] 13;
  show t;
  [%expect
    {|
    ((value  13)
     (set_at lib/core/test/test_set_once.ml:LINE:COL))
    |}]
;;

let%expect_test "[get]" =
  let t = create () in
  let show_get () = print_s [%message "" ~_:(get t : int option)] in
  show_get ();
  [%expect {| () |}];
  set_exn t [%here] 13;
  show_get ();
  [%expect {| (13) |}]
;;

let%expect_test "[get] doesn't allocate" =
  let t = create () in
  let check_get here =
    ignore (require_no_allocation here (fun () -> get t) : int option)
  in
  check_get [%here];
  [%expect {| |}];
  set_exn t [%here] 13;
  check_get [%here];
  [%expect {| |}]
;;

let%expect_test "[get_exn]" =
  let t = create () in
  show_raise ~hide_positions (fun () -> get_exn t [%here]);
  [%expect
    {| (raised ("[Set_once.get_exn] unset" (at lib/core/test/test_set_once.ml:LINE:COL))) |}];
  set_exn t [%here] 13;
  print_s [%message "" ~_:(get_exn t [%here] : int)];
  [%expect {| 13 |}]
;;

let%expect_test "[set]" =
  let t = create () in
  print_s [%message "" ~_:(set t [%here] 13 : unit Or_error.t)];
  [%expect {| (Ok ()) |}]
;;

let%expect_test "[set_if_none]" =
  let t = create () in
  let set_if_none_and_print ~value =
    set_if_none t [%here] value;
    print_s ~hide_positions [%sexp (Set_once.get_exn t [%here] : string)]
  in
  set_if_none_and_print ~value:"first call to set_if_none";
  [%expect {| "first call to set_if_none" |}];
  set_if_none_and_print ~value:"second call to set_if_none";
  [%expect {| "first call to set_if_none" |}]
;;

let%expect_test "[set] error" =
  let t = create () in
  set_exn t [%here] 13;
  print_s ~hide_positions [%message "" ~_:(set t [%here] 14 : unit Or_error.t)];
  [%expect
    {|
    (Error (
      "[Set_once.set_exn] already set"
      (setting_at lib/core/test/test_set_once.ml:LINE:COL)
      (previously_set_at lib/core/test/test_set_once.ml:LINE:COL)))
    |}]
;;

let%expect_test "[set_exn] error" =
  let t = create () in
  set_exn t [%here] 13;
  show_raise ~hide_positions (fun () -> set_exn t [%here] 14);
  [%expect
    {|
    (raised (
      "[Set_once.set_exn] already set"
      (setting_at lib/core/test/test_set_once.ml:LINE:COL)
      (previously_set_at lib/core/test/test_set_once.ml:LINE:COL)))
    |}]
;;

let%expect_test "[is_none], [is_some]" =
  let t = create () in
  let show () =
    print_s [%message "" ~is_none:(is_none t : bool) ~is_some:(is_some t : bool)]
  in
  show ();
  [%expect {|
    ((is_none true)
     (is_some false))
    |}];
  set_exn t [%here] 13;
  show ();
  [%expect {|
    ((is_none false)
     (is_some true))
    |}]
;;

let%expect_test "[match%optional]" =
  let t = create () in
  let show () =
    print_s
      (let open Optional_syntax in
       match%optional t with
       | None -> [%message "none"]
       | Some i -> [%message "some" ~_:(i : int)])
  in
  show ();
  [%expect {| none |}];
  set_exn t [%here] 13;
  show ();
  [%expect {| (some 13) |}]
;;

let%expect_test "[iter]" =
  let t = create () in
  let iter () = iter t ~f:(fun i -> print_s [%message (i : int)]) in
  iter ();
  [%expect {| |}];
  set_exn t [%here] 13;
  iter ();
  [%expect {| (i 13) |}]
;;
