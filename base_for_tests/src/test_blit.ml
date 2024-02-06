open! Base
open! Blit
include Test_blit_intf

module type S_gen = sig
  open Blit

  type 'a src
  type 'a dst

  (*  val blit        : ('a src, 'a dst) blit*)
  val blito : ('a src, 'a dst) blito

  (*  val unsafe_blit : ('a src, 'a dst) blit*)
  val sub : ('a src, 'a dst) sub

  (*val subo        : ('a src, 'a dst) subo*)
end

module type For_tests_gen = sig
  module Elt : sig
    type 'a t

    val equal : bool t -> bool t -> bool
    val of_bool : bool -> bool t
  end

  type 'a z

  module Src : sig
    type 'a t

    val length : _ t -> int
    val create_bool : len:int -> bool z t
    val get : 'a z t -> int -> 'a Elt.t
    val set : 'a z t -> int -> 'a Elt.t -> unit
  end

  module Dst : sig
    type 'a t

    val length : _ t -> int
    val create_bool : len:int -> bool z t
    val get : 'a z t -> int -> 'a Elt.t
    val set : 'a z t -> int -> 'a Elt.t -> unit
    val overlapping_src_dst : [ `Do_not_check | `Check of 'a Src.t -> 'a t ]
  end
end

module Test_gen
  (For_tests : For_tests_gen)
  (Tested : S_gen
              with type 'a src := 'a For_tests.Src.t
              with type 'a dst := 'a For_tests.Dst.t) =
struct
  open Tested
  open For_tests

  let init ~len ~create ~set ~f =
    let t = create ~len in
    for i = 0 to len - 1 do
      set t i (f i)
    done;
    t
  ;;

  let elt1 = Elt.of_bool true
  let elt2 = Elt.of_bool false
  let () = assert (not (Elt.equal elt1 elt2))
  let src_bit i = if i land 0x1 = 0 then elt1 else elt2
  let dst_bit i = if i land 0x1 = 0 then elt2 else elt1

  (* Test [blit]. *)
  let%test_unit _ =
    let n = 4 in
    for src_length = 0 to n do
      for dst_length = 0 to n do
        for src_pos = 0 to src_length do
          for dst_pos = 0 to dst_length do
            for src_len = 0 to min (src_length - src_pos) (dst_length - dst_pos) do
              try
                let is_in_range i = i >= dst_pos && i < dst_pos + src_len in
                let check length get name sequence ~expect =
                  for i = 0 to length sequence - 1 do
                    if not (Elt.equal (get sequence i) (expect i))
                    then raise_s [%message "bug" (name : string) (i : int)]
                  done
                in
                let check_src = check Src.length Src.get in
                let check_dst = check Dst.length Dst.get in
                let src =
                  init ~len:src_length ~create:Src.create_bool ~set:Src.set ~f:src_bit
                in
                assert (Src.length src = src_length);
                let dst =
                  init ~len:dst_length ~create:Dst.create_bool ~set:Dst.set ~f:dst_bit
                in
                assert (Dst.length dst = dst_length);
                let init_src () =
                  for i = 0 to src_length - 1 do
                    Src.set src i (src_bit i)
                  done
                in
                blito ~src ~src_pos ~src_len ~dst ~dst_pos ();
                check_src "blit src" src ~expect:src_bit;
                check_dst "blit dst" dst ~expect:(fun i ->
                  if is_in_range i then src_bit (src_pos + i - dst_pos) else dst_bit i);
                (match Dst.overlapping_src_dst with
                 | `Do_not_check -> ()
                 | `Check src_to_dst ->
                   if dst_pos + src_len <= src_length
                   then (
                     init_src ();
                     let dst = src_to_dst src in
                     if false
                     then (
                       blito ~src ~src_pos ~src_len ~dst ~dst_pos ();
                       check_dst "blit dst overlapping" dst ~expect:(fun i ->
                         src_bit (if is_in_range i then src_pos + i - dst_pos else i)))));
                (* Check [sub]. *)
                init_src ();
                let dst = sub src ~pos:src_pos ~len:src_len in
                check_src "sub src" src ~expect:src_bit;
                check_dst "sub dst" dst ~expect:(fun i -> src_bit (src_pos + i))
              with
              | exn ->
                raise_s
                  [%message
                    "bug"
                      (exn : exn)
                      (src_length : int)
                      (src_pos : int)
                      (dst_length : int)
                      (dst_pos : int)]
            done
          done
        done
      done
    done
  ;;

  let%test_unit _ =
    let src = init ~len:4 ~create:Src.create_bool ~set:Src.set ~f:src_bit in
    let dst = init ~len:8 ~create:Dst.create_bool ~set:Dst.set ~f:dst_bit in
    let assert_raises f = assert (Exn.does_raise f) in
    assert_raises (fun () -> blito ~src ~src_pos:(-1) ~src_len:4 ~dst ~dst_pos:0 ());
    assert_raises (fun () -> blito ~src ~src_pos:0 ~src_len:4 ~dst ~dst_pos:(-1) ());
    assert_raises (fun () -> blito ~src ~src_pos:5 ~src_len:1 ~dst ~dst_pos:0 ());
    assert_raises (fun () -> blito ~src ~src_pos:0 ~src_len:8 ~dst ~dst_pos:0 ());
    assert_raises (fun () -> blito ~src ~src_pos:0 ~src_len:4 ~dst ~dst_pos:5 ());
    assert_raises (fun () -> blito ~src ~src_pos:0 ~src_len:4 ~dst ~dst_pos:8 ())
  ;;
end

module Test1
  (Sequence : Sequence1 with type 'a elt := 'a poly)
  (Tested : S1 with type 'a t := 'a Sequence.t) =
  Test_gen
    (struct
      module Elt = struct
        type 'a t = 'a

        let equal = Poly.equal
        let of_bool = Fn.id
      end

      type 'a z = 'a Sequence.z

      module Src = Sequence

      module Dst = struct
        include Sequence

        let overlapping_src_dst = `Check Fn.id
      end
    end)
    (Tested)

module Test1_generic
  (Elt : Elt1)
  (Sequence : Sequence1 with type 'a elt := 'a Elt.t)
  (Tested : S1 with type 'a t := 'a Sequence.t) =
  Test_gen
    (struct
      module Elt = Elt

      type 'a z = 'a Sequence.z

      module Src = Sequence

      module Dst = struct
        include Sequence

        let overlapping_src_dst = `Check Fn.id
      end
    end)
    (Tested)

module Elt_to_elt1 (Elt : Elt) = struct
  type 'a t = Elt.t

  let equal = Elt.equal
  let of_bool = Elt.of_bool
end

module Test
  (Elt : Elt)
  (Sequence : Sequence with type elt := Elt.t)
  (Tested : S with type t := Sequence.t) =
  Test_gen
    (struct
      module Elt = Elt_to_elt1 (Elt)

      type 'a z = unit

      module Src = struct
        open Sequence

        type nonrec 'a t = t

        let length = length
        let get = get
        let set = set
        let create_bool = create
      end

      module Dst = struct
        include Src

        let overlapping_src_dst = `Check Fn.id
      end
    end)
    (Tested)

module Test_distinct
  (Elt : Elt)
  (Src : Sequence with type elt := Elt.t)
  (Dst : Sequence with type elt := Elt.t)
  (Tested : S_distinct with type src := Src.t with type dst := Dst.t) =
  Test_gen
    (struct
      module Elt = Elt_to_elt1 (Elt)

      type 'a z = unit

      module Src = struct
        open Src

        type nonrec 'a t = t

        let length = length
        let get = get
        let set = set
        let create_bool = create
      end

      module Dst = struct
        open Dst

        type nonrec 'a t = t

        let length = length
        let get = get
        let set = set
        let create_bool = create
        let overlapping_src_dst = `Do_not_check
      end
    end)
    (Tested)

module Make_and_test
  (Elt : Elt) (Sequence : sig
    include Sequence with type elt := Elt.t

    val unsafe_blit : (t, t) blit
  end) =
struct
  module B = Make (Sequence)
  include Test (Elt) (Sequence) (B)
  include B
end

module Make_distinct_and_test
  (Elt : Elt)
  (Src : Sequence with type elt := Elt.t) (Dst : sig
    include Sequence with type elt := Elt.t

    val unsafe_blit : (Src.t, t) blit
  end) =
struct
  module B = Make_distinct (Src) (Dst)
  include Test_distinct (Elt) (Src) (Dst) (B)
  include B
end

module Make1_and_test (Sequence : sig
  include Blit.Sequence1
  include Sequence1 with type 'a t := 'a t with type 'a elt := 'a poly
end) =
struct
  module B = Make1 (Sequence)
  include Test1 (Sequence) (B)
  include B
end

module Make1_generic_and_test
  (Elt : Elt1) (Sequence : sig
    include Blit.Sequence1
    include Sequence1 with type 'a t := 'a t with type 'a elt := 'a Elt.t
  end) =
struct
  module B = Make1_generic (Sequence)
  include Test1_generic (Elt) (Sequence) (B)
  include B
end
