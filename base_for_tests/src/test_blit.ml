open! Base
open! Blit
include Test_blit_intf.Definitions

module type For_tests = sig
  module Elt : Elt1
  module Src : Sequence1_phantom2 with type 'a elt := 'a Elt.t

  module Dst : sig
    include Sequence1_phantom2 with type 'a elt := 'a Elt.t

    val overlapping_src_dst
      : [ `Do_not_check | `Check of ('elt, 'p1, 'p2) Src.t -> ('elt, 'p1, 'p2) t ]
  end
end

module Test_gen
    (For_tests : For_tests)
    (Tested : S1_phantom2_distinct
              with type ('elt, 'p1, 'p2) src := ('elt, 'p1, 'p2) For_tests.Src.t
              with type ('elt, 'p1, 'p2) dst := ('elt, 'p1, 'p2) For_tests.Dst.t) =
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
    (Sequence : Sequence1 with type 'a elt := 'a)
    (Tested : S1 with type 'a t := 'a Sequence.t) =
  Test_gen
    (struct
      module Elt = struct
        type 'a t = 'a

        let equal = Poly.equal
        let of_bool = Fn.id
      end

      module Src = struct
        include Sequence

        type ('a, _, _) t = 'a Sequence.t
      end

      module Dst = struct
        include Sequence

        type ('a, _, _) t = 'a Sequence.t

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

      module Src = struct
        include Sequence

        type ('a, _, _) t = 'a Sequence.t
      end

      module Dst = struct
        include Sequence

        type ('a, _, _) t = 'a Sequence.t

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

      module Src = struct
        open Sequence

        type nonrec (_, _, _) t = t

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

      module Src = struct
        open Src

        type nonrec (_, _, _) t = t

        let length = length
        let get = get
        let set = set
        let create_bool = create
      end

      module Dst = struct
        open Dst

        type nonrec (_, _, _) t = t

        let length = length
        let get = get
        let set = set
        let create_bool = create
        let overlapping_src_dst = `Do_not_check
      end
    end)
    (Tested)

module Test1_phantom2
    (Elt : Elt1)
    (Sequence : Sequence1_phantom2 with type 'a elt := 'a Elt.t)
    (Tested : S1_phantom2_distinct
              with type ('elt, 'p1, 'p2) src := ('elt, 'p1, 'p2) Sequence.t
               and type ('elt, 'p1, 'p2) dst := ('elt, 'p1, 'p2) Sequence.t) =
  Test_gen
    (struct
      module Elt = Elt
      module Src = Sequence

      module Dst = struct
        include Sequence

        let overlapping_src_dst = `Check Fn.id
      end
    end)
    (Tested)

module Test1_phantom2_distinct
    (Elt : Elt1)
    (Src : Sequence1_phantom2 with type 'a elt := 'a Elt.t)
    (Dst : Sequence1_phantom2 with type 'a elt := 'a Elt.t)
    (Tested : S1_phantom2_distinct
              with type ('elt, 'p1, 'p2) src := ('elt, 'p1, 'p2) Src.t
               and type ('elt, 'p1, 'p2) dst := ('elt, 'p1, 'p2) Dst.t) =
  Test_gen
    (struct
      module Elt = Elt
      module Src = Src

      module Dst = struct
        include Dst

        let overlapping_src_dst = `Do_not_check
      end
    end)
    (Tested)

module%template.portable
  [@modality p] Make_and_test
    (Elt : Elt)
    (Sequence : sig
       include Sequence with type elt := Elt.t

       val unsafe_blit : (t, t) blit
     end) =
struct
  module B = Make [@modality p] (Sequence)
  include Test (Elt) (Sequence) (B)
  include B
end

module%template.portable
  [@modality p] Make_distinct_and_test
    (Elt : Elt)
    (Src : Sequence with type elt := Elt.t)
    (Dst : sig
       include Sequence with type elt := Elt.t

       val unsafe_blit : (Src.t, t) blit
     end) =
struct
  module B = Make_distinct [@modality p] (Src) (Dst)
  include Test_distinct (Elt) (Src) (Dst) (B)
  include B
end

module Make1_and_test (Sequence : sig
    include Blit.Sequence1
    include Sequence1 with type 'a t := 'a t with type 'a elt := 'a
  end) =
struct
  module B = Make1 (Sequence)
  include Test1 (Sequence) (B)
  include B
end

module Make1_generic_and_test
    (Elt : Elt1)
    (Sequence : sig
       include Blit.Sequence1
       include Sequence1 with type 'a t := 'a t with type 'a elt := 'a Elt.t
     end) =
struct
  module B = Make1 (Sequence)
  include Test1_generic (Elt) (Sequence) (B)
  include B
end

module Make1_phantom2_and_test
    (Elt : Elt1)
    (Sequence : sig
       type (_, _, _) t

       val get : ('elt, _, _) t -> int -> 'elt Elt.t
       val set : ('elt, _, _) t -> int -> 'elt Elt.t -> unit
       val length : local_ (_, _, _) t -> int
       val create_bool : len:int -> (bool, _, _) t
       val create_like : len:int -> local_ ('elt, _, _) t -> ('elt, _, _) t
       val unsafe_blit : (('elt, _, _) t, ('elt, _, _) t) Blit.blit
     end) =
struct
  module B = Make1_phantom2_distinct (Sequence) (Sequence)
  include Test1_phantom2 (Elt) (Sequence) (B)
  include B
end

module Make1_phantom2_distinct_and_test
    (Elt : Elt1)
    (Src : Sequence1_phantom2 with type 'a elt := 'a Elt.t)
    (Dst : sig
       include Sequence1_phantom2 with type 'a elt := 'a Elt.t

       val create_like : len:int -> local_ ('elt, _, _) Src.t -> ('elt, _, _) t
       val unsafe_blit : (('elt, _, _) Src.t, ('elt, _, _) t) blit
     end) =
struct
  module B = Make1_phantom2_distinct (Src) (Dst)
  include Test1_phantom2_distinct (Elt) (Src) (Dst) (B)
  include B
end
