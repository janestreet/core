open! Base
include Blit_helpers_intf
open Expect_test_helpers_base
open Base_quickcheck
open Generator.Let_syntax

module Int_array_option = struct
  type t = int array option [@@deriving equal, sexp_of]
end

let index_of array =
  let%map i = Generator.int_uniform_inclusive (-2) (Array.length array + 1) in
  if i = -2 then None else Some i
;;

let shrink_array array = Shrinker.shrink [%shrinker: int array] array

let shrink_index = function
  | None -> Sequence.empty
  | Some n ->
    (match Int.sign n with
     | Zero -> Sequence.return None
     | Pos -> Sequence.of_list [ None; Some (n - 1) ]
     | Neg -> Sequence.of_list [ None; Some (n + 1) ])
;;

module For_sub = struct
  type t =
    { array : int array
    ; pos : int option
    ; len : int option
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let%bind array = [%generator: int array] in
    let%bind pos = index_of array in
    let%bind len = index_of array in
    Generator.return { array; pos; len }
  ;;

  let quickcheck_shrinker =
    Shrinker.create (fun { array; pos; len } ->
      Sequence.round_robin
        [ Sequence.map (shrink_array array) ~f:(fun array -> { array; pos; len })
        ; Sequence.map (shrink_index pos) ~f:(fun pos -> { array; pos; len })
        ; Sequence.map (shrink_index len) ~f:(fun len -> { array; pos; len })
        ])
  ;;
end

module For_blit = struct
  type t =
    { src : int array
    ; src_pos : int option
    ; dst : int array
    ; dst_pos : int option
    ; len : int option
    }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let%bind src = [%generator: int array] in
    let%bind dst = [%generator: int array] in
    let%bind src_pos = index_of src in
    let%bind dst_pos = index_of dst in
    let%bind len = index_of src in
    Generator.return { src; src_pos; dst; dst_pos; len }
  ;;

  let quickcheck_shrinker =
    Shrinker.create (fun { src; src_pos; dst; dst_pos; len } ->
      Sequence.round_robin
        [ Sequence.map (shrink_array src) ~f:(fun src ->
            { src; src_pos; dst; dst_pos; len })
        ; Sequence.map (shrink_index src_pos) ~f:(fun src_pos ->
            { src; src_pos; dst; dst_pos; len })
        ; Sequence.map (shrink_array dst) ~f:(fun dst ->
            { src; src_pos; dst; dst_pos; len })
        ; Sequence.map (shrink_index dst_pos) ~f:(fun dst_pos ->
            { src; src_pos; dst; dst_pos; len })
        ; Sequence.map (shrink_index len) ~f:(fun len ->
            { src; src_pos; dst; dst_pos; len })
        ])
  ;;
end

module Make (Src : Blittable) (Dst : Blittable) = struct
  include M (Src) (Dst)

  let test_sub_internal sub =
    quickcheck_m (module For_sub) ~f:(fun { array; pos; len } ->
      require_equal
        (module Int_array_option)
        (Option.try_with (fun () -> Dst.to_array (sub (Src.of_array array) ~pos ~len)))
        (Option.try_with (fun () -> Array.subo array ?pos ?len)))
  ;;

  let test_sub ~sub:{ sub } =
    test_sub_internal (fun src ~pos ~len ->
      let pos, len =
        Ordered_collection_common.get_pos_len_exn
          ?pos
          ?len
          ()
          ~total_length:(Src.length src)
      in
      sub src ~pos ~len)
  ;;

  let test_subo ~subo:{ subo } =
    test_sub_internal (fun src ~pos ~len -> subo ?pos ?len src)
  ;;

  let test_blit_internal blit =
    quickcheck_m (module For_blit) ~f:(fun { src; src_pos; dst; dst_pos; len } ->
      require_equal
        (module Int_array_option)
        (Option.try_with (fun () ->
           let src = Src.of_array (Array.copy src) in
           let dst = Dst.of_array (Array.copy dst) in
           blit ~src ~src_pos ~dst ~dst_pos ~len;
           Dst.to_array dst))
        (Option.try_with (fun () ->
           let src = Array.copy src in
           let dst = Array.copy dst in
           Array.blito ~src ?src_pos ?src_len:len ~dst ?dst_pos ();
           dst)))
  ;;

  let test_blit ~blit:{ blit } =
    test_blit_internal (fun ~src ~src_pos ~dst ~dst_pos ~len ->
      let src_pos = Option.value src_pos ~default:0 in
      let dst_pos = Option.value dst_pos ~default:0 in
      let len = Option.value len ~default:(Src.length src - src_pos) in
      blit ~src ~src_pos ~dst ~dst_pos ~len)
  ;;

  let test_blito ~blito:{ blito } =
    test_blit_internal (fun ~src ~src_pos ~dst ~dst_pos ~len ->
      blito ~src ?src_pos ?src_len:len ~dst ?dst_pos ())
  ;;

  let test_unsafe_blit ~unsafe_blit:{ unsafe_blit } =
    test_blit_internal (fun ~src ~src_pos ~dst ~dst_pos ~len ->
      let src_pos = Option.value src_pos ~default:0 in
      let dst_pos = Option.value dst_pos ~default:0 in
      let len = Option.value len ~default:(Src.length src - src_pos) in
      Ordered_collection_common.check_pos_len_exn
        ~pos:src_pos
        ~len
        ~total_length:(Src.length src);
      Ordered_collection_common.check_pos_len_exn
        ~pos:dst_pos
        ~len
        ~total_length:(Dst.length dst);
      unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len)
  ;;
end
