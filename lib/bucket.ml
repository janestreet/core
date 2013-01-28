open Std_internal

module type Contents =
sig
  type t
  include Sexpable with type t := t
  include Binable with type t := t
  include Comparable with type t := t
  val zero : t
  val (+) : t -> t -> t
  val (-) : t -> t -> t
end

module type S = sig
  type contents
  type t
  include Sexpable with type t := t
  include Binable with type t := t

  (* Fails if init_level is not within bounds [zero;size]. *)
  val create : size:contents -> init_level:contents -> t

  (* the current bucket level *)
  val level : t -> contents

  (* Take some exact amount out of the bucket and return `Taken. If there is not enough
   * left in the bucket, return `Unable. *)
  val take : t -> contents -> [ `Taken | `Unable ]

  (* Take some amount out of the bucket, possibly emptying it. The return value is the
   * amount that was actually taken out. *)
  val take_at_most : t -> contents -> contents

  (* Put some amount into the bucket, possibly overflowing. *)
  val fill : t -> contents -> unit
end

module Make (C: Contents): (S with type contents = C.t) =
struct
  type contents = C.t
  with sexp, bin_io

  type t = {
    mutable level : contents;
    size : contents;
  }
  with sexp, bin_io


  let create ~size ~init_level =
    let error msg =
      failwithf "Bucket.create ~size:%s ~init_level:%s: %s"
        (Sexp.to_string (C.sexp_of_t size))
        (Sexp.to_string (C.sexp_of_t init_level))
        msg ();
    in
    if C.(<) init_level C.zero then error "init_level negative";
    if C.(>) init_level size then error "init_level above bucket size";
    { level = init_level; size = size }
  ;;

  let level t = t.level

  let assert_positive name x =
    if C.(<) x C.zero
    then invalid_argf "Bucket.%s %s < 0" name (Sexp.to_string (C.sexp_of_t x)) ()

  let take t x =
    assert_positive "take" x;
    let new_level = C.(-) t.level x in
    if C.(<) new_level C.zero then
      `Unable
    else begin
      t.level <- new_level;
      `Taken
    end

  let take_at_most t x =
    assert_positive "take_at_most" x;
    let old_level = t.level in
    t.level <- C.max C.zero (C.(-) old_level x);
    C.(-) old_level t.level

  let fill t x =
    assert_positive "fill" x;
    let old_level = t.level in
    t.level <- C.min t.size (C.(+) old_level x)
end

module Int = Make (Int)
module Int64 = Make (Int64)
module Float = Make (Float)
