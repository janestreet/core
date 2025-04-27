open! Base

module type Blittable = sig
  type 'a t [@@deriving quickcheck, equal, sexp_of]

  val length : _ t -> int
  val of_array : 'a array -> 'a t
  val to_array : 'a t -> 'a array
end

module M (Src : T1) (Dst : T1) = struct
  type 'a src = 'a Src.t
  type 'a dst = 'a Dst.t
  type sub = { sub : 'a. 'a src -> pos:int -> len:int -> 'a dst }
  type subo = { subo : 'a. ?pos:int -> ?len:int -> 'a src -> 'a dst }

  type blit =
    { blit : 'a. src:'a src -> src_pos:int -> dst:'a dst -> dst_pos:int -> len:int -> unit
    }

  type blito =
    { blito :
        'a.
        src:'a src
        -> ?src_pos:int
        -> ?src_len:int
        -> dst:'a dst
        -> ?dst_pos:int
        -> unit
        -> unit
    }

  type unsafe_blit =
    { unsafe_blit :
        'a. src:'a src -> src_pos:int -> dst:'a dst -> dst_pos:int -> len:int -> unit
    }
end

module type Helpers = sig
  module Src : T1
  module Dst : T1

  include module type of struct
    include M (Src) (Dst)
  end

  val test_sub : sub:sub -> unit
  val test_subo : subo:subo -> unit
  val test_blit : blit:blit -> unit
  val test_blito : blito:blito -> unit
  val test_unsafe_blit : unsafe_blit:unsafe_blit -> unit
end

module type Blit_helpers = sig
  module type Blittable = Blittable
  module type Helpers = Helpers

  module Make (Src : Blittable) (Dst : Blittable) :
    Helpers with module Src := Src and module Dst := Dst
end
