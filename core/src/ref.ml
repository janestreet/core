open! Import
open Base_quickcheck.Export

module T = struct
  include Base.Ref

  include (
    struct
      type 'a t = 'a ref [@@deriving bin_io ~localize, quickcheck, typerep]
    end :
      sig
        type 'a t = 'a ref [@@deriving bin_io ~localize, quickcheck, typerep]
      end
      with type 'a t := 'a t)
end

include T

module Permissioned = struct
  include T

  type ('a, -'perms) t = 'a T.t [@@deriving bin_io ~localize, sexp]

  let read_only = Fn.id
  let of_ref = Fn.id
  let to_ref = Fn.id
  let set = ( := )
  let get = ( ! )
end
