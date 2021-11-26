open! Import

module type S = sig
  type 'a t [@@deriving bin_io]

  include Base.Queue.S with type 'a t := 'a t (** @inline *)
end
