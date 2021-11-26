(** This module extends the Base [Blit] module *)

open Base.Blit

(*_ These are not implemented less-general-in-terms-of-more-general because odoc produces
  unreadable documentation in that case, with or without [inline] on [include]. *)

module type S_permissions = sig
  open Perms.Export

  type -'perms t

  val blit : ([> read ] t, [> write ] t) blit
  val blito : ([> read ] t, [> write ] t) blito
  val unsafe_blit : ([> read ] t, [> write ] t) blit
  val sub : ([> read ] t, [< _ perms ] t) sub
  val subo : ([> read ] t, [< _ perms ] t) subo
end

module type S1_permissions = sig
  open Perms.Export

  type ('a, -'perms) t

  val blit : (('a, [> read ]) t, ('a, [> write ]) t) blit
  val blito : (('a, [> read ]) t, ('a, [> write ]) t) blito
  val unsafe_blit : (('a, [> read ]) t, ('a, [> write ]) t) blit
  val sub : (('a, [> read ]) t, ('a, [< _ perms ]) t) sub
  val subo : (('a, [> read ]) t, ('a, [< _ perms ]) t) subo
end

module type Blit = sig
  (** @inline *)
  include module type of struct
    include Base.Blit
  end

  module type S_permissions = S_permissions
  module type S1_permissions = S1_permissions
end
