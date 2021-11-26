(** This module extends {!Base.Binary_searchable}. *)

open Base.Binary_searchable

module type S1_permissions = sig
  open Perms.Export

  type ('a, -'perms) t

  val binary_search : (('a, [> read ]) t, 'a, 'key) binary_search
  val binary_search_segmented : (('a, [> read ]) t, 'a) binary_search_segmented
end

module type Binary_searchable = sig
  include module type of struct
    include Base.Binary_searchable
  end

  module type S1_permissions = S1_permissions
end
