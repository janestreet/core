open Core_hashtbl_intf
open T

module Hashtbl = Core_hashtbl

module type S = sig
  type t
  module Hashable : T with type t = t
  val hash : t -> int
  val compare : t -> t -> int
  val hashable : t Hashtbl.Hashable.t
  module Table      : Hashtbl   .S with type key   = t
  module Hash_set   : Hash_set  .S with type elt   = t
  module Hash_queue : Hash_queue.S with type Key.t = t
  module Hash_heap  : Hash_heap .S with type Key.t = t
end

module Make (T : Hashtbl.Key) : S with type t := T.t = struct
  include T
  module Hashable = T
  module Table      = Hashtbl   .Make (T)
  module Hash_set   = Hash_set  .Make (T)
  module Hash_queue = Hash_queue.Make (T)
  module Hash_heap  = Hash_heap .Make (T)
  let hashable = Table.hashable
end

module type S_binable = sig
  type t
  module Hashable : T with type t = t
  val hash : t -> int
  val hashable : t Hashtbl.Hashable.t
  module Table : sig
    include Hashtbl.S with type key = t
    include Binable.S1 with type 'a t := 'a t
  end
  module Hash_set   : Hash_set  .S_binable with type elt   = t
  module Hash_queue : Hash_queue.S         with type Key.t = t
  module Hash_heap  : Hash_heap .S         with type Key.t = t
end

module Make_binable (T : sig
  include Hashtbl.Key
  include Binable.S with type t := t
end) : S_binable with type t := T.t = struct
  module Hashable = T
  module Table      = Hashtbl   .Make_binable (T)
  module Hash_set   = Hash_set  .Make_binable (T)
  module Hash_queue = Hash_queue.Make         (T)
  module Hash_heap  = Hash_heap .Make         (T)

  include T
  let hashable = Table.hashable
end
