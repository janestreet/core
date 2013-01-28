module type S = sig
  type t
  include Stringable.S         with type t := t
  include Comparable.S_binable with type t := t
  include Hashable  .S_binable with type t := t
  include Sexpable  .S         with type t := t
  include Binable   .S         with type t := t
  val pp : Format.formatter -> t -> unit
end

module Of_stringable_sexpable (T : sig
  include Stringable.S
  include Sexpable.S with type t := t
end) = struct
  module T' = struct
    include T
    include (Binable.Of_stringable (T) : Binable.S with type t := t)
    let equal t t' = Core_string.equal (T.to_string t) (T.to_string t')
    let hash t = Core_string.hash (T.to_string t)
    let compare t t' = Core_string.compare (T.to_string t) (T.to_string t')
  end
  include T'
  include (Comparable.Make_binable (T') : Comparable.S_binable with type t := t)
  include (Hashable  .Make_binable (T') : Hashable  .S_binable with type t := t)
  let pp formatter t = Core_string.pp formatter (T.to_string t)
end

module Of_stringable (T : Stringable.S) =
  Of_stringable_sexpable (struct
    include T
    include (Sexpable.Of_stringable(T) : Sexpable.S with type t := t)
  end)

module Of_sexpable (T : Sexpable.S) =
  Of_stringable_sexpable (struct
    include T
    include Sexpable.To_stringable (T)
  end)
