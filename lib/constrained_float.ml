open Std_internal 

module type S = sig
  type t = private float
  include Sexpable with type t := t
  include Binable with type t := t
  include Comparable_binable with type t := t
  include Hashable_binable with type t := t
  include Robustly_comparable with type t := t
  include Stringable with type t := t
  include Floatable with type t := t
end
