type t = bool

open Interfaces
include Comparable with type t := t
include Hashable   with type t := t
include Sexpable   with type t := t
include Binable    with type t := t
include Stringable with type t := t

module True_  : Default.S with type real = t
module False_ : Default.S with type real = t
