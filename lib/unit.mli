type t = unit

include Identifiable.S with type t := t

module type S = sig end

type m = (module S)
