open Typerep.Std

(* adding with_typerep to some stable types *)
include module type of Core.Std
  with module Month    := Core.Std.Month
   and module Date     := Core.Std.Date
   and module Time     := Core.Std.Time

module Month : sig
  include module type of Core.Std.Month with module Stable := Core.Std.Month.Stable
  module Stable : sig
    module V1 : sig
      include module type of Core.Std.Month.Stable.V1
      include Typerepable.S0 with type t := t
    end
  end
end

module Date : sig
  include module type of Core.Std.Date with module Stable := Core.Std.Date.Stable
  module Stable : sig
    module V1 : sig
      include module type of Core.Std.Date.Stable.V1
      include Typerepable.S0 with type t := t
    end
  end
end

module Time : sig
  include module type of Core.Std.Time with module Stable := Core.Std.Time.Stable
  module Stable : sig
    module V1 : sig
      include module type of Core.Std.Time.Stable.V1
      include Typerepable.S0 with type t := t
    end
  end
end

(* additional functor *)
open Typestruct.Std
module Serializable_of_typestructable(T : Typestructable.S0) : sig
  type t
  include Typestructable.S0 with type t := T.t
  include Binable.S with type t := t
  include Sexpable.S with type t := t
  val t_of_string : string -> t
  val string_of_t : t -> string
  val t_of_bigstring : Bigstring.t -> t
  val bigstring_of_t : t -> Bigstring.t
end
