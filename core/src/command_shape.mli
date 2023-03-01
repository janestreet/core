open! Import
open! Std_internal
include module type of Command.Shape

module Stable : sig
  module Anons : sig
    module Grammar : sig
      module V1 : Stable_without_comparator with type t = Anons.Grammar.t
    end

    module V2 : Stable_without_comparator with type t = Anons.t
  end

  module Flag_info : sig
    module V1 : Stable_without_comparator with type t = Flag_info.t
  end

  module Base_info : sig
    module V2 : Stable_without_comparator with type t = Base_info.t
  end

  module Group_info : sig
    module V2 : Stable1 with type 'a t = 'a Group_info.t
  end

  module Exec_info : sig
    module V3 : Stable_without_comparator with type t = Exec_info.t
  end

  module Fully_forced : sig
    module V1 : Stable_without_comparator with type t = Fully_forced.t
  end
end
