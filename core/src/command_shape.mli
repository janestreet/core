open! Import
open! Std_internal
include module type of Command.Shape

module%template Stable : sig
  module Anons : sig
    module Grammar : sig
      module V1 : Stable_without_comparator [@mode local] with type t = Anons.Grammar.t
    end

    module V2 : Stable_without_comparator [@mode local] with type t = Anons.t
  end

  module Help_category : sig
    module V1 : Stable_without_comparator [@mode local] with type t = Help_category.t
  end

  module Flag_info : sig
    module V2 : Stable_without_comparator [@mode local] with type t = Flag_info.t

    module V1 : sig
      include
        Stable_without_comparator
        [@mode local]
        with type t = Command.Shape.Stable.Flag_info.V1.t

      val to_latest : t -> V2.t
      val of_latest : V2.t -> t
    end
  end

  module Base_info : sig
    module V3 : Stable_without_comparator [@mode local] with type t = Base_info.t

    module V2 : sig
      include
        Stable_without_comparator
        [@mode local]
        with type t = Command.Shape.Stable.Base_info.V2.t

      val to_latest : t -> V3.t
      val of_latest : V3.t -> t
    end
  end

  module Group_info : sig
    module V2 : Stable1 with type 'a t = 'a Group_info.t
  end

  module Exec_info : sig
    module V3 : Stable_without_comparator [@mode local] with type t = Exec_info.t
  end

  module Fully_forced : sig
    module V2 : Stable_without_comparator [@mode local] with type t = Fully_forced.t

    module V1 : sig
      include
        Stable_without_comparator
        [@mode local]
        with type t = Command.Shape.Stable.Fully_forced.V1.t
      [@@alert "-deprecated"]

      val to_latest : t -> V2.t
      val of_latest : V2.t -> t
    end
    [@@deprecated "[since 2026-08] Update to [Fully_forced.V2]."]
  end
end
