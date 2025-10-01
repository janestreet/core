open! Import

(** Extension to the base signature *)
module type Extension = sig @@ portable
  type t
  [@@deriving
    bin_io ~localize, diff ~how:"atomic" ~extra_derive:[ sexp ], globalize, quickcheck]

  type info := t

  module Portable : sig
    type t = info Modes.Portable.t
    [@@deriving
      bin_io ~localize, diff ~how:"atomic" ~extra_derive:[ sexp ], globalize, quickcheck]

    include Base.Info.Portable with type info := info and type t := t
  end

  module Stable : sig
    (** [Info.t] is wire-compatible with [V2.t], but not [V1.t]. [V1] bin-prots a sexp of
        the underlying message, whereas [V2] bin-prots the underlying message. *)
    module V1 : sig
      include%template
        Stable_module_types.With_stable_witness.S0 [@mode local] with type t = t

      (** Explicitly indicate that [t_of_sexp] produces a portable [t]. This is nicer for
          the user: you can do more things with a portable [t], e.g. move it between
          domains. *)
      val t_of_sexp : Sexplib.Sexp.t -> t @ portable
    end

    module V2 : sig
      type nonrec t = t
      [@@deriving
        globalize
        , equal ~localize
        , hash
        , sexp_grammar
        , diff ~extra_derive:[ sexp; bin_io ]]

      include%template
        Stable_module_types.With_stable_witness.S0 [@mode local] with type t := t

      (** See comment on [V1.t_of_sexp]. *)
      val t_of_sexp : Sexplib.Sexp.t -> t @ portable
    end

    module Portable : sig
      module V1 : sig
        (** Wire compatible with [Core.Info.Stable.V1] *)
        type t = Portable.t

        include%template
          Stable_module_types.With_stable_witness.S0 [@mode local] with type t := t
      end

      module V2 : sig
        (** Wire compatible with [Core.Info.Stable.V2] *)
        type t = Portable.t
        [@@deriving
          globalize
          , equal ~localize
          , hash
          , sexp_grammar
          , diff ~extra_derive:[ sexp; bin_io ]]

        include%template
          Stable_module_types.With_stable_witness.S0 [@mode local] with type t := t
      end
    end
  end
end

module type Info = sig @@ portable
  module type S = Base.Info.S

  include S with type t = Base.Info.t (** @inline *)

  module Internal_repr : Base.Info.Internal_repr with type info := t
  include Extension with type t := t
  module Extend (Info : Base.Info.S) : Extension with type t := Info.t
end
