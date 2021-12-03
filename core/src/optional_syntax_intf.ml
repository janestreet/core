open! Import

module type S = sig
  type t
  type value

  module Optional_syntax : sig
    val is_none : t -> bool
    val unsafe_value : t -> value
  end
end

module type S1 = sig
  type 'a t
  type 'a value

  module Optional_syntax : sig
    val is_none : _ t -> bool
    val unsafe_value : 'a t -> 'a value
  end
end

module type S2 = sig
  type ('a, 'b) t
  type ('a, 'b) value

  module Optional_syntax : sig
    val is_none : _ t -> bool
    val unsafe_value : ('a, 'b) t -> ('a, 'b) value
  end
end

module type Optional_syntax = sig
  (** Idiomatic usage is to have a module [M] like:

      {[
        module M : sig
          type t

          module Optional_syntax : Optional_syntax.S
            with type t := t
            with type value := ...
        end = struct
          ...

          module Optional_syntax = struct
            module Optional_syntax = struct
              let is_none = is_none
              let unsafe_value = unsafe_value
            end
          end
        end
      ]}

      Then, uses look like:

      {[
        match%optional.M m with
        | None   -> ?
        | Some v -> ?
      ]}

      [match%optional] then expands to references to [M.Optional_syntax]'s [is_none] and
      [unsafe_value] functions.

      The reason for the double [module Optional_syntax] is historical.  The idiom used to
      use [open M.Optional_syntax], and we wanted that to bring into scope as little as
      possible, so we made it put in scope only [module Optional_syntax].

      [unsafe_value] does not have to be memory-safe if not guarded by [is_none].

      Implementations of [is_none] and [unsafe_value] must not have any side effects.
      More precisely, if you mutate any value currently being match'ed on (not necessarily
      your own argument) you risk a segfault as well.

      This is because [match%optional] does not make any guarantee about [is_none] call
      being immediately followed by the corresponding [unsafe_value] call. In fact it
      makes several [is_none] calls followed by several [unsafe_value] calls, so in
      the presence of side-effects by the time it makes an [unsafe_value] call the result
      of the corresponding [is_none] can go stale.

      For more details on the syntax extension, see [ppx/ppx_optional/README.md]. *)

  module type S = S
  module type S1 = S1
  module type S2 = S2
end
