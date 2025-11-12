open! Import

[@@@warning "-incompatible-with-upstream"]

[%%template
[@@@mode.default m = (global, local)]

module type S = sig
  type t : any
  type value : any

  module Optional_syntax : sig
    val is_none : t @ local -> bool
    val unsafe_value : t @ m -> value @ m [@@mode m = (global, m)]
  end
end

module type S_zero_alloc = sig
  type t : any
  type value : any

  module Optional_syntax : sig
    val is_none : t @ local -> bool [@@zero_alloc]
    val unsafe_value : t @ m -> value @ m [@@zero_alloc] [@@mode m = (global, m)]
  end
end

[@@@kind.default ka = base_with_imm]

module type S1 = sig
  type ('a : ka) t : any
  type ('a : ka) value : any

  module Optional_syntax : sig
    val is_none : _ t @ local -> bool
    val unsafe_value : 'a t @ m -> 'a value @ m [@@mode m = (global, m)]
  end
end

module type S1_zero_alloc = sig
  type ('a : ka) t : any
  type ('a : ka) value : any

  module Optional_syntax : sig
    val is_none : _ t @ local -> bool [@@zero_alloc]
    val unsafe_value : 'a t @ m -> 'a value @ m [@@zero_alloc] [@@mode m = (global, m)]
  end
end

[@@@kind.default kb = base_with_imm]

module type S2 = sig
  type ('a : ka, 'b : kb) t : any
  type ('a : ka, 'b : kb) value : any

  module Optional_syntax : sig
    val is_none : _ t @ local -> bool
    val unsafe_value : ('a, 'b) t @ m -> ('a, 'b) value @ m [@@mode m = (global, m)]
  end
end

module type S2_zero_alloc = sig
  type ('a : ka, 'b : kb) t : any
  type ('a : ka, 'b : kb) value : any

  module Optional_syntax : sig
    val is_none : _ t @ local -> bool [@@zero_alloc]

    val unsafe_value : ('a, 'b) t @ m -> ('a, 'b) value @ m
    [@@zero_alloc] [@@mode m = (global, m)]
  end
end]

module type%template Optional_syntax = sig @@ portable
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

      The reason for the double [module Optional_syntax] is historical. The idiom used to
      use [open M.Optional_syntax], and we wanted that to bring into scope as little as
      possible, so we made it put in scope only [module Optional_syntax].

      [unsafe_value] does not have to be memory-safe if not guarded by [is_none].

      Implementations of [is_none] and [unsafe_value] must not have any side effects. More
      precisely, if you mutate any value currently being match'ed on (not necessarily your
      own argument) you risk a segfault as well.

      This is because [match%optional] does not make any guarantee about [is_none] call
      being immediately followed by the corresponding [unsafe_value] call. In fact it
      makes several [is_none] calls followed by several [unsafe_value] calls, so in the
      presence of side-effects by the time it makes an [unsafe_value] call the result of
      the corresponding [is_none] can go stale.

      For more details on the syntax extension, see [ppx/ppx_optional/README.md]. *)

  [@@@mode.default m = (global, local)]

  module type S = S [@mode m]
  module type S_zero_alloc = S_zero_alloc [@mode m]

  [@@@kind.default ka = base_with_imm]

  module type S1 = S1 [@kind ka] [@mode m]
  module type S1_zero_alloc = S1_zero_alloc [@kind ka] [@mode m]

  [@@@kind.default kb = base_with_imm]

  module type S2 = S2 [@kind ka kb] [@mode m]
  module type S2_zero_alloc = S2_zero_alloc [@kind ka kb] [@mode m]
end
