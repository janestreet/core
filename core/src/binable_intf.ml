open! Import
open Bin_prot.Binable
open Bigarray

[%%template
[@@@mode.default m = (global, local)]

module type Conv_without_uuid = sig
  type binable : value_or_null
  type t : value_or_null

  val to_binable : t @ m -> binable @ m [@@mode m = (global, m)]
  val of_binable : binable -> t
end

module type Conv = sig
  include Conv_without_uuid [@mode m]

  val caller_identity : Bin_prot.Shape.Uuid.t
end

[@@@kind.default ka = (value, any)]

module type Conv1_without_uuid = sig
  type ('a : ka) binable : value_or_null
  type ('a : ka) t : value_or_null

  val to_binable : ('a : ka). 'a t @ m -> 'a binable @ m [@@mode m = (global, m)]
  val of_binable : ('a : ka). 'a binable -> 'a t
end

module type Conv1 = sig
  include Conv1_without_uuid [@kind ka] [@mode m]

  val caller_identity : Bin_prot.Shape.Uuid.t
end

[@@@kind.default kb = (value, any)]

module type Conv2_without_uuid = sig
  type ('a : ka, 'b : kb) binable : value_or_null
  type ('a : ka, 'b : kb) t : value_or_null

  val to_binable : ('a : ka) ('b : kb). ('a, 'b) t @ m -> ('a, 'b) binable @ m
  [@@mode m = (global, m)]

  val of_binable : ('a : ka) ('b : kb). ('a, 'b) binable -> ('a, 'b) t
end

module type Conv2 = sig
  include Conv2_without_uuid [@kind ka kb] [@mode m]

  val caller_identity : Bin_prot.Shape.Uuid.t
end

[@@@kind.default kc = (value, any)]

module type Conv3_without_uuid = sig
  type ('a : ka, 'b : kb, 'c : kc) binable : value_or_null
  type ('a : ka, 'b : kb, 'c : kc) t : value_or_null

  val to_binable
    : ('a : ka) ('b : kb) ('c : kc).
    ('a, 'b, 'c) t @ m -> ('a, 'b, 'c) binable @ m
  [@@mode m = (global, m)]

  val of_binable : ('a : ka) ('b : kb) ('c : kc). ('a, 'b, 'c) binable -> ('a, 'b, 'c) t
end

module type Conv3 = sig
  include Conv3_without_uuid [@kind ka kb kc] [@mode m]

  val caller_identity : Bin_prot.Shape.Uuid.t
end]

module type Conv_sexpable = sig
  type t : value_or_null

  include Sexpable.S with type t := t

  val caller_identity : Bin_prot.Shape.Uuid.t
end

module type Conv_stringable = sig
  include Stringable.S

  val caller_identity : Bin_prot.Shape.Uuid.t
end

(** Module types and utilities for dealing with types that support the bin-io binary
    encoding. *)
module type Binable0 = sig @@ portable
  (** We copy the definition of the bigstring type here, because we cannot depend on
      bigstring.ml *)
  type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

  module%template Minimal : sig
    [@@@mode.default m = (global, local)]

    module type S = Minimal.S [@mode m]

    [@@@kind.default ka = (value, any)]

    module type S1 = Minimal.S1 [@kind ka] [@mode m]

    [@@@kind.default kb = (value, any)]

    module type S2 = Minimal.S2 [@kind ka kb] [@mode m]

    [@@@kind.default kc = (value, any)]

    module type S3 = Minimal.S3 [@kind ka kb kc] [@mode m]
  end

  [%%template:
  [@@@mode.default m = (global, local)]

  (** New code should use [@@deriving bin_io]. These module types ([S], [S1], and [S2])
      are exported only for backwards compatibility. *)

  module type S = S [@mode m]
  module type S_only_functions = S_only_functions [@mode m]
  module type S1 = S1 [@mode m]
  module type S2 = S2 [@mode m]
  module type S3 = S3 [@mode m]
  module type Conv = Conv [@mode m]
  module type Conv1 = Conv1 [@mode m]
  module type Conv2 = Conv2 [@mode m]
  module type Conv3 = Conv3 [@mode m]
  module type Conv_without_uuid = Conv_without_uuid [@mode m]
  module type Conv1_without_uuid = Conv1_without_uuid [@mode m]
  module type Conv2_without_uuid = Conv2_without_uuid [@mode m]
  module type Conv3_without_uuid = Conv3_without_uuid [@mode m]

  (** [Of_binable*] functors are for when you want the binary representation of one type
      to be the same as that for some other isomorphic type. *)

  module%template.portable Of_binable_with_uuid
      (Binable : Minimal.S
    [@mode m])
      (M : Conv [@mode m] with type binable := Binable.t) : S [@mode m] with type t := M.t

  module%template.portable Of_binable_without_uuid
      (Binable : Minimal.S
    [@mode m])
      (M : Conv_without_uuid [@mode m] with type binable := Binable.t) :
    S [@mode m] with type t := M.t
  [@@alert legacy "Use [Of_binable_with_uuid] if possible."]

  [@@@kind.default ka = (value, any)]

  module%template.portable Of_binable1_with_uuid
      (Binable : Minimal.S1
    [@kind ka] [@mode m])
      (M : Conv1 [@kind ka] [@mode m] with type ('a : ka) binable := 'a Binable.t) :
    S1 [@kind ka] [@mode m] with type ('a : ka) t := 'a M.t

  module%template.portable Of_binable1_without_uuid
      (Binable : Minimal.S1
    [@kind ka] [@mode m])
      (M : Conv1_without_uuid
           [@kind ka] [@mode m]
           with type ('a : ka) binable := 'a Binable.t) :
    S1 [@kind ka] [@mode m] with type ('a : ka) t := 'a M.t
  [@@alert legacy "Use [Of_binable1_with_uuid] if possible."]

  [@@@kind.default kb = (value, any)]

  module%template.portable Of_binable2_with_uuid
      (Binable : Minimal.S2
    [@kind ka kb] [@mode m])
      (M : Conv2
           [@kind ka kb] [@mode m]
           with type ('a : ka, 'b : kb) binable := ('a, 'b) Binable.t) :
    S2 [@kind ka kb] [@mode m] with type ('a : ka, 'b : kb) t := ('a, 'b) M.t

  module%template.portable Of_binable2_without_uuid
      (Binable : Minimal.S2
    [@kind ka kb] [@mode m])
      (M : Conv2_without_uuid
           [@kind ka kb] [@mode m]
           with type ('a : ka, 'b : kb) binable := ('a, 'b) Binable.t) :
    S2 [@kind ka kb] [@mode m] with type ('a : ka, 'b : kb) t := ('a, 'b) M.t
  [@@alert legacy "Use [Of_binable2_with_uuid] if possible."]

  [@@@kind.default kc = (value, any)]

  module%template.portable Of_binable3_with_uuid
      (Binable : Minimal.S3
    [@kind ka kb kc] [@mode m])
      (M : Conv3
           [@kind ka kb kc] [@mode m]
           with type ('a : ka, 'b : kb, 'c : kc) binable := ('a, 'b, 'c) Binable.t) :
    S3
    [@kind ka kb kc] [@mode m]
    with type ('a : ka, 'b : kb, 'c : kc) t := ('a, 'b, 'c) M.t

  module%template.portable Of_binable3_without_uuid
      (Binable : Minimal.S3
    [@kind ka kb kc] [@mode m])
      (M : Conv3_without_uuid
           [@kind ka kb kc] [@mode m]
           with type ('a : ka, 'b : kb, 'c : kc) binable := ('a, 'b, 'c) Binable.t) :
    S3
    [@kind ka kb kc] [@mode m]
    with type ('a : ka, 'b : kb, 'c : kc) t := ('a, 'b, 'c) M.t
  [@@alert legacy "Use [Of_binable3_with_uuid] if possible."]]

  module type Conv_sexpable = Conv_sexpable
  module type Conv_stringable = Conv_stringable

  (** [Of_sexpable_with_uuid] serializes a value using the bin-io of the sexp
      serialization of the value. This is not as efficient as using [@@deriving bin_io].
      However, it is useful when performance isn't important and there are obstacles to
      using [@@deriving bin_io], e.g., some type missing [@@deriving bin_io].
      [Of_sexpable_with_uuid] is also useful when one wants to be forgiving about format
      changes, due to the sexp serialization being more robust to changes like adding or
      removing a constructor. *)

  module%template.portable Of_sexpable_with_uuid (M : Conv_sexpable) :
    S with type t := M.t

  module%template.portable Of_stringable_with_uuid (M : Conv_stringable) :
    S with type t := M.t

  module%template.portable Of_sexpable_without_uuid (M : sig
      type t : value_or_null

      include Sexpable.S with type t := t
    end) : S with type t := M.t
  [@@alert legacy "Use [Of_sexpable_with_uuid] if possible."]

  module%template.portable Of_stringable_without_uuid (M : Stringable.S) :
    S with type t := M.t
  [@@alert legacy "Use [Of_stringable_with_uuid] if possible."]

  [%%template:
  [@@@mode.default m = (global, local)]

  type ('a : any) m = ((module S with type t = 'a)[@mode m])

  val of_bigstring : ('a : value_or_null). ('a m[@mode m]) -> bigstring -> 'a

  val to_bigstring
    : ('a : value_or_null).
    ?prefix_with_length:bool (** defaults to false *)
    -> ('a m[@mode m])
    -> 'a @ m
    -> bigstring]

  (** The following functors preserve stability: if applied to stable types with stable
      (de)serializations, they will produce stable types with stable (de)serializations.

      Note: In all cases, stability of the input (and therefore the output) depends on the
      semantics of all conversion functions (e.g. [to_string], [to_sexpable]) not changing
      in the future. *)

  module%template Stable : sig
    module Of_binable : sig
      [@@@mode.default m = (global, local)]
      [@@@modality.default p = (portable, nonportable)]

      module V1 : (module type of Of_binable_without_uuid [@mode m] [@modality p])
      [@alert "-legacy"]
      [@@alert legacy "Use [V2] instead."]

      module V2 : module type of Of_binable_with_uuid [@mode m] [@modality p]
    end

    module Of_binable1 : sig
      [@@@mode.default m = (global, local)]
      [@@@modality.default p = (portable, nonportable)]

      module V1 : (module type of Of_binable1_without_uuid [@mode m] [@modality p])
      [@alert "-legacy"]
      [@@alert legacy "Use [V2] instead."]

      module V2 : module type of Of_binable1_with_uuid [@mode m] [@modality p]
    end

    module Of_binable2 : sig
      [@@@mode.default m = (global, local)]
      [@@@modality.default p = (portable, nonportable)]

      module V1 : (module type of Of_binable2_without_uuid [@mode m] [@modality p])
      [@alert "-legacy"]
      [@@alert legacy "Use [V2] instead."]

      module V2 : module type of Of_binable2_with_uuid [@mode m] [@modality p]
    end

    module Of_binable3 : sig
      [@@@mode.default m = (global, local)]
      [@@@modality.default p = (portable, nonportable)]

      module V1 : (module type of Of_binable3_without_uuid [@mode m] [@modality p])
      [@alert "-legacy"]
      [@@alert legacy "Use [V2] instead."]

      module V2 : module type of Of_binable3_with_uuid [@mode m] [@modality p]
    end

    module Of_sexpable : sig
      [@@@modality.default p = (portable, nonportable)]

      module V1 : (module type of Of_sexpable_without_uuid [@modality p])
      [@alert "-legacy"]
      [@@alert legacy "Use [V2] instead."]

      module V2 : module type of Of_sexpable_with_uuid [@modality p]
    end

    module Of_stringable : sig
      [@@@modality.default p = (portable, nonportable)]

      module V1 : (module type of Of_stringable_without_uuid [@modality p])
      [@alert "-legacy"]
      [@@alert legacy "Use [V2] instead."]

      module V2 : module type of Of_stringable_with_uuid [@modality p]
    end
  end
end

module type Binable = sig @@ portable
  include Binable0

  [%%template:
  [@@@mode.default m = (global, local)]

  val of_string : ('a : value_or_null). ('a m[@mode m]) -> string @ local -> 'a
  val to_string : ('a : value_or_null). ('a m[@mode m]) -> 'a @ m -> string]
end
