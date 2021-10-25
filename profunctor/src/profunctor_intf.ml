open Base

module Interfaces = struct
  (** A profunctor has an input end with contravariant map, an output
      end with covariant map and an operation to join two terms with
      the same input type giving both outputs.
  *)
  module type S = sig
    type ('b, 'a) t

    val map : ('a, 'u) t -> f:('a -> 'b) -> ('b, 'u) t
    val contra_map : ('u, 'b) t -> f:('a -> 'b) -> ('u, 'a) t
    val both : ('a, 'i) t -> ('b, 'i) t -> ('a * 'b, 'i) t
  end
  (** @open *)

  (** A module used to traverse each field of a record performing some
      action using a specific profunctor.

      You can construct a record builder for a given profunctor using
      {{!module-Profunctor.module-Record_builder}Record_builder}.

      This is based on the library {!module-Record_builder}
      (which provides traversal with an
      {{!modtype:Base.Applicative.S}applicative}) and adds
      the contravariant mapping of each field.

      {[
        type t = {
          name : string;
          age : int;
        } [@@deriving fields]

        module F : Profunctor.S = "your module here"
        module F_of_record = Profunctor.Record_builder(F)

        let for_string : (string, string) F.t = "your code here"
        let for_int : (int, int) F.t = "your code here"

        let example : (t, t) F.t =
          F_of_record.(build_for_record (
            Fields.make_creator
              ~name:(field for_string)
              ~age:(field for_int)))
      ]}

      This is equivalent to:

      {[
        let example : (t, t) F.t =
          let name = F.contra_map ~f:name for_string
          and age = F.contra_map ~f:age for_int
          in
          F.map (F.both name age) ~f:(fun (name, age) ->
            { name; age; })
        ;;
      ]}
  *)
  module type Record_builder = sig
    type ('b, 'a) profunctor

    (** A term of the profunctor where the input and output type are
        the same.

        Although this module must use the type parameters separately
        internally all terms supplied as arguments or returned will
        have both type parameters equal. This type alias is used to
        allow automatically converting with some other type in some
        cases e.g. {!module-Of_conv_based}.
    *)
    type 'a profunctor_term

    val prj : ('a, 'a) profunctor -> 'a profunctor_term
    val inj : 'a profunctor_term -> ('a, 'a) profunctor

    (** The underlying applicative record builder,
        which does not perform the contravariant mapping.
    *)
    module Bare : Record_builder.S2 with type ('b, 'a) applicative = ('b, 'a) profunctor

    (** Supply the term for one field.

        The type of this function is designed to match up with [Fields.make_creator]
        (see the example).
    *)
    val field
      :  'field profunctor_term
      -> ('record, 'field) Field.t
      -> ('field, _, _, 'record) Bare.Make_creator_types.handle_one_field

    (** Build the overarching profunctor for the whole record.

        This takes a partial application of [Fields.make_creator] as its argument,
        which should supply no initial value but use {!field} to supply a term
        for every field of the record.

        The type of this is designed to match up with [Fields.make_creator]
        (see the example).
    *)
    val build_for_record
      :  ('record, _, 'record) Bare.Make_creator_types.handle_all_fields
      -> 'record profunctor_term
  end

  (** A profunctor constructed from an {{!modtype:Base.Applicative.S}applicative}.

      In this case [contra_map] has no effect, and [map] and [both]
      behave exactly as they would with the underlying applicative.
  *)
  module type Of_applicative = sig
    type 'a applicative
    type ('b, 'a) t = 'b applicative

    include S with type ('b, 'a) t := ('b, 'a) t

    module Of_record :
      Record_builder
      with type 'a profunctor_term = 'a applicative
       and type ('b, 'a) profunctor = ('b, 'a) t
  end

  (** A profunctor-ish where both parameters must be mapped together
      at the same time. This is less expressive but appears in
      several libraries.
  *)
  module type Conv_based = sig
    type 'a t

    val conv : 'a t -> ('a -> 'b) -> ('b -> 'a) -> 'b t
    val both : 'a t -> 'b t -> ('a * 'b) t
  end

  (** Embed a {!modtype:Conv_based} profunctor-ish into a full profunctor,
      allowing the use of {!modtype:Record_builder} directly.
  *)
  module type Of_conv_based = sig
    type 'a conv_based
    type ('b, 'a) t

    include S with type ('b, 'a) t := ('b, 'a) t

    val inj : 'a conv_based -> ('a, 'a) t
    val prj : ('a, 'a) t -> 'a conv_based

    module Of_record :
      Record_builder
      with type 'a profunctor_term = 'a conv_based
       and type ('b, 'a) profunctor = ('b, 'a) t
  end
end

module type Profunctor = sig
  include module type of Interfaces (** @inline *)

  module Record_builder (F : S) :
    Record_builder
    with type ('b, 'a) profunctor = ('b, 'a) F.t
     and type 'a profunctor_term = ('a, 'a) F.t

  module Of_applicative (F : Applicative.S) :
    Of_applicative with type 'a applicative := 'a F.t

  module Of_conv_based (F : Conv_based) : Of_conv_based with type 'a conv_based := 'a F.t

  (** A profunctor which represents a function where
      [Fn.id] may sometimes be distinguished from other
      functions.

      This is primarily used internally to implement other
      things where discarding identity functions can be advantageous.
  *)
  module Fn_with_id : sig
    type ('b, 'a) t =
      | Id : ('a, 'a) t
      | Apply : ('a -> 'b) -> ('b, 'a) t

    include S with type ('b, 'a) t := ('b, 'a) t

    val id : ('a, 'a) t
    val of_fn : ('a -> 'b) -> ('b, 'a) t

    (** Unpack the function, [as_fn (of_fn x) ≡ Staged.stage x]. *)
    val as_fn : ('b, 'a) t -> ('a -> 'b) Staged.t

    (** [split l r ≡ both (contra_map ~f:fst l) (contra_map ~f:snd r)],
        but is more efficient (the result may be [Id]).
    *)
    val split : ('b, 'a) t -> ('d, 'c) t -> ('b * 'd, 'a * 'c) t

    (** Composition of function, [as_fn (compose g f) ≡ Fn.compose (as_fn g) (as_fn f)]. *)
    val compose : ('c, 'b) t -> ('b, 'a) t -> ('c, 'a) t

    module Of_record :
      Record_builder
      with type ('b, 'a) profunctor = ('b, 'a) t
       and type 'a profunctor_term = ('a, 'a) t
  end
end
