open Base
include Profunctor_intf.Interfaces

module Record_builder_internal
    (F : S) (T : sig
               type 'a profunctor_term

               val prj : ('a, 'a) F.t -> 'a profunctor_term
               val inj : 'a profunctor_term -> ('a, 'a) F.t
             end) =
struct
  include T

  type ('b, 'a) profunctor = ('b, 'a) F.t

  module Bare = Record_builder.Make_2 (F)

  let field term field = Bare.field (F.contra_map (inj term) ~f:(Field.get field)) field
  let build_for_record f = prj (Bare.build_for_record f)
end

module Record_builder (F : S) =
  Record_builder_internal
    (F)
    (struct
      type 'a profunctor_term = ('a, 'a) F.t

      let prj = Fn.id
      let inj = Fn.id
    end)

module Fn_with_id = struct
  module T = struct
    type ('b, 'a) t =
      | Id : ('a, 'a) t
      | Apply : ('a -> 'b) -> ('b, 'a) t

    let map (type a b c) (x : (b, a) t) ~(f : b -> c) : (c, a) t =
      match x with
      | Id -> Apply f
      | Apply g -> Apply (Fn.compose f g)
    ;;

    let contra_map (type a b c) (x : (c, b) t) ~(f : a -> b) : (c, a) t =
      match x with
      | Id -> Apply f
      | Apply g -> Apply (Fn.compose g f)
    ;;

    let as_fn' (type a b) (x : (b, a) t) : a -> b =
      match x with
      | Id -> Fn.id
      | Apply f -> f
    ;;

    let both l r =
      let l = as_fn' l
      and r = as_fn' r in
      Apply (fun x -> l x, r x)
    ;;
  end

  include T

  let split (type a b c d) (l : (b, a) t) (r : (d, c) t) : (b * d, a * c) t =
    match l, r with
    | Id, Id -> Id
    | _, _ ->
      let l = as_fn' l
      and r = as_fn' r in
      Apply (fun (x, y) -> l x, r y)
  ;;

  let id = Id
  let of_fn x = Apply x
  let as_fn t = Staged.stage (as_fn' t)

  let compose (type a b c) (g : (c, b) t) (f : (b, a) t) : (c, a) t =
    match g, f with
    | Id, Id -> Id
    | Id, f -> f
    | g, Id -> g
    | Apply g, Apply f -> Apply (Fn.compose g f)
  ;;

  module Of_record = Record_builder (T)
end

module Of_applicative (F : Applicative.S) = struct
  module T = struct
    type ('b, 'a) t = 'b F.t

    let contra_map x ~f:_ = x
    let map = F.map
    let both = F.both
  end

  include T

  module Of_record =
    Record_builder_internal
      (T)
      (struct
        type 'a profunctor_term = 'a F.t

        let inj = Fn.id
        let prj = Fn.id
      end)
end

module Of_conv_based (F : Conv_based) = struct
  module T = struct
    type ('c, 'a) t =
      | Embed :
          { pre_map : ('b, 'a) Fn_with_id.t
          ; inner : 'b F.t
          ; post_map : ('c, 'b) Fn_with_id.t
          }
          -> ('c, 'a) t

    let contra_map (Embed x) ~f =
      Embed { x with pre_map = Fn_with_id.contra_map x.pre_map ~f }
    ;;

    let map (Embed x) ~f = Embed { x with post_map = Fn_with_id.map x.post_map ~f }

    let both (Embed l) (Embed r) =
      Embed
        { pre_map = Fn_with_id.both l.pre_map r.pre_map
        ; inner = F.both l.inner r.inner
        ; post_map = Fn_with_id.split l.post_map r.post_map
        }
    ;;
  end

  include T

  let inj inner = Embed { pre_map = Fn_with_id.Id; inner; post_map = Fn_with_id.Id }

  let prj (Embed x) =
    F.conv x.inner (Fn_with_id.as_fn' x.post_map) (Fn_with_id.as_fn' x.pre_map)
  ;;

  module Of_record =
    Record_builder_internal
      (T)
      (struct
        type 'a profunctor_term = 'a F.t

        let inj = inj
        let prj = prj
      end)
end
