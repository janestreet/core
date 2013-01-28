module type T = sig type t end

module Make (T1 : T) (T2 : T) = struct type t = T1.t * T2.t end

module T2 = struct
  type ('a, 'b) t = 'a * 'b with sexp

  let create a b = (a, b)

  let curry f = (); fun a b -> f (a, b)

  let uncurry f = (); fun (a,b) -> f a b

  external get1 : ('a, _) t -> 'a = "%field0"
  external get2 : (_, 'a) t -> 'a = "%field1"

  let map1 ~f (x,y) = (f x, y)

  let map2 ~f (x,y) = (x, f y)

  let compare ~cmp1 ~cmp2 =
    fun (x, y) (x', y') ->
      match cmp1 x x' with
      | 0 -> cmp2 y y'
      | i -> i
  ;;

  let swap (a, b) = (b, a)
end

module T3 = struct
  type ('a, 'b, 'c) t = 'a * 'b * 'c with sexp

  let create a b c = (a, b, c)

  let curry f = (); fun a b c -> f (a,b,c)

  let uncurry f = (); fun (a,b,c) -> f a b c

  let map1 ~f (x,y,z) = (f x, y, z)

  let map2 ~f (x,y,z) = (x, f y, z)

  let map3 ~f (x,y,z) = (x, y, f z)

  external get1 : ('a, _, _) t -> 'a = "%field0"
  external get2 : (_, 'a, _) t -> 'a = "%field1"
  (* There's no %field2....*)
  let get3 (_, _, a) = a

  (* lexicographic comparison  *)
  let compare ~cmp1 ~cmp2 ~cmp3 =
    fun (x, y, z) (x', y', z') ->
      let c1 = cmp1 x x' in
      if c1 <> 0 then c1 else
        let c2 = cmp2 y y' in
        if c2 <> 0 then c2 else
          cmp3 z z'
  ;;

end

module type Comparable_sexpable = sig
  include Comparable.S
  include Sexpable.S with type t := t
end

module type Hashable_sexpable = sig
  type t
  include Hashable.S with type t := t
  include Sexpable.S with type t := t
end

module Sexpable (S1 : Sexpable.S) (S2 : Sexpable.S) = struct
  type t = S1.t * S2.t with sexp
end

module Comparable (S1 : Comparable_sexpable) (S2 : Comparable_sexpable) = struct
  module T = struct
    include Sexpable (S1) (S2)

    let compare (s1, s2) (s1', s2') =
      match S1.compare s1 s1' with
      | 0 -> S2.compare s2 s2'
      | x -> x

  end

  include T
  include Comparable.Make (T)
end

module Hashable_t (S1 : Hashable_sexpable) (S2 : Hashable_sexpable)
  = struct
  module T = struct
    include Sexpable (S1) (S2)

    let compare (s1, s2) (s1', s2') =
      match S1.compare s1 s1' with
      | 0 -> S2.compare s2 s2'
      | x -> x

    (* The [land] ensures the return value is the same in 32 and 64-bit processes *)
    let hash (s1, s2) = (S1.hash s1 + S2.hash s2 * 65599) land 0x3FFFFFFF
  end
  include T
  include Hashable.Make (T)
end

module Hashable = Hashable_t

