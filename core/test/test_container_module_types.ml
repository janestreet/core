(** This file tests the consistency of [Container] and [Indexed_container] module types.

    We compare each module type S to the most generic version G that exports the same set
    of values. We create a module type I by instantiating G to mimic S, such as by
    dropping a type parameter. We then test that S = I by writing two identity functors,
    one from S to I and one from I to S. *)

open! Core

module For_container = struct
  module type%template Generic_permissions = sig
    type 'a elt
    type ('a, 'phantom1, 'phantom2, -'permissions) t

    val length : (_, _, _, [> read ]) t -> int
    val is_empty : (_, _, _, [> read ]) t -> bool

    val mem
      :  ('a, _, _, [> read ]) t
      -> 'a elt
      -> equal:('a elt -> 'a elt -> bool)
      -> bool
    [@@mode m = (global, m)]

    val iter : ('a, _, _, [> read ]) t -> f:('a elt -> unit) -> unit
    [@@mode m = (global, m)]

    val iter_until
      :  ('a, _, _, [> read ]) t
      -> f:('a elt -> (unit, 'final) Continue_or_stop.t)
      -> finish:(unit -> 'final)
      -> 'final
    [@@mode mi = (global, m), mo = (global, m)]

    val fold
      :  ('a, _, _, [> read ]) t
      -> init:'accum
      -> f:('accum -> 'a elt -> 'accum)
      -> 'accum
    [@@mode mi = (global, m), mo = (global, m)]

    val fold_result
      :  ('a, _, _, [> read ]) t
      -> init:'accum
      -> f:('accum -> 'a elt -> ('accum, 'e) Result.t)
      -> ('accum, 'e) Result.t
    [@@mode mi = (global, m), mo = (global, m)]

    val fold_until
      :  ('a, _, _, [> read ]) t
      -> init:'accum
      -> f:('accum -> 'a elt -> ('accum, 'final) Continue_or_stop.t)
      -> finish:('accum -> 'final)
      -> 'final
    [@@mode mi = (global, m), mo = (global, m)]

    val exists : ('a, _, _, [> read ]) t -> f:('a elt -> bool) -> bool
    [@@mode m = (global, m)]

    val for_all : ('a, _, _, [> read ]) t -> f:('a elt -> bool) -> bool
    [@@mode m = (global, m)]

    val count : ('a, _, _, [> read ]) t -> f:('a elt -> bool) -> int
    [@@mode m = (global, m)]

    val sum
      :  ((module Container.Summable with type t = 'sum)[@mode mo])
      -> ('a, _, _, [> read ]) t
      -> f:('a elt -> 'sum)
      -> 'sum
    [@@mode mi = (global, m), mo = (global, m)]

    val find : ('a, _, _, [> read ]) t -> f:('a elt -> bool) -> 'a elt option
    [@@mode m = (global, m)]

    val find_map : ('a, _, _, [> read ]) t -> f:('a elt -> 'b option) -> 'b option
    [@@mode mi = (global, m), mo = (global, m)]

    val to_list : ('a, _, _, [> read ]) t -> 'a elt list
    [@@alloc a @ m = (heap_global, a @ m)]

    val to_array : ('a, _, _, [> read ]) t -> 'a elt array

    val min_elt
      :  ('a, _, _, [> read ]) t
      -> compare:('a elt -> 'a elt -> int)
      -> 'a elt option
    [@@mode m = (global, m)]

    val max_elt
      :  ('a, _, _, [> read ]) t
      -> compare:('a elt -> 'a elt -> int)
      -> 'a elt option
    [@@mode m = (global, m)]
  end
  [@@alloc a @ m = (heap_global, stack_local)]

  module type%template Generic_with_creators_permissions = sig
    type (_, _, _, _) concat

    include Generic_permissions [@alloc a]

    val of_list : 'a elt list -> ('a, 'p1, 'p2, [< _ perms ]) t
    [@@alloc __ @ m = (heap_global, a @ m)]

    val of_array : 'a elt array -> ('a, 'p1, 'p2, [< _ perms ]) t
    [@@alloc __ @ m = (heap_global, a @ m)]

    val append
      :  ('a, 'p1, 'p2, [> read ]) t
      -> ('a, 'p1, 'p2, [> read ]) t
      -> ('a, 'p1, 'p2, [< _ perms ]) t
    [@@alloc __ @ m = (heap_global, a @ m)]

    val concat
      :  (('a, 'p1, 'p2, [> read ]) t, 'p1, 'p2, [> read ]) concat
      -> ('a, 'p1, 'p2, [< _ perms ]) t
    [@@alloc __ @ m = (heap_global, a @ m)]

    val map
      :  ('a, 'p1, 'p2, [> read ]) t
      -> f:('a elt -> 'b elt)
      -> ('b, 'p1, 'p2, [< _ perms ]) t
    [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]

    val filter
      :  ('a, 'p1, 'p2, [> read ]) t
      -> f:('a elt -> bool)
      -> ('a, 'p1, 'p2, [< _ perms ]) t
    [@@alloc __ @ m = (heap_global, a @ m)]

    val filter_map
      :  ('a, 'p1, 'p2, [> read ]) t
      -> f:('a elt -> 'b elt option)
      -> ('b, 'p1, 'p2, [< _ perms ]) t
    [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]

    val concat_map
      :  ('a, 'p1, 'p2, [> read ]) t
      -> f:('a elt -> ('b, 'p1, 'p2, [> read ]) t)
      -> ('b, 'p1, 'p2, [< _ perms ]) t
    [@@mode mi = (global, m)] [@@alloc a @ mo = (heap_global, a @ m)]

    val partition_tf
      :  ('a, 'p1, 'p2, [> read ]) t
      -> f:('a elt -> bool)
      -> ('a, 'p1, 'p2, [< _ perms ]) t * ('a, 'p1, 'p2, [< _ perms ]) t
    [@@alloc __ @ m = (heap_global, a @ m)]

    val partition_map
      :  ('a, 'p1, 'p2, [> read ]) t
      -> f:('a elt -> ('b elt, 'c elt) Either.t)
      -> ('b, 'p1, 'p2, [< _ perms ]) t * ('c, 'p1, 'p2, [< _ perms ]) t
    [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]
  end
  [@@alloc a @ m = (heap_global, stack_local)]
end

module For_indexed_container = struct
  module type%template Generic_permissions = sig
    include For_container.Generic_permissions [@alloc a]

    (** These are all like their equivalents in [Container] except that an index starting
        at 0 is added as the first argument to [f]. *)

    val foldi
      :  ('a, 'p1, 'p2, [> read ]) t
      -> init:'acc
      -> f:(int -> 'acc -> 'a elt -> 'acc)
      -> 'acc
    [@@mode mi = (global, m), mo = (global, m)]

    val foldi_until
      :  ('a, 'p1, 'p2, [> read ]) t
      -> init:'acc
      -> f:(int -> 'acc -> 'a elt -> ('acc, 'final) Continue_or_stop.t)
      -> finish:(int -> 'acc -> 'final)
      -> 'final
    [@@mode mi = (global, m), mo = (global, m)]

    val iteri : ('a, 'p1, 'p2, [> read ]) t -> f:(int -> 'a elt -> unit) -> unit
    [@@mode m = (global, m)]

    val iteri_until
      :  ('a, 'p1, 'p2, [> read ]) t
      -> f:(int -> 'a elt -> (unit, 'final) Continue_or_stop.t)
      -> finish:(int -> 'final)
      -> 'final
    [@@mode mi = (global, m), mo = (global, m)]

    val existsi : ('a, 'p1, 'p2, [> read ]) t -> f:(int -> 'a elt -> bool) -> bool
    [@@mode m = (global, m)]

    val for_alli : ('a, 'p1, 'p2, [> read ]) t -> f:(int -> 'a elt -> bool) -> bool
    [@@mode m = (global, m)]

    val counti : ('a, 'p1, 'p2, [> read ]) t -> f:(int -> 'a elt -> bool) -> int
    [@@mode m = (global, m)]

    val findi
      :  ('a, 'p1, 'p2, [> read ]) t
      -> f:(int -> 'a elt -> bool)
      -> (int * 'a elt) option
    [@@mode m = (global, m)]

    val find_mapi
      :  ('a, 'p1, 'p2, [> read ]) t
      -> f:(int -> 'a elt -> 'b option)
      -> 'b option
    [@@mode mi = (global, m), mo = (global, m)]
  end
  [@@alloc a @ m = (heap_global, stack_local)]

  module type%template Generic_with_creators_permissions = sig
    include For_container.Generic_with_creators_permissions [@alloc a]

    include
      Generic_permissions
      [@alloc a]
      with type 'a elt := 'a elt
       and type ('a, 'b, 'c, 'd) t := ('a, 'b, 'c, 'd) t

    val init : int -> f:(int -> 'a elt) -> ('a, 'p1, 'p2, [< _ perms ]) t
    [@@alloc __ @ m = (heap_global, a @ m)]

    val mapi
      :  ('a, 'p1, 'p2, [> read ]) t
      -> f:(int -> 'a elt -> 'b elt)
      -> ('b, 'p1, 'p2, [< _ perms ]) t
    [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]

    val filteri
      :  ('a, 'p1, 'p2, [> read ]) t
      -> f:(int -> 'a elt -> bool)
      -> ('a, 'p1, 'p2, [< _ perms ]) t
    [@@alloc __ @ m = (heap_global, a @ m)]

    val filter_mapi
      :  ('a, 'p1, 'p2, [> read ]) t
      -> f:(int -> 'a elt -> 'b elt option)
      -> ('b, 'p1, 'p2, [< _ perms ]) t
    [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]

    val concat_mapi
      :  ('a, 'p1, 'p2, [> read ]) t
      -> f:(int -> 'a elt -> ('b, 'p1, 'p2, [> read ]) t)
      -> ('b, 'p1, 'p2, [< _ perms ]) t
    [@@mode mi = (global, m)] [@@alloc a @ mo = (heap_global, a @ m)]

    val partitioni_tf
      :  ('a, 'p1, 'p2, [> read ]) t
      -> f:(int -> 'a elt -> bool)
      -> ('a, 'p1, 'p2, [< _ perms ]) t * ('a, 'p1, 'p2, [< _ perms ]) t
    [@@alloc __ @ m = (heap_global, a @ m)]

    val partition_mapi
      :  ('a, 'p1, 'p2, [> read ]) t
      -> f:(int -> 'a elt -> ('b elt, 'c elt) Either.t)
      -> ('b, 'p1, 'p2, [< _ perms ]) t * ('c, 'p1, 'p2, [< _ perms ]) t
    [@@mode mi = (global, m)] [@@alloc __ @ mo = (heap_global, a @ m)]
  end
  [@@alloc a @ m = (heap_global, stack_local)]
end

module%template _ : module type of Container = struct
  include Base.Container
  open For_container

  [@@@alloc.default a = (heap, stack)]

  (* Ensure that Generic_permissions without the permissions is just Generic. *)
  open struct
    module type Generic_without_permissions = sig
      type ('a, 'phantom1, 'phantom2) t

      include
        Generic_permissions
        [@alloc a]
        with type ('a, 'phantom1, 'phantom2, _) t := ('a, 'phantom1, 'phantom2) t
    end
    [@@alloc a = (heap, stack)]
  end

  module _ (M : Generic [@alloc a]) : Generic_without_permissions [@alloc a] = M
  module _ (M : Generic_without_permissions [@alloc a]) : Generic [@alloc a] = M

  (* Ensure that Generic_with_creators_permissions is Generic_with_creators with
     permissions, and that it includes Generic_permissions. *)

  open struct
    module type Generic_with_creators_without_permissions = sig
      type ('a, 'phantom1, 'phantom2) t
      type ('a, 'phantom1, 'phantom2) concat

      include
        Generic_with_creators_permissions
        [@alloc a]
        with type ('a, 'phantom1, 'phantom2, _) t := ('a, 'phantom1, 'phantom2) t
         and type ('a, 'phantom1, 'phantom2, _) concat :=
          ('a, 'phantom1, 'phantom2) concat
    end
    [@@alloc a = (heap, stack)]
  end

  module _ (M : Generic_with_creators [@alloc a]) :
    Generic_with_creators_without_permissions [@alloc a] =
    M

  module _ (M : Generic_with_creators_without_permissions [@alloc a]) :
    Generic_with_creators [@alloc a] =
    M

  module _ (M : Generic_with_creators_permissions [@alloc a]) : Generic_permissions
  [@alloc a] =
    M

  (* Ensure that S0_permissions is Generic_permissions with no type parameter. *)
  module type S0_permissions = Container.S0_permissions [@alloc a]

  open struct
    module type Generic0_permissions = sig
      type elt
      type -_ t

      include
        Generic_permissions
        [@alloc a]
        with type 'a elt := elt
         and type (_, _, _, 'p) t := 'p t

      val mem : [> read ] t -> elt -> bool [@@mode m = (global, m)]
    end
    [@@alloc a @ m = (heap_global, stack_local)]
  end

  module _ (M : S0_permissions [@alloc a]) : Generic0_permissions [@alloc a] = M
  module _ (M : Generic0_permissions [@alloc a]) : S0_permissions [@alloc a] = M

  (* Ensure that S1_permissions is Generic_permissions with no [elt] type. *)
  module type S1_permissions = Container.S1_permissions [@alloc a]

  open struct
    module type Generic1_permissions = sig
      type (_, -_) t

      include
        Generic_permissions
        [@alloc a]
        with type 'a elt := 'a
         and type ('a, _, _, 'p) t := ('a, 'p) t
    end
    [@@alloc a = (heap, stack)]
  end

  module _ (M : S1_permissions [@alloc a]) : Generic1_permissions [@alloc a] = M
  module _ (M : Generic1_permissions [@alloc a]) : S1_permissions [@alloc a] = M

  (* Ensure that S1_with_creators_permissions is Generic_with_creators_permissions with no
     [elt] type. *)
  module type S1_with_creators_permissions = Container.S1_with_creators_permissions
  [@alloc a]

  open struct
    module type Generic1_with_creators_permissions = sig
      type (_, -_) t

      include
        Generic_with_creators_permissions
        [@alloc a]
        with type 'a elt := 'a
         and type ('a, _, _, 'p) t := ('a, 'p) t
         and type ('a, _, _, 'p) concat := ('a, 'p) t
    end
    [@@alloc a = (heap, stack)]
  end

  module _ (M : S1_with_creators_permissions [@alloc a]) :
    Generic1_with_creators_permissions [@alloc a] =
    M

  module _ (M : Generic1_with_creators_permissions [@alloc a]) :
    S1_with_creators_permissions [@alloc a] =
    M
end

module%template _ : module type of Indexed_container = struct
  include Base.Indexed_container
  open For_indexed_container

  [@@@alloc.default a = (heap, stack)]

  (* Ensure that Generic_permissions without the permissions is just Generic. *)
  open struct
    module type Generic_without_permissions = sig
      type ('a, 'phantom1, 'phantom2) t

      include
        Generic_permissions
        [@alloc a]
        with type ('a, 'phantom1, 'phantom2, _) t := ('a, 'phantom1, 'phantom2) t
    end
    [@@alloc a = (heap, stack)]
  end

  module _ (M : Generic [@alloc a]) : Generic_without_permissions [@alloc a] = M
  module _ (M : Generic_without_permissions [@alloc a]) : Generic [@alloc a] = M

  (* Ensure that Generic_with_creators_permissions is Generic_with_creators with
     permissions, and that it includes Generic_permissions. *)

  open struct
    module type Generic_with_creators_without_permissions = sig
      type ('a, 'phantom1, 'phantom2) t
      type ('a, 'phantom1, 'phantom2) concat

      include
        Generic_with_creators_permissions
        [@alloc a]
        with type ('a, 'phantom1, 'phantom2, _) t := ('a, 'phantom1, 'phantom2) t
         and type ('a, 'phantom1, 'phantom2, _) concat :=
          ('a, 'phantom1, 'phantom2) concat
    end
    [@@alloc a = (heap, stack)]
  end

  module _ (M : Generic_with_creators [@alloc a]) :
    Generic_with_creators_without_permissions [@alloc a] =
    M

  module _ (M : Generic_with_creators_without_permissions [@alloc a]) :
    Generic_with_creators [@alloc a] =
    M

  module _ (M : Generic_with_creators_permissions [@alloc a]) : Generic_permissions
  [@alloc a] =
    M

  (* Ensure that S1_permissions is Generic_permissions with no [elt] type. *)
  module type S1_permissions = Indexed_container.S1_permissions [@alloc a]

  open struct
    module type Generic1_permissions = sig
      type (_, -_) t

      include
        Generic_permissions
        [@alloc a]
        with type 'a elt := 'a
         and type ('a, _, _, 'perms) t := ('a, 'perms) t
    end
    [@@alloc a = (heap, stack)]
  end

  module _ (M : S1_permissions [@alloc a]) : Generic1_permissions [@alloc a] = M
  module _ (M : Generic1_permissions [@alloc a]) : S1_permissions [@alloc a] = M

  (* Ensure that S1_with_creators_permissions is Generic_with_creators_permissions with no
     [elt] type. *)
  module type S1_with_creators_permissions =
    Indexed_container.S1_with_creators_permissions [@alloc a]

  open struct
    module type Generic1_with_creators_permissions = sig
      type (_, -_) t

      include
        Generic_with_creators_permissions
        [@alloc a]
        with type 'a elt := 'a
         and type ('a, _, _, 'p) t := ('a, 'p) t
         and type ('a, _, _, 'p) concat := ('a, 'p) t
    end
    [@@alloc a = (heap, stack)]
  end

  module _ (M : S1_with_creators_permissions [@alloc a]) :
    Generic1_with_creators_permissions [@alloc a] =
    M

  module _ (M : Generic1_with_creators_permissions [@alloc a]) :
    S1_with_creators_permissions [@alloc a] =
    M
end
