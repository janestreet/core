open! Import
open! Std_internal

module type S = sig
  type t [@@deriving hash]

  include Identifiable with type t := t

  (** [create index ~min ~max] raises if [index < min || index > max].  The resulting [t]
      is only equal to other [t] if all three fields are the same. *)
  val create : int -> min:int -> max:int -> t

  (** all indices in ascending order *)
  val create_all : min:int -> max:int -> t list

  (** Accessors. *)
  val index : t -> int

  val min_index : t -> int
  val max_index : t -> int

  module Stable : sig
    module V1 :
      Stable_comparable.V1
      with type t = t
      with type comparator_witness = comparator_witness
  end
end

module type Bounded_index = sig
  (** [Bounded_index] creates unique index types with explicit bounds and human-readable
      labels. "(thing 2 of 0 to 4)" refers to a 0-based index for the third element of
      five with the label "thing", whereas a 1-based index for the second element of
      twelve with the label "item" renders as "(item 2 of 1 to 12)", even though both
      represent the index 2.

      Use [Bounded_index] to help distinguish between different index types when reading
      rendered values, deserializing sexps, and typechecking. Consider using
      [Bounded_index] to label fixed pools of resources such as cores in a cpu, worker
      processes in a parallel application, or machines in a cluster. *)

  module type S = S

  module Make (M : sig
      val label : string
      val module_name : string
    end) : S
end
