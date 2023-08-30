open! Core

(** A subset of [Hashtbl_intf.Hashtbl]. Leaves out definitions that we do not need to test
    and that some implementations may not provide. *)
module type Hashtbl_for_testing = sig
  type ('a, 'b) t [@@deriving sexp_of]

  include
    Hashtbl_intf.Creators
      with type ('a, 'b) t := ('a, 'b) t
      with type 'a key := 'a
      with type ('a, 'b, 'c) create_options :=
        ('a, 'b, 'c) Hashtbl_intf.create_options_with_first_class_module

  (* [Creators] gives us a different create than [Hashtbl_intf.Hashtbl] does *)
  val create : ?growth_allowed:bool -> ?size:int -> 'a Base.Hashtbl.Key.t -> ('a, 'b) t

  include Hashtbl_intf.Accessors with type ('a, 'b) t := ('a, 'b) t with type 'a key := 'a
  include Hashtbl_intf.Multi with type ('a, 'b) t := ('a, 'b) t with type 'a key := 'a

  val invariant : 'a Invariant.t -> 'b Invariant.t -> ('a, 'b) t Invariant.t
end

module type Hashtbl_unit_tests = sig
  module type Hashtbl_for_testing = Hashtbl_for_testing

  (** Wrap this in a [let%test_module] to ensure the tests get run *)
  module Make (Hashtbl : Hashtbl_for_testing) : sig end
end
