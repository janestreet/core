open! Core

module _ : sig
  open Set_intf

  module Tree : sig
    type ('a, 'b) t

    include
      Creators_and_accessors_generic
        with type ('a, 'b) set := ('a, 'b) t
        with type ('a, 'b) t := ('a, 'b) t
        with type ('a, 'b) tree := ('a, 'b) t
        with type 'a elt := 'a
        with type 'c cmp := 'c
        with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) With_comparator.t
        with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) With_comparator.t
  end

  type ('a, 'b) t

  include
    Creators_and_accessors_generic
      with type ('a, 'b) set := ('a, 'b) t
      with type ('a, 'b) t := ('a, 'b) t
      with type ('a, 'b) tree := ('a, 'b) Tree.t
      with type 'a elt := 'a
      with type 'a cmp := 'a
      with type ('a, 'cmp, 'z) create_options :=
        ('a, 'cmp, 'z) Set_intf.With_first_class_module.t
      with type ('a, 'cmp, 'z) access_options :=
        ('a, 'cmp, 'z) Set_intf.Without_comparator.t
end =
  Set
