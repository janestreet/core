open! Core

module _ : sig
  open Map_intf

  module Tree : sig
    type ('a, 'b, 'c) t

    include
      Creators_and_accessors_generic
        with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
        with type ('a, 'b, 'c) tree := ('a, 'b, 'c) t
        with type 'c cmp := 'c
        with type 'k key := 'k
        with type ('a, 'b, 'c) create_options := ('a, 'b, 'c) With_comparator.t
        with type ('a, 'b, 'c) access_options := ('a, 'b, 'c) With_comparator.t
  end

  type ('a, 'b, 'c) t

  include
    Creators_and_accessors_generic
      with type ('a, 'b, 'c) t := ('a, 'b, 'c) t
      with type ('a, 'b, 'c) tree := ('a, 'b, 'c) Tree.t
      with type ('a, 'cmp, 'z) access_options := ('a, 'cmp, 'z) Without_comparator.t
      with type ('a, 'cmp, 'z) create_options :=
        ('a, 'cmp, 'z) Map_intf.With_first_class_module.t
      with type 'k key := 'k
      with type 'c cmp := 'c
end =
  Map
