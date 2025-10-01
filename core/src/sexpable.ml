open! Import
include Base.Sexpable

module Stable = struct
  module Of_sexpable = struct
    module%template.portable
      [@alloc a @ m = (heap_global, stack_local)] V1
        (Sexpable : S
      [@alloc a])
        (M : sig
           type t

           val to_sexpable : t -> Sexpable.t [@@alloc a @ m = (a @ m, heap_global)]
           val of_sexpable : Sexpable.t -> t
         end) : S [@alloc a] with type t := M.t = struct
      let t_of_sexp sexp =
        let s = Sexpable.t_of_sexp sexp in
        try M.of_sexpable s with
        | exn -> of_sexp_error_exn exn sexp
      ;;

      let[@alloc a = (a, heap)] sexp_of_t t =
        (Sexpable.sexp_of_t [@alloc a])
          ((M.to_sexpable [@alloc a]) t) [@exclave_if_stack a]
      ;;
    end
  end

  module Of_sexpable1 = struct
    module%template.portable
      [@alloc a @ m = (heap_global, stack_local)] [@kind ka = (value, any)] V1
        (Sexpable : S1
      [@kind ka] [@alloc a])
        (M : sig
           type 'a t

           val to_sexpable : 'a. 'a t -> 'a Sexpable.t
           [@@alloc a @ m = (a @ m, heap_global)]

           val of_sexpable : 'a. 'a Sexpable.t -> 'a t
         end) : S1 [@kind ka] [@alloc a] with type 'a t := 'a M.t = struct
      let t_of_sexp a_of_sexp sexp =
        let s = Sexpable.t_of_sexp a_of_sexp sexp in
        try M.of_sexpable s with
        | exn -> of_sexp_error_exn exn sexp
      ;;

      let[@alloc a = (a, heap)] sexp_of_t sexp_of_a t =
        (Sexpable.sexp_of_t [@alloc a])
          sexp_of_a
          ((M.to_sexpable [@alloc a]) t) [@exclave_if_stack a]
      ;;
    end
  end

  module Of_sexpable2 = struct
    module%template.portable
      [@alloc a @ m = (heap_global, stack_local)]
      [@kind ka = (value, any), kb = (value, any)] V1
        (Sexpable : S2
      [@kind ka kb] [@alloc a])
        (M : sig
           type ('a, 'b) t

           val to_sexpable : 'a 'b. ('a, 'b) t -> ('a, 'b) Sexpable.t
           [@@alloc a @ m = (a @ m, heap_global)]

           val of_sexpable : 'a 'b. ('a, 'b) Sexpable.t -> ('a, 'b) t
         end) : S2 [@kind ka kb] [@alloc a] with type ('a, 'b) t := ('a, 'b) M.t = struct
      let t_of_sexp a_of_sexp b_of_sexp sexp =
        let s = Sexpable.t_of_sexp a_of_sexp b_of_sexp sexp in
        try M.of_sexpable s with
        | exn -> of_sexp_error_exn exn sexp
      ;;

      let[@alloc a = (a, heap)] sexp_of_t sexp_of_a sexp_of_b t =
        (Sexpable.sexp_of_t [@alloc a])
          sexp_of_a
          sexp_of_b
          ((M.to_sexpable [@alloc a]) t) [@exclave_if_stack a]
      ;;
    end
  end

  module Of_sexpable3 = struct
    module%template.portable
      [@alloc a @ m = (heap_global, stack_local)]
      [@kind ka = (value, any), kb = (value, any), kc = (value, any)] V1
        (Sexpable : S3
      [@kind ka kb kc] [@alloc a])
        (M : sig
           type ('a, 'b, 'c) t

           val to_sexpable : 'a 'b 'c. ('a, 'b, 'c) t -> ('a, 'b, 'c) Sexpable.t
           [@@alloc a @ m = (a @ m, heap_global)]

           val of_sexpable : 'a 'b 'c. ('a, 'b, 'c) Sexpable.t -> ('a, 'b, 'c) t
         end) :
      S3 [@kind ka kb kc] [@alloc a] with type ('a, 'b, 'c) t := ('a, 'b, 'c) M.t = struct
      let t_of_sexp a_of_sexp b_of_sexp c_of_sexp sexp =
        let s = Sexpable.t_of_sexp a_of_sexp b_of_sexp c_of_sexp sexp in
        try M.of_sexpable s with
        | exn -> of_sexp_error_exn exn sexp
      ;;

      let[@alloc a = (a, heap)] sexp_of_t sexp_of_a sexp_of_b sexp_of_c t =
        (Sexpable.sexp_of_t [@alloc a])
          sexp_of_a
          sexp_of_b
          sexp_of_c
          ((M.to_sexpable [@alloc a]) t) [@exclave_if_stack a]
      ;;
    end
  end

  module Of_stringable = struct
    module%template.portable
      [@alloc a = (heap, stack)] V1
        (M : Stringable.S
      [@alloc a]) : sig
        type t [@@deriving sexp_grammar]

        include S [@alloc a] with type t := t
      end
      with type t := M.t = struct
      let t_of_sexp sexp =
        match sexp with
        | Sexplib.Sexp.Atom s ->
          (try M.of_string s with
           | exn -> of_sexp_error_exn exn sexp)
        | Sexplib.Sexp.List _ ->
          of_sexp_error
            "Sexpable.Of_stringable.t_of_sexp expected an atom, but got a list"
            sexp
      ;;

      let[@alloc a = (a, heap)] sexp_of_t t =
        Sexplib.Sexp.Atom ((M.to_string [@alloc a]) t) [@exclave_if_stack a]
      ;;

      let t_sexp_grammar : M.t Sexplib.Sexp_grammar.t =
        Sexplib.Sexp_grammar.coerce string_sexp_grammar
      ;;
    end
  end

  module To_stringable = struct
    module%template.portable V1 (M : S) : Stringable.S with type t := M.t = struct
      let of_string x = Sexplib.Conv.of_string__of__of_sexp M.t_of_sexp x
      let to_string x = Sexplib.Conv.string_of__of__sexp_of M.sexp_of_t x
    end
  end

  module To_stringable_utf8 = struct
    module%template.portable V1 (M : S) : Stringable.S with type t := M.t = struct
      let of_string x = Sexplib.Conv.of_string__of__of_sexp M.t_of_sexp x

      let to_string x =
        M.sexp_of_t x |> Base.Sexp.Utf8.to_string |> Base.String.Utf8.to_string
      ;;
    end
  end
end

module%template.portable [@modality p] To_stringable =
  Stable.To_stringable.V1
  [@modality p]

module%template.portable [@modality p] To_stringable_utf8 =
  Stable.To_stringable_utf8.V1
  [@modality p]
