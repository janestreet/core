open Sexplib

let failwithf = Core_printf.failwithf

module type S = sig
  type t
  val sexp_of_t : t -> Sexp.t
  val t_of_sexp : Sexp.t -> t
end

module type S1 = sig
  type 'a t
  val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
  val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t
end

module type S2 = sig
  type ('a, 'b) t
  val sexp_of_t :
    ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) t -> Sexp.t
  val t_of_sexp :
    (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t
end

module type S3 = sig
  type ('a, 'b, 'c) t
  val sexp_of_t :
    ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('c -> Sexp.t)
    -> ('a, 'b, 'c) t -> Sexp.t
  val t_of_sexp :
    (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> (Sexp.t -> 'c)
    -> Sexp.t -> ('a, 'b, 'c) t
end

module Of_stringable (M : Stringable.S)
  : S with type t := M.t = struct
  type t = M.t
  let t_of_sexp sexp =
    match sexp with
    | Sexp.Atom s -> M.of_string s
    | Sexp.List _ -> Conv.of_sexp_error "t_of_sexp" sexp
  let sexp_of_t t = Sexp.Atom (M.to_string t)
end

module To_stringable (M : S) : Stringable.S with type t := M.t =
struct
  type t = M.t
  let of_string x = Conv.of_string__of__of_sexp M.t_of_sexp x
  let to_string x = Conv.string_of__of__sexp_of M.sexp_of_t x
end
