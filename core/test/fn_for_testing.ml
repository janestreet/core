open Core

module type S = sig
  type t [@@deriving sexp_of, compare]
end

module type Examples = sig
  type t

  val examples : t list
end

module Make (Input : S) (Output : S) (Examples : Examples with type t := Input.t) = struct
  open Examples

  type t = Input.t -> Output.t

  let to_alist f = List.map examples ~f:(fun x -> x, f x)
  let compare t1 t2 = [%compare: (Input.t * Output.t) list] (to_alist t1) (to_alist t2)
  let sexp_of_t t = [%sexp (to_alist t : (Input.t * Output.t) list)]
end
