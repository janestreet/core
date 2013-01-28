module type S = sig
  type t

  val of_int_exn : int -> t
  val to_int_exn : t -> int
end
