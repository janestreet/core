module type S = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
end

(* If you're looking for the [Stringable.Of_sexpable] functor, it's called
   [Sexpable.To_stringable], in order to avoid a circular dependency. *)
