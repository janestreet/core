(** Module for easily generating unit tests.  Based on code posted by
    padiolea\@irisa.fr to the caml mailing list. *)


(** the type of a random ['a]-generator *)
type 'a gen = unit -> 'a

(** float generator (no nan, inf, etc.) *)
val fg : float gen

(** character generator *)
val cg : char gen

(** natural number generator *)
val nng : int gen

(** unsigned int generator (uniform random in range min_int, max_int) *)
val uig : int gen

(** pair generator *)
val pg : 'a gen -> 'b gen -> ('a * 'b) gen

(** triple generator *)
val tg : 'a gen -> 'b gen -> 'c gen -> ('a * 'b * 'c) gen

(** list generator *)
val lg : 'a gen -> ?size_gen:int gen -> 'a list gen

(** string generator *)
val sg : ?char_gen : char gen -> ?size_gen : int gen -> string gen

(** generator that always returns given value *)
val always : 'a -> 'a gen

(** [laws iter gen func] applies [func] repeatedly ([iter] times) on output
    of [gen], and if [func] ever returns false, then the input that caused
    the failure is returned optionally. *)
val laws : int -> 'a gen -> ('a -> bool) -> 'a option

(** Like laws, but throws an exception instead of returning an option. *)
val laws_exn : string -> int -> 'a gen -> ('a -> bool) -> unit

(* [repeat n f gen] runs [f] for [n] iterations, each using a different value from
   [gen] *)
val repeat : int -> ('a -> unit) -> 'a gen -> unit

