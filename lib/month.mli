type t
include Binable.S with type t := t
include Comparable.S with type t := t
include Hashable.S with type t := t
include Sexpable.S with type t := t

(**
 * [of_string s] accepts three-character abbreviations with any capitalization
 *)
include Stringable.S with type t := t

val invariant : t -> unit

val jan : t
val feb : t
val mar : t
val apr : t
val may : t
val jun : t
val jul : t
val aug : t
val sep : t
val oct : t
val nov : t
val dec : t

val all : t list

type variant = [ `Jan | `Feb | `Mar | `Apr | `May | `Jun
               | `Jul | `Aug | `Sep | `Oct | `Nov | `Dec ]

val get : t -> variant

val create : variant -> t

(** [of_int i] returns i'th month if [i] is in 1,2,...,12.  Otherwise it returns
    None. *)
val of_int : int -> t option

(** [of_int_exn i] should have i in 1,2,...,12 and returns the i'th month. *)
val of_int_exn : int -> t

(** [to_int t] returns an int in 1,2,...12. *)
val to_int : t -> int

(** [shift t i] goes forward (or backward) the specified number of months *)
val shift : t -> int -> t
