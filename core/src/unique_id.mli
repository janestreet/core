(** Functors for creating modules that mint unique identifiers. *)

open! Import
open Unique_id_intf

module type Id = Id

(** An abstract unique identifier based on ordinary OCaml integers.  Be careful, this may
    easily overflow on 32-bit platforms!  [Int63] is a safer choice for portability.

    [Int] is useful when one is passing unique ids to C and needs a guarantee as to their
    representation.  [Int] is always represented as an integer, while [Int63] is either an
    integer (on 64-bit machines) or a pointer (on 32-bit machines).

    The generated ids will therefore be fast to generate and not use much memory.  If you
    do not have very stringent requirements on the size, speed, and ordering of your IDs
    then you should use the UUIDM library instead, which will give you a truly unique id,
    even amongst different runs and different machines. *)
module Int () : Id with type t = private int

(** An abstract unique identifier based on 63 bit integers. *)
module Int63 () : Id with type t = private Int63.t
