(** Extensible string buffers based on Bigstrings.

    This module implements string buffers that automatically expand as necessary.  It
    provides accumulative concatenation of strings in quasi-linear time (instead of
    quadratic time when strings are concatenated pairwise).

    This implementation uses Bigstrings instead of strings. This removes the 16MB limit
    on buffer size (on 32-bit machines), and improves I/O-performance when reading/writing
    from/to channels.
*)

open! Import
include Base.Buffer.S

(** Return a copy of the current contents of the buffer as a bigstring.
    The buffer itself is unchanged. *)
val big_contents : t -> Bigstring.t

(** Return the actual underlying bigstring used by this bigbuffer.
    No copying is involved.  To be safe, use and finish with the returned value
    before calling any other function in this module on the same [Bigbuffer.t]. *)
val volatile_contents : t -> Bigstring.t

(** [add_bigstring b s] appends the bigstring [s] at the end of the buffer [b]. *)
val add_bigstring : t -> Bigstring.t -> unit

(** [add_bin_prot b writer x] appends the bin-protted representation of [x] at the end of
    the buffer [b]. *)
val add_bin_prot : t -> 'a Bin_prot.Type_class.writer -> 'a -> unit

(** [add_substitute b f s] appends the string pattern [s] at the end
    of the buffer [b] with substitution.
    The substitution process looks for variables into
    the pattern and substitutes each variable name by its value, as
    obtained by applying the mapping [f] to the variable name. Inside the
    string pattern, a variable name immediately follows a non-escaped
    [$] character and is one of the following:
    - a non empty sequence of alphanumeric or [_] characters,
    - an arbitrary sequence of characters enclosed by a pair of
      matching parentheses or curly brackets.

    An escaped [$] character is a [$] that immediately follows a backslash
    character; it then stands for a plain [$].
    Raise [Caml.Not_found] or [Not_found_s] if the closing character of a
    parenthesized variable cannot be found. *)
val add_substitute : t -> (string -> string) -> string -> unit


(** *)

module Format : sig
  open Format

  val formatter_of_buffer : t -> formatter
  val bprintf : t -> ('a, formatter, unit) format -> 'a
end

module Printf : sig
  val bprintf : t -> ('a, unit, string, unit) format4 -> 'a
end

(**/**)

(** For Core.Bigbuffer, not for users! *)
val __internal : t -> Bigbuffer_internal.t
