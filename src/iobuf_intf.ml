(** See {{!Core.Iobuf}[Iobuf]} for documentation. *)

open! Import

(** [no_seek] and [seek] are phantom types used in a similar manner to [read] and
    [read_write]. *)

type no_seek                [@@deriving sexp_of]  (** Like [read]. *)

type seek = private no_seek [@@deriving sexp_of]  (** Like [read_write]. *)

(** A collection of iobuf access functions.  This abstracts over [Iobuf.Consume],
    [Iobuf.Fill], [Iobuf.Peek], and [Iobuf.Poke].

    Make all labeled arguments mandatory in [string] and [bigstring] to avoid accidental
    allocation in, e.g., [Iobuf.Poke.string].  For convenience, [stringo] and [bigstringo]
    are available by analogy between [blit] and [blito]. *)
module type Accessors = sig
  (** [('d, 'w) Iobuf.t] accessor function manipulating ['a], either writing it to the
      iobuf or reading it from the iobuf. *)
  type ('a, 'd, 'w) t constraint 'd = [> read ]
  type 'a bin_prot

  val char                      :                             (char,        'd, 'w) t
  val  int8                     :                             (int,         'd, 'w) t
  val  int16_be                 :                             (int,         'd, 'w) t
  val  int16_le                 :                             (int,         'd, 'w) t
  val  int32_be                 :                             (int,         'd, 'w) t
  val  int32_le                 :                             (int,         'd, 'w) t
  val uint8                     :                             (int,         'd, 'w) t
  val uint16_be                 :                             (int,         'd, 'w) t
  val uint16_le                 :                             (int,         'd, 'w) t
  val uint32_be                 :                             (int,         'd, 'w) t
  val uint32_le                 :                             (int,         'd, 'w) t
  val  int64_be                 :                             (int,         'd, 'w) t
  val  int64_le                 :                             (int,         'd, 'w) t
  val uint64_be                 :                             (int,         'd, 'w) t
  val uint64_le                 :                             (int,         'd, 'w) t
  val  int64_t_be               :                             (Int64.t,     'd, 'w) t
  val  int64_t_le               :                             (Int64.t,     'd, 'w) t
  val  int64_be_trunc           :                             (int,         'd, 'w) t
  val  int64_le_trunc           :                             (int,         'd, 'w) t
  val head_padded_fixed_string  : padding:char ->  len:int ->    (string,   'd, 'w) t
  val tail_padded_fixed_string  : padding:char ->  len:int ->    (string,   'd, 'w) t
  val                   string  :  str_pos:int ->  len:int ->    (string,   'd, 'w) t
  val                   bytes   :  str_pos:int ->  len:int ->   (Bytes.t,   'd, 'w) t
  val                bigstring  :  str_pos:int ->  len:int -> (Bigstring.t, 'd, 'w) t
  val                   stringo : ?str_pos:int -> ?len:int ->    (string,   'd, 'w) t
  val                   byteso  : ?str_pos:int -> ?len:int ->   (Bytes.t,   'd, 'w) t
  val                bigstringo : ?str_pos:int -> ?len:int -> (Bigstring.t, 'd, 'w) t
  val bin_prot                  : 'a bin_prot              -> ('a,          'd, 'w) t
end

(** An iobuf window bound, either upper or lower.  You can't see its int value, but you
    can save and restore it. *)
module type Bound = sig
  type ('d, 'w) iobuf

  type t =
    private int (*_ performance hack: avoid the write barrier *)
  [@@deriving compare, sexp_of]

  val window : (_, _) iobuf -> t
  val limit  : (_, _) iobuf -> t

  val restore : t -> (_, seek) iobuf -> unit

end

(** The [src_pos] argument of {!Core_kernel.Blit.blit} doesn't make sense here. *)

type ('src, 'dst) consuming_blit
  =  src     : 'src
  -> dst     : 'dst
  -> dst_pos : int
  -> len     : int
  -> unit
type ('src, 'dst) consuming_blito
  =  src      : 'src
  -> ?src_len : int                     (** Default is [Iobuf.length src]. *)
  -> dst      : 'dst
  -> ?dst_pos : int                     (** Default is [0]. *)
  -> unit
  -> unit

module type Consuming_blit = sig
  type src
  type dst

  val blito       : (src, dst) consuming_blito
  val blit        : (src, dst) consuming_blit
  val unsafe_blit : (src, dst) consuming_blit

  (** [subo] defaults to using [Iobuf.length src] *)
  val subo : ?len:int -> src -> dst
  val sub  : src -> len:int -> dst
end

module type Compound_hexdump = sig
  type ('rw, 'seek) t
  module Hexdump : sig
    type nonrec ('rw, 'seek) t = ('rw, 'seek) t [@@deriving sexp_of]
    val to_string_hum : ?max_lines:int -> (_, _) t -> string
    val to_sequence   : ?max_lines:int -> (_, _) t -> string Sequence.t
  end
end

(*_ For use in iobuf.mli -- can't be added to Std_internal due to dependencies.  This
  reduces noise in iobuf.mli by allowing the use of [Unix] rather than [Core_unix]
  throughout. *)
module Unix = Core_unix
