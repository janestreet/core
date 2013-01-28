open StdLabels
open MoreLabels
open Sexplib.Conv
open Bin_prot.Std

let seek_out = `Deprecated_use_out_channel
let pos_out = `Deprecated_use_out_channel
let out_channel_length = `Deprecated_use_out_channel
let seek_in = `Deprecated_use_in_channel
let pos_in = `Deprecated_use_in_channel
let in_channel_length = `Deprecated_use_in_channel
let modf = `Deprecated_use_float_modf
let truncate = `Deprecated_use_float_iround_towards_zero

let read_wrap ?binary:_ ~f:_ _ = `Deprecated_use_In_channel_with_file
let write_wrap ?binary:_ ~f:_ _ = `Deprecated_use_Out_channel_with_file

let write_lines _ _ = `Deprecated_use_Out_channel_write_lines
let read_lines _ = `Deprecated_use_In_channel_read_lines
let input_lines ?fix_win_eol:_ _ = `Deprecated_use_In_channel_input_lines

let close_in = In_channel.close
let close_out = Out_channel.close

type read_only with bin_io
type immutable  = private read_only with bin_io
type read_write = private read_only with bin_io
type write_only with bin_io

(* These are to work around a bug in pa_sexp where sexp_of_immutable would assert false
   rather than give a clear error message. *)
let sexp_of_immutable _ = failwith "attempt to convert abstract type immutable"
let immutable_of_sexp = sexp_of_immutable
let sexp_of_read_only _ = failwith "attempt to convert abstract type read_only"
let read_only_of_sexp = sexp_of_read_only
let sexp_of_read_write _ = failwith "attempt to convert abstract type read_write"
let read_write_of_sexp = sexp_of_read_write
let sexp_of_write_only _ = failwith "attempt to convert abstract type write_only"
let write_only_of_sexp = sexp_of_write_only

type never_returns
let never_returns (_ : never_returns) = assert false

exception Finally = Exn.Finally

let protectx = Exn.protectx
let protect = Exn.protect

let (|!) = Fn.(|!)
let ident = Fn.id
let const = Fn.const
let (==>) a b = (not a) || b

let uw = function Some x -> x | None -> raise Not_found

let is_none = Option.is_none
let is_some = Option.is_some

let fst3 (x,_,_) = x
let snd3 (_,y,_) = y
let trd3 (_,_,z) = z

external ascending : 'a -> 'a -> int = "%compare"
let descending x y = compare y x

open Sexplib

let failwiths = Error.failwiths

include struct
  open Core_printf
  let failwithf = failwithf
  let invalid_argf = invalid_argf
  let ksprintf = ksprintf
end

let sexp_underscore = Sexp.Atom "_"
let sexp_of_a  _ = sexp_underscore
let sexp_of_a1 _ = sexp_underscore
let sexp_of_a2 _ = sexp_underscore
let sexp_of_a3 _ = sexp_underscore
let sexp_of_a4 _ = sexp_underscore
let sexp_of_a5 _ = sexp_underscore
let sexp_of_b  _ = sexp_underscore
let sexp_of_c  _ = sexp_underscore
let sexp_of_d  _ = sexp_underscore
let sexp_of_e  _ = sexp_underscore

(* module With_return only exists to avoid circular dependencies *)
include With_return

let equal = Caml.(=)

let phys_equal = Caml.(==)
let (==) _ _ = `Consider_using_phys_equal
let (!=) _ _ = `Consider_using_phys_equal

let force = Lazy.force

let ( ^/ ) = Core_filename.concat

exception Decimal_nan_or_inf with sexp
module Decimal = struct
  module T = struct
    module Binable = Float
    type t = float
    let verify t =
      match Pervasives.classify_float t with
      | FP_normal
      | FP_subnormal
      | FP_zero      -> ()
      | FP_infinite
      | FP_nan       -> raise Decimal_nan_or_inf
    let of_binable t = verify t; t
    let to_binable t = verify t; t
  end
  include T
  include Bin_prot.Utils.Make_binable (T)

  let sexp_of_t t = Sexp.Atom (Core_printf.sprintf "%.12G" t)
  let t_of_sexp = function
  | Sexp.Atom s ->
    let t = Float.of_string s in
    begin
      try
        verify t
      with e -> Conv.of_sexp_error (Exn.to_string e) (Sexp.Atom s)
    end;
    t
  | s ->
    Conv.of_sexp_error "decimal_of_sexp: Expected Atom, found List" s
end

let stage = Staged.stage
let unstage = Staged.unstage

type decimal = Decimal.t with sexp, bin_io

type passfail = Pass | Fail of string

exception Unimplemented of string with sexp
let unimplemented = Or_error.unimplemented

exception Bug of string with sexp
