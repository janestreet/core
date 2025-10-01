@@ portable

open! Import
include module type of Bin_prot

module Writer : sig
  type ('a : any) t = 'a Bin_prot.Type_class.writer =
    { size : 'a Size.sizer
    ; write : 'a Write.writer
    }

  val to_string : 'a t -> 'a -> string
  val to_bytes : 'a t -> 'a -> bytes
  val to_bigstring : 'a t -> 'a -> Bigstring.t
end

module Reader : sig
  type ('a : any) t = 'a Bin_prot.Type_class.reader =
    { read : 'a Read.reader
    ; vtag_read : 'a Read.vtag_reader
    }

  val of_string : 'a t -> string @ local -> 'a
  val of_bytes : 'a t -> bytes @ local -> 'a
  val of_bigstring : 'a t -> Bigstring.t -> 'a
end
