open! Import
include Base.Portable_lazy

module Stable = struct
  module V1 = struct
    type nonrec 'a t = 'a t

    let compare = compare
    let compare__local = compare__local
    let equal = equal
    let equal__local = equal__local
    let sexp_of_t = sexp_of_t
    let t_of_sexp = t_of_sexp
    let t_sexp_grammar = t_sexp_grammar
    let hash_fold_t = hash_fold_t

    include struct
      open Bin_prot

      open struct
        let from_val (a : (_ : value mod portable)) = from_val a
        let force (a : ('a : value mod contended) t) : 'a = force a
      end

      let bin_shape_t bin_shape_el = bin_shape_el
      let bin_size_t bin_size_el t = bin_size_el (force t)
      let bin_write_t bin_write_el buf ~pos t = bin_write_el buf ~pos (force t)
      let bin_read_t bin_read_el buf ~pos_ref = from_val (bin_read_el buf ~pos_ref)
      let __bin_read_t__ bin_read_el buf ~pos_ref _n = from_val (bin_read_el buf ~pos_ref)

      let bin_writer_t : _ Type_class.S1.writer =
        fun bin_writer ->
        { size = (fun v -> bin_size_t bin_writer.size v)
        ; write = (fun buf ~pos v -> bin_write_t bin_writer.write buf ~pos v)
        }
      ;;

      let bin_reader_t : _ Type_class.S1.reader =
        fun bin_reader ->
        { read = (fun buf ~pos_ref -> bin_read_t bin_reader.read buf ~pos_ref)
        ; vtag_read =
            (fun _buf ~pos_ref _n ->
              Common.raise_variant_wrong_type "Core.Atomic_lazy.bin_reader_t" !pos_ref)
        }
      ;;

      let bin_t : _ Type_class.S1.t =
        fun type_class ->
        { shape = bin_shape_t type_class.shape
        ; writer = bin_writer_t type_class.writer
        ; reader = bin_reader_t type_class.reader
        }
      ;;
    end
  end
end

include Stable.V1

include struct
  open Quickcheck

  open struct
    let from_val (a : (_ : value mod portable)) = from_val a
    let force (a : ('a : value mod contended) t) : 'a = force a
  end

  let quickcheck_generator gen_a = Generator.map gen_a ~f:from_val
  let quickcheck_shrinker shrink_a = Shrinker.map shrink_a ~f:from_val ~f_inverse:force
  let quickcheck_observer obs_a = Observer.unmap obs_a ~f:force
end
