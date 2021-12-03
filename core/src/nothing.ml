open! Import

module Stable = struct
  module V1 = struct
    type t = Base.Nothing.t = |

    module Shape = struct
      type t [@@deriving bin_shape]
    end

    let unreachable_code = Base.Nothing.unreachable_code
    let bin_shape_t = Shape.bin_shape_t
    let tp_loc = [%here].pos_fname ^ ".Stable.V1.t"
    let all = []
    let hash_fold_t _ t = unreachable_code t
    let hash = unreachable_code
    let compare a _ = unreachable_code a
    let bin_size_t = unreachable_code
    let bin_write_t _buf ~pos:_ t = unreachable_code t
    let bin_writer_t = { Bin_prot.Type_class.size = bin_size_t; write = bin_write_t }

    let __bin_read_t__ _buf ~pos_ref _ =
      Bin_prot.Common.raise_variant_wrong_type tp_loc !pos_ref
    ;;

    let bin_read_t _buf ~pos_ref =
      Bin_prot.Common.raise_read_error (Empty_type tp_loc) !pos_ref
    ;;

    let bin_reader_t =
      { Bin_prot.Type_class.read = bin_read_t; vtag_read = __bin_read_t__ }
    ;;

    let bin_t =
      { Bin_prot.Type_class.writer = bin_writer_t
      ; reader = bin_reader_t
      ; shape = bin_shape_t
      }
    ;;

    let sexp_of_t = unreachable_code
    let t_of_sexp sexp = Sexplib.Conv_error.empty_type tp_loc sexp
  end
end

include Stable.V1
include Base.Nothing
include Identifiable.Extend (Base.Nothing) (Stable.V1)
