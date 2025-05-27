open! Import
include Portable.Atomic

include struct
  open Bin_prot

  let bin_shape_t bin_shape_el = bin_shape_el

  [%%template
  [@@@mode.default m = (local, global)]

  let bin_size_t bin_size_el t = bin_size_el (get t |> Modes.Contended.cross)

  let bin_write_t bin_write_el buf ~pos t =
    bin_write_el buf ~pos (get t |> Modes.Contended.cross)
  ;;]

  let bin_read_t bin_read_el buf ~pos_ref =
    make (bin_read_el buf ~pos_ref |> Modes.Portable.cross)
  ;;

  let __bin_read_t__ bin_read_el buf ~pos_ref _n = make (bin_read_el buf ~pos_ref)

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
          Common.raise_variant_wrong_type "Core.Atomic.bin_reader_t" !pos_ref)
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

let stable_witness _ = Stable_witness.assert_stable
