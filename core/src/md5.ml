module T = struct
  include Bin_prot.Md5

  let equal = [%compare.equal: t]
  let sexp_of_t t = t |> to_hex |> String.sexp_of_t
  let t_of_sexp s = s |> String.t_of_sexp |> of_hex_exn
  let t_sexp_grammar = Sexplib.Sexp_grammar.coerce String.t_sexp_grammar
end

let hash_fold_t accum t = String.hash_fold_t accum (T.to_binary t)
let hash t = String.hash (T.to_binary t)

module As_binary_string = struct
  module Stable = struct
    module V1 = struct
      type t = T.t [@@deriving compare, equal]

      let hash_fold_t = hash_fold_t
      let hash = hash
      let sexp_of_t x = String.sexp_of_t (T.to_binary x)
      let t_of_sexp x = T.of_binary_exn (String.t_of_sexp x)
      let t_sexp_grammar = Sexplib.Sexp_grammar.coerce String.t_sexp_grammar
      let to_binable = T.to_binary
      let of_binable = T.of_binary_exn

      include Bin_prot.Utils.Make_binable_without_uuid [@alert "-legacy"] (struct
        module Binable = String.Stable.V1

        type t = Bin_prot.Md5.t

        let to_binable = to_binable
        let of_binable = of_binable
      end)

      let stable_witness : t Stable_witness.t =
        Stable_witness.of_serializable
          String.Stable.V1.stable_witness
          of_binable
          to_binable
      ;;
    end
  end

  include Stable.V1
  include Comparable.Make (Stable.V1)
  include Hashable.Make (Stable.V1)
end

module Stable = struct
  module V1 = struct
    type t = T.t [@@deriving compare, equal, sexp, sexp_grammar]

    let hash_fold_t = hash_fold_t
    let hash = hash
    let to_binable = Fn.id
    let of_binable = Fn.id

    include Bin_prot.Utils.Make_binable_without_uuid [@alert "-legacy"] (struct
      module Binable = Bin_prot.Md5.Stable.V1

      type t = Bin_prot.Md5.t

      let to_binable = to_binable
      let of_binable = of_binable
    end)

    let stable_witness : t Stable_witness.t =
      Stable_witness.of_serializable
        Bin_prot.Md5.Stable.V1.stable_witness
        of_binable
        to_binable
    ;;
  end

  let digest_string s = Md5_lib.string s
end

include Stable.V1
include Comparable.Make (Stable.V1)
include Hashable.Make (Stable.V1)

let digest_num_bytes = 16
let to_hex = T.to_hex
let from_hex = T.of_hex_exn
let of_hex_exn = T.of_hex_exn
let of_binary_exn = T.of_binary_exn
let to_binary = T.to_binary
let digest_string = Stable.digest_string
let digest_bytes = Md5_lib.bytes

external caml_sys_open
  :  string
  -> Stdlib.open_flag list
  -> perm:int
  -> int
  = "caml_sys_open"

external caml_sys_close : int -> unit = "caml_sys_close"
external digest_fd_blocking : int -> string = "core_md5_fd"

let digest_file_blocking path =
  of_binary_exn
    (Base.Exn.protectx
       (caml_sys_open path [ Open_rdonly; Open_binary ] ~perm:0o000)
       ~f:digest_fd_blocking
       ~finally:caml_sys_close)
;;

let file = digest_file_blocking

let digest_channel_blocking_without_releasing_runtime_lock channel ~len =
  of_binary_exn (Stdlib.Digest.channel channel len)
;;

let channel channel len =
  digest_channel_blocking_without_releasing_runtime_lock channel ~len
;;

let output_blocking t oc = Stdlib.Digest.output oc (to_binary t)
let output oc t = output_blocking t oc
let input_blocking ic = of_binary_exn (Stdlib.Digest.input ic)
let input = input_blocking
let digest_subbytes = Md5_lib.subbytes
let string = digest_string
let bytes = digest_bytes
let subbytes s pos len = digest_subbytes s ~pos ~len

let digest_bin_prot writer value =
  digest_string (Core_bin_prot.Writer.to_string writer value)
;;

external c_digest_subbigstring
  :  Bigstring.t
  -> pos:int
  -> len:int
  -> res:Bytes.t
  -> unit
  = "core_md5_digest_subbigstring"

let unsafe_digest_subbigstring buf ~pos ~len =
  (* It's more efficient to allocate the result on the OCaml side and declare the C
     function as noalloc than to let the C function allocate. *)
  let res = Bytes.create 16 in
  c_digest_subbigstring buf ~pos ~len ~res;
  Md5_lib.unsafe_of_binary
    (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:res)
;;

let digest_subbigstring buf ~pos ~len =
  Ordered_collection_common.check_pos_len_exn
    ~pos
    ~len
    ~total_length:(Bigstring.length buf);
  unsafe_digest_subbigstring buf ~pos ~len
;;

let digest_bigstring buf =
  unsafe_digest_subbigstring buf ~pos:0 ~len:(Bigstring.length buf)
;;
