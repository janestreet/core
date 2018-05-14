open Core_kernel

external digest_fd : Core_unix.File_descr.t -> string = "core_md5_fd"

include Md5

let digest_fd_blocking fd = of_binary_exn (digest_fd fd)

let digest_file_blocking path =
  Exn.protectx
    (Core_unix.openfile path ~mode:[O_RDONLY])
    ~f:digest_fd_blocking
    ~finally:Core_unix.close

external c_digest_subbigstring : Bigstring.t -> pos:int -> len:int -> res:Bytes.t -> unit =
  "core_md5_digest_subbigstring"

let unsafe_digest_subbigstring buf ~pos ~len =
  (* It's more efficient to allocate the result on the OCaml side and declare the C
     function as noalloc than to let the C function allocate. *)
  let res = Bytes.create 16 in
  c_digest_subbigstring buf ~pos ~len ~res;
  Md5_lib.unsafe_of_binary
    (Bytes.unsafe_to_string ~no_mutation_while_string_reachable:res)

let digest_subbigstring buf ~pos ~len =
  Ordered_collection_common.check_pos_len_exn ~pos ~len ~length:(Bigstring.length buf);
  unsafe_digest_subbigstring buf ~pos ~len

let digest_bigstring buf =
  unsafe_digest_subbigstring buf ~pos:0 ~len:(Bigstring.length buf)
