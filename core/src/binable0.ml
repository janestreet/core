open! Import
include Binable_intf
include Bin_prot.Binable
module Shape = Bin_prot.Shape

module Stable = struct
  module Of_binable = struct
    module V1 (Binable : Minimal.S) (M : Conv_without_uuid with type binable := Binable.t) :
      S with type t := M.t = Bin_prot.Utils.Make_binable_without_uuid (struct
      module Binable = Binable
      include M
    end)
    [@@alert "-legacy"]

    module V2 (Binable : Minimal.S) (M : Conv with type binable := Binable.t) :
      S with type t := M.t = Bin_prot.Utils.Make_binable_with_uuid (struct
      module Binable = Binable
      include M
    end)
  end

  module Of_binable1 = struct
    module V1
      (Binable : Minimal.S1)
      (M : Conv1_without_uuid with type 'a binable := 'a Binable.t) :
      S1 with type 'a t := 'a M.t = Bin_prot.Utils.Make_binable1_without_uuid (struct
      module Binable = Binable
      include M
    end)
    [@@alert "-legacy"]

    module V2 (Binable : Minimal.S1) (M : Conv1 with type 'a binable := 'a Binable.t) :
      S1 with type 'a t := 'a M.t = Bin_prot.Utils.Make_binable1_with_uuid (struct
      module Binable = Binable
      include M
    end)
  end

  module Of_binable2 = struct
    module V1
      (Binable : Minimal.S2)
      (M : Conv2_without_uuid with type ('a, 'b) binable := ('a, 'b) Binable.t) :
      S2 with type ('a, 'b) t := ('a, 'b) M.t =
    Bin_prot.Utils.Make_binable2_without_uuid (struct
      module Binable = Binable
      include M
    end)
    [@@alert "-legacy"]

    module V2
      (Binable : Minimal.S2)
      (M : Conv2 with type ('a, 'b) binable := ('a, 'b) Binable.t) :
      S2 with type ('a, 'b) t := ('a, 'b) M.t =
    Bin_prot.Utils.Make_binable2_with_uuid (struct
      module Binable = Binable
      include M
    end)
  end

  module Of_binable3 = struct
    module V1
      (Binable : Minimal.S3)
      (M : Conv3_without_uuid with type ('a, 'b, 'c) binable := ('a, 'b, 'c) Binable.t) :
      S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) M.t =
    Bin_prot.Utils.Make_binable3_without_uuid (struct
      module Binable = Binable
      include M
    end)
    [@@alert "-legacy"]

    module V2
      (Binable : Minimal.S3)
      (M : Conv3 with type ('a, 'b, 'c) binable := ('a, 'b, 'c) Binable.t) :
      S3 with type ('a, 'b, 'c) t := ('a, 'b, 'c) M.t =
    Bin_prot.Utils.Make_binable3_with_uuid (struct
      module Binable = Binable
      include M
    end)
  end

  module Of_sexpable = struct
    module V1 (M : Sexpable.S) =
      Of_binable.V1
        (struct
          type t = Base.Sexp.t =
            | Atom of string
            | List of t list
          [@@deriving bin_io]
        end)
        (struct
          type t = M.t

          let to_binable = M.sexp_of_t
          let of_binable = M.t_of_sexp
        end)

    module V2 (M : Conv_sexpable) =
      Of_binable.V2
        (struct
          type t = Base.Sexp.t =
            | Atom of string
            | List of t list
          [@@deriving bin_io]
        end)
        (struct
          type t = M.t

          let to_binable = M.sexp_of_t
          let of_binable = M.t_of_sexp
          let caller_identity = M.caller_identity
        end)
  end

  module Of_stringable = struct
    module V1 (M : Stringable.S) = Bin_prot.Utils.Make_binable_without_uuid (struct
      module Binable = struct
        type t = string [@@deriving bin_io]
      end

      type t = M.t

      let to_binable = M.to_string

      (* Wrap exception for improved diagnostics. *)
      exception Of_binable of string * exn [@@deriving sexp]

      let of_binable s =
        try M.of_string s with
        | x -> raise (Of_binable (s, x))
      ;;
    end)
    [@@alert "-legacy"]

    module V2 (M : Conv_stringable) = Bin_prot.Utils.Make_binable_with_uuid (struct
      module Binable = struct
        type t = string [@@deriving bin_io]
      end

      type t = M.t

      let to_binable = M.to_string

      (* Wrap exception for improved diagnostics. *)
      exception Of_binable of string * exn [@@deriving sexp]

      let of_binable s =
        try M.of_string s with
        | x -> raise (Of_binable (s, x))
      ;;

      let caller_identity = M.caller_identity
    end)
  end
end

open Bigarray

type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t
type 'a m = (module S with type t = 'a)

let of_bigstring (type a) m bigstring =
  let module M = (val m : S with type t = a) in
  let pos_ref = ref 0 in
  let t = M.bin_read_t bigstring ~pos_ref in
  let bigstring_length = Array1.dim bigstring in
  (match !pos_ref = bigstring_length with
   | true -> ()
   | false ->
     raise_s
       [%message
         "bin_read_t did not consume the entire buffer"
           ~consumed:(!pos_ref : int)
           (bigstring_length : int)]);
  t
;;

(* Using the [Bigstring] module would introduce a cyclic dependency. *)
let create_bigstring size = Array1.create Bigarray.char Bigarray.c_layout size

let to_bigstring ?(prefix_with_length = false) (type a) m t =
  let module M = (val m : S with type t = a) in
  let t_length = M.bin_size_t t in
  let bigstring_length =
    if prefix_with_length then t_length + 8 (* the size of a 64-bit int *) else t_length
  in
  let bigstring = create_bigstring bigstring_length in
  let pos =
    if prefix_with_length
    then Bin_prot.Write.bin_write_int_64bit bigstring ~pos:0 t_length
    else 0
  in
  let pos = M.bin_write_t bigstring ~pos t in
  assert (pos = bigstring_length);
  bigstring
;;

module Of_binable_with_uuid = Stable.Of_binable.V2
module Of_binable1_with_uuid = Stable.Of_binable1.V2
module Of_binable2_with_uuid = Stable.Of_binable2.V2
module Of_binable3_with_uuid = Stable.Of_binable3.V2
module Of_sexpable_with_uuid = Stable.Of_sexpable.V2
module Of_stringable_with_uuid = Stable.Of_stringable.V2
module Of_binable_without_uuid = Stable.Of_binable.V1
module Of_binable1_without_uuid = Stable.Of_binable1.V1
module Of_binable2_without_uuid = Stable.Of_binable2.V1
module Of_binable3_without_uuid = Stable.Of_binable3.V1
module Of_sexpable_without_uuid = Stable.Of_sexpable.V1
module Of_stringable_without_uuid = Stable.Of_stringable.V1

let%test_module _ =
  (module struct
    module type S_only_functions_and_shape = sig
      include S_only_functions

      val bin_shape_t : Shape.t
    end

    (* Check that only the functions & shape are sufficient for [@@deriving bin_io]. The
       fact that this functor typechecks is, itself, the test. *)
    module _ (X : S_only_functions_and_shape) : S = struct
      type t = X.t [@@deriving bin_io]
    end
  end)
;;
