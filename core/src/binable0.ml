open! Import
include Binable_intf
include Bin_prot.Binable
module Shape = Bin_prot.Shape

module Stable = struct
  module%template Of_binable = struct
    [@@@mode.default m = (global, local)]

    module%template.portable
      [@modality p] V1
        (Binable : Minimal.S
      [@mode m])
        (M : Conv_without_uuid [@mode m] with type binable := Binable.t) :
      S [@mode m] with type t := M.t =
    Bin_prot.Utils.Make_binable_without_uuid [@mode m] [@modality p] (struct
        module Binable = Binable
        include M
      end)
    [@@alert "-legacy"]

    module%template.portable
      [@modality p] V2
        (Binable : Minimal.S
      [@mode m])
        (M : Conv [@mode m] with type binable := Binable.t) :
      S [@mode m] with type t := M.t =
    Bin_prot.Utils.Make_binable_with_uuid [@mode m] [@modality p] (struct
        module Binable = Binable
        include M
      end)
  end

  module%template Of_binable1 = struct
    [@@@mode.default m = (global, local)]
    [@@@kind.default ka = (value, any)]

    module%template.portable
      [@modality p] V1
        (Binable : Minimal.S1
      [@kind ka] [@mode m])
        (M : Conv1_without_uuid
             [@kind ka] [@mode m]
             with type ('a : ka) binable := 'a Binable.t) :
      S1 [@kind ka] [@mode m] with type ('a : ka) t := 'a M.t =
    Bin_prot.Utils.Make_binable1_without_uuid [@kind ka] [@mode m] [@modality p] (struct
        module Binable = Binable
        include M
      end)
    [@@alert "-legacy"]

    module%template.portable
      [@modality p] V2
        (Binable : Minimal.S1
      [@kind ka] [@mode m])
        (M : Conv1 [@kind ka] [@mode m] with type ('a : ka) binable := 'a Binable.t) :
      S1 [@kind ka] [@mode m] with type ('a : ka) t := 'a M.t =
    Bin_prot.Utils.Make_binable1_with_uuid [@kind ka] [@mode m] [@modality p] (struct
        module Binable = Binable
        include M
      end)
  end

  module%template Of_binable2 = struct
    [@@@mode.default m = (global, local)]
    [@@@kind.default ka = (value, any), kb = (value, any)]

    module%template.portable
      [@modality p] V1
        (Binable : Minimal.S2
      [@kind ka kb] [@mode m])
        (M : Conv2_without_uuid
             [@kind ka kb] [@mode m]
             with type ('a : ka, 'b : kb) binable := ('a, 'b) Binable.t) :
      S2 [@kind ka kb] [@mode m] with type ('a : ka, 'b : kb) t := ('a, 'b) M.t =
    Bin_prot.Utils.Make_binable2_without_uuid [@kind ka kb] [@mode m] [@modality p] (struct
        module Binable = Binable
        include M
      end)
    [@@alert "-legacy"]

    module%template.portable
      [@modality p] V2
        (Binable : Minimal.S2
      [@kind ka kb] [@mode m])
        (M : Conv2
             [@kind ka kb] [@mode m]
             with type ('a : ka, 'b : kb) binable := ('a, 'b) Binable.t) :
      S2 [@kind ka kb] [@mode m] with type ('a : ka, 'b : kb) t := ('a, 'b) M.t =
    Bin_prot.Utils.Make_binable2_with_uuid [@kind ka kb] [@mode m] [@modality p] (struct
        module Binable = Binable
        include M
      end)
  end

  module%template Of_binable3 = struct
    [@@@mode.default m = (global, local)]
    [@@@kind.default ka = (value, any), kb = (value, any), kc = (value, any)]

    module%template.portable
      [@modality p] V1
        (Binable : Minimal.S3
      [@kind ka kb kc] [@mode m])
        (M : Conv3_without_uuid
             [@kind ka kb kc] [@mode m]
             with type ('a : ka, 'b : kb, 'c : kc) binable := ('a, 'b, 'c) Binable.t) :
      S3
      [@kind ka kb kc] [@mode m]
      with type ('a : ka, 'b : kb, 'c : kc) t := ('a, 'b, 'c) M.t =
    Bin_prot.Utils.Make_binable3_without_uuid [@kind ka kb kc] [@mode m] [@modality p] (struct
        module Binable = Binable
        include M
      end)
    [@@alert "-legacy"]

    module%template.portable
      [@modality p] V2
        (Binable : Minimal.S3
      [@kind ka kb kc] [@mode m])
        (M : Conv3
             [@kind ka kb kc] [@mode m]
             with type ('a : ka, 'b : kb, 'c : kc) binable := ('a, 'b, 'c) Binable.t) :
      S3
      [@kind ka kb kc] [@mode m]
      with type ('a : ka, 'b : kb, 'c : kc) t := ('a, 'b, 'c) M.t =
    Bin_prot.Utils.Make_binable3_with_uuid [@kind ka kb kc] [@mode m] [@modality p] (struct
        module Binable = Binable
        include M
      end)
  end

  module Of_sexpable = struct
    module%template.portable
      [@modality p] V1 (M : sig
        type t : value_or_null

        include Sexpable.S with type t := t
      end) =
      Of_binable.V1 [@modality p]
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

    module%template.portable [@modality p] V2 (M : Conv_sexpable) =
      Of_binable.V2 [@modality p]
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
    module%template.portable [@modality p] V1 (M : Stringable.S) =
    Bin_prot.Utils.Make_binable_without_uuid [@modality p] (struct
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

    module%template.portable [@modality p] V2 (M : Conv_stringable) =
    Bin_prot.Utils.Make_binable_with_uuid [@modality p] (struct
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

(* Using the [Bigstring] module would introduce a cyclic dependency. *)
let create_bigstring size = Array1.create Bigarray.char Bigarray.c_layout size

[%%template
[@@@mode.default m = (global, local)]

type ('a : any) m = ((module S with type t = 'a)[@mode m])

let of_bigstring (type a : value_or_null) m bigstring =
  let module M = (val (m : (a m[@mode m]))) in
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

let to_bigstring ?(prefix_with_length = false) (type a : value_or_null) m t =
  let module M = (val (m : (a m[@mode m]))) in
  let t_length = (M.bin_size_t [@mode m]) t in
  let bigstring_length =
    if prefix_with_length then t_length + 8 (* the size of a 64-bit int *) else t_length
  in
  let bigstring = create_bigstring bigstring_length in
  let pos =
    if prefix_with_length
    then Bin_prot.Write.bin_write_int_64bit bigstring ~pos:0 t_length
    else 0
  in
  let pos = (M.bin_write_t [@mode m]) bigstring ~pos t in
  assert (pos = bigstring_length);
  bigstring
;;

[@@@modality.default p = (portable, nonportable)]

module Of_binable_with_uuid = Stable.Of_binable.V2 [@mode m] [@modality p]
module Of_binable_without_uuid = Stable.Of_binable.V1 [@mode m] [@modality p]

[@@@kind.default ka = (value, any)]

module Of_binable1_with_uuid = Stable.Of_binable1.V2 [@kind ka] [@mode m] [@modality p]
module Of_binable1_without_uuid = Stable.Of_binable1.V1 [@kind ka] [@mode m] [@modality p]

[@@@kind.default kb = (value, any)]

module Of_binable2_with_uuid = Stable.Of_binable2.V2 [@kind ka kb] [@mode m] [@modality p]

module Of_binable2_without_uuid =
  Stable.Of_binable2.V1
  [@kind ka kb]
  [@mode m]
  [@modality p]

[@@@kind.default kc = (value, any)]

module Of_binable3_with_uuid =
  Stable.Of_binable3.V2
  [@kind ka kb kc]
  [@mode m]
  [@modality p]

module Of_binable3_without_uuid =
  Stable.Of_binable3.V1
  [@kind ka kb kc]
  [@mode m]
  [@modality p]]

[%%template
[@@@modality.default p = (portable, nonportable)]

module Of_sexpable_with_uuid = Stable.Of_sexpable.V2 [@modality p]
module Of_stringable_with_uuid = Stable.Of_stringable.V2 [@modality p]
module Of_sexpable_without_uuid = Stable.Of_sexpable.V1 [@modality p]
module Of_stringable_without_uuid = Stable.Of_stringable.V1 [@modality p]]

module%test _ = struct
  module type S_only_functions_and_shape = sig
    include S_only_functions

    val bin_shape_t : Shape.t
  end

  (* Check that only the functions & shape are sufficient for [@@deriving bin_io]. The
       fact that this functor typechecks is, itself, the test. *)
  module _ (X : sig
      type t

      include S_only_functions_and_shape with type t := t
    end) : S = struct
    type t = X.t [@@deriving bin_io]
  end
end
