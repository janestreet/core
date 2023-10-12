module Stable = struct
  open Stable_internal

  module T = struct
    type 'a t =
      { mutable value : 'a Option.t
      ; mutable set_at : Source_code_position.Stable.V1.t
           [@compare.ignore] [@equal.ignore]
      }
    [@@deriving compare, equal]
  end

  module V1 = struct
    module Format = struct
      type 'a t = 'a option ref [@@deriving bin_io, sexp]
    end

    include T

    let of_format (v1 : 'a Format.t) : 'a t = { value = !v1; set_at = [%here] }
    let to_format (t : 'a t) : 'a Format.t = ref t.value

    include
      Binable.Of_binable1_without_uuid [@alert "-legacy"]
        (Format)
        (struct
          include T

          let of_binable = of_format
          let to_binable = to_format
        end)

    include
      Sexpable.Of_sexpable1
        (Format)
        (struct
          include T

          let of_sexpable = of_format
          let to_sexpable = to_format
        end)
  end
end

open! Import
module Unstable = Stable.V1
open Stable.T

type 'a t = 'a Stable.T.t [@@deriving compare, equal]

let sexp_of_t sexp_of_a { value; set_at } =
  match value with
  | None -> [%message "unset"]
  | Some value ->
    [%message "" (value : a) ~set_at:(set_at |> Source_code_position.to_string)]
;;

let invariant invariant_a t =
  match t.value with
  | None -> ()
  | Some a -> invariant_a a
;;

let create () = { value = None; set_at = [%here] }

let set_internal t here value =
  t.value <- Some value;
  t.set_at <- here
;;

let set_if_none t here value = if Option.is_none t.value then set_internal t here value

let set t here value =
  if Option.is_none t.value
  then (
    set_internal t here value;
    Ok ())
  else
    Or_error.error_s
      [%message
        "[Set_once.set_exn] already set"
          ~setting_at:(here : Source_code_position.t)
          ~previously_set_at:(t.set_at : Source_code_position.t)]
;;

let set_exn t here value = Or_error.ok_exn (set t here value)
let get t = t.value

let get_exn (t : _ t) here =
  match t.value with
  | Some a -> a
  | None ->
    raise_s [%message "[Set_once.get_exn] unset" ~at:(here : Source_code_position.t)]
;;

let is_none t = Option.is_none t.value
let is_some t = Option.is_some t.value
let iter t ~f = Option.iter t.value ~f

module Optional_syntax = struct
  module Optional_syntax = struct
    let is_none = is_none
    let unsafe_value t = get_exn t [%here]
  end
end
