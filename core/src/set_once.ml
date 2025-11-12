module Stable = struct
  open Stable_internal

  module T = struct
    type%template ('a : k) t =
      { mutable value : ('a Option.t[@kind k])
      ; mutable set_at : Source_code_position.Stable.V1.t
           [@compare.ignore] [@equal.ignore]
      }
    [@@deriving compare ~localize, equal ~localize] [@@kind k = base]
  end

  module V1 = struct
    module Format = struct
      type 'a t = 'a option ref [@@deriving bin_io, sexp, sexp_grammar]
    end

    include T

    let of_format (v1 : 'a Format.t) : 'a t = { value = !v1; set_at = [%here] }
    let to_format (t : 'a t) : 'a Format.t = ref t.value

    include%template
      Binable.Of_binable1_without_uuid [@modality portable] [@alert "-legacy"]
        (Format)
        (struct
          include T

          let of_binable = of_format
          let to_binable = to_format
        end)

    include%template
      Sexpable.Of_sexpable1 [@modality portable]
        (Format)
        (struct
          include T

          let of_sexpable = of_format
          let to_sexpable = to_format
        end)

    let t_sexp_grammar : 'a Sexplib.Sexp_grammar.t -> 'a t Sexplib.Sexp_grammar.t =
      fun a_sexp_grammar ->
      Sexplib.Sexp_grammar.coerce (Format.t_sexp_grammar a_sexp_grammar)
    ;;
  end
end

open! Import
module Unstable = Stable.V1
open Stable.T

[%%template
[@@@kind.default k = base]

type nonrec ('a : k) t = ('a t[@kind k]) =
  { mutable value : ('a Option.t[@kind k])
  ; mutable set_at : Source_code_position.t
  }

[%%rederive.portable
  type nonrec ('a : k) t = ('a t[@kind k])
  [@@deriving compare ~localize, equal ~localize] [@@kind k]]

let sexp_of_t sexp_of_a { value; set_at } =
  match value with
  | None -> [%message "unset"]
  | Some value ->
    if Source_code_position.is_dummy set_at
    then [%message "" (value : a)]
    else [%message "" (value : a) ~set_at:(set_at |> Source_code_position.to_string)]
;;

let invariant invariant_a t =
  match t.value with
  | None -> ()
  | Some a -> invariant_a a
;;

let create () = { value = None; set_at = [%here] }
let create_full ~(here : [%call_pos]) value = { value = Some value; set_at = here }

let set_internal t here value =
  t.value <- Some value;
  t.set_at <- here
;;

let set_if_none t ~(here : [%call_pos]) value =
  if (Option.is_none [@kind k]) t.value then (set_internal [@kind k]) t here value
;;

let set t ~(here : [%call_pos]) value =
  if (Option.is_none [@kind k]) t.value
  then (
    (set_internal [@kind k]) t here value;
    Ok ())
  else
    Or_error.error_s
      [%message
        "[Set_once.set_exn] already set"
          ~setting_at:(here : Source_code_position.t)
          ~previously_set_at:(t.set_at : Source_code_position.t)]
;;

let set_exn t ~(here : [%call_pos]) value =
  Or_error.ok_exn ((set [@kind k]) t ~here value)
;;

let get t = t.value

let get_exn ~(here : [%call_pos]) t =
  match t.value with
  | Some a -> a
  | None ->
    raise_s
      (if Source_code_position.is_dummy here
       then [%message "[Set_once.get_exn] unset"]
       else [%message "[Set_once.get_exn] unset" ~at:(here : Source_code_position.t)])
    |> (Never_returns.never_returns [@kind k])
;;

let get_or_set_thunk ~(here : [%call_pos]) t ~f =
  match t.value with
  | Some a -> a
  | None ->
    let value = f () in
    (set_internal [@kind k]) t here value;
    value
;;

let is_none t = (Option.is_none [@kind k]) t.value
let is_some t = (Option.is_some [@kind k]) t.value
let iter t ~f = (Option.iter [@kind k]) t.value ~f

module Optional_syntax = struct
  module Optional_syntax = struct
    let is_none = (is_none [@kind k])
    let unsafe_value t = (get_exn [@kind k]) t
  end
end]

include%template
  Quickcheckable.Of_quickcheckable1 [@modality portable]
    (Option)
    (struct
      type nonrec 'a t = 'a t

      let to_quickcheckable { value; set_at = _ } = value
      let of_quickcheckable value = { value; set_at = [%here] }
    end)
