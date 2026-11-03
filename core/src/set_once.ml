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
      type 'a t = 'a option ref [@@deriving bin_io, sexp, sexp_grammar]
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

    let t_sexp_grammar : 'a Sexplib.Sexp_grammar.t -> 'a t Sexplib.Sexp_grammar.t =
      fun a_sexp_grammar ->
      Sexplib.Sexp_grammar.coerce (Format.t_sexp_grammar a_sexp_grammar)
    ;;
  end
end

open! Import
module Unstable = Stable.V1
open Stable.T

type nonrec 'a t = 'a t =
  { mutable value : 'a Option.t
  ; mutable set_at : Source_code_position.t
  }

[%%rederive.portable type nonrec 'a t = 'a t [@@deriving compare, equal]]

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

let create_full ?(here = Stdlib.Lexing.dummy_pos) value =
  { value = Some value; set_at = here }
;;

let set_internal t here value =
  t.value <- Some value;
  t.set_at <- here
;;

let set_if_none t ?(here = Stdlib.Lexing.dummy_pos) value =
  if Option.is_none t.value then set_internal t here value
;;

let set t ?(here = Stdlib.Lexing.dummy_pos) value =
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

let set_exn t ?(here = Stdlib.Lexing.dummy_pos) value =
  Or_error.ok_exn (set t ~here value)
;;

let get t = t.value

let get_exn ?(here = Stdlib.Lexing.dummy_pos) t =
  match t.value with
  | Some a -> a
  | None ->
    raise_s
      (if Source_code_position.is_dummy here
       then [%message "[Set_once.get_exn] unset"]
       else [%message "[Set_once.get_exn] unset" ~at:(here : Source_code_position.t)])
    |> Never_returns.never_returns
;;

let get_or_set_thunk ?(here = Stdlib.Lexing.dummy_pos) t ~f =
  match t.value with
  | Some a -> a
  | None ->
    let value = f () in
    set_internal t here value;
    value
;;

let is_none t = Option.is_none t.value
let is_some t = Option.is_some t.value
let iter t ~f = Option.iter t.value ~f

module Optional_syntax = struct
  module Optional_syntax = struct
    let is_none t = is_none t
    let unsafe_value t = get_exn t
  end
end

include
  Quickcheckable.Of_quickcheckable1
    (Option)
    (struct
      type nonrec 'a t = 'a t

      let to_quickcheckable { value; set_at = _ } = value
      let of_quickcheckable value = { value; set_at = [%here] }
    end)

module Atomic = struct
  type 'a inner =
    { value : 'a option
        (* we still store an option here to avoid allocating in the [get] case. *)
    ; set_at : Source_code_position.Stable.V1.t
    }
  [@@deriving sexp_of]

  type 'a t = 'a inner or_null Atomic.t

  let invariant (type a) (invariant_a : a -> unit) t =
    match Atomic.get t with
    | Null -> ()
    | This { value; set_at = _ } ->
      (match value with
       | None ->
         raise_s
           [%message
             "invariant failed"
               "Core.Set_once.Atomic: [t] is [This] but [value] is [None]"]
       | Some value -> invariant_a value)
  ;;

  let sexp_of_t (type a) (sexp_of_a : a -> Sexp.t) t =
    match Atomic.get t with
    | Null -> [%message "unset"]
    | This { value = Some value; set_at } ->
      if Source_code_position.is_dummy set_at
      then [%message "" (value : a)]
      else [%message "" (value : a) ~set_at:(set_at |> Source_code_position.to_string)]
    | This { value = None; _ } -> assert false
  ;;

  let create () = Atomic.make Null

  let create_full ?(here = Stdlib.Lexing.dummy_pos) value =
    Atomic.make (This { value = Some value; set_at = here })
  ;;

  let set_internal t ~here value =
    let inner = { value = Some value; set_at = here } in
    Atomic.compare_and_set t ~if_phys_equal_to:Null ~replace_with:(This inner)
  ;;

  let set_if_none t ?(here = Stdlib.Lexing.dummy_pos) value =
    match set_internal t ~here value with
    | Compare_failed | Set_here -> ()
  ;;

  let set t ?(here = Stdlib.Lexing.dummy_pos) value =
    match set_internal t ~here value with
    | Set_here -> Ok ()
    | Compare_failed ->
      let inner =
        match Atomic.get t with
        | Null -> assert false
        | This inner -> inner
      in
      Or_error.error_s
        [%message
          "[Set_once.Atomic.set_exn] already set"
            ~setting_at:(here : Source_code_position.t)
            ~previously_set_at:(inner.set_at : Source_code_position.t)]
  ;;

  let set_exn t ?(here = Stdlib.Lexing.dummy_pos) value =
    Or_error.ok_exn (set t ~here value)
  ;;

  let get t =
    match Atomic.get t with
    | Null -> None
    | This inner -> inner.value
  ;;

  let get_exn ?(here = Stdlib.Lexing.dummy_pos) t =
    match get t with
    | Some value -> value
    | None ->
      raise_s
        (if Source_code_position.is_dummy here
         then [%message "[Set_once.Atomic.get_exn] unset"]
         else
           [%message
             "[Set_once.Atomic.get_exn] unset" ~at:(here : Source_code_position.t)])
  ;;

  let is_none t = Option.is_none (get t)
  let is_some t = Option.is_some (get t)

  let iter t ~f =
    match get t with
    | None -> ()
    | Some value -> f value
  ;;
end
