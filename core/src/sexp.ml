open! Import

module Stable = struct
  module V1 = struct
    include struct
      (* inherit previously derived values, rather than re-deriving from the type *)
      type t = Base.Sexp.t
      [@@deriving compare ~localize, equal ~localize, globalize, hash]
    end

    type t = Base.Sexp.t =
      | Atom of string
      | List of t list
    [@@deriving bin_io, stable_witness]

    let t_sexp_grammar = Sexplib.Sexp.t_sexp_grammar
    let t_of_sexp = Sexplib.Sexp.t_of_sexp
    let sexp_of_t = Sexplib.Sexp.sexp_of_t
  end
end

include Stable.V1

include (
  Base.Sexp :
    module type of struct
      include Base.Sexp
    end
    with type t := t)

include (
  Sexplib.Sexp :
    module type of struct
      include Sexplib.Sexp
    end
    with type t := t)

module O = struct
  type sexp = Base.Sexp.t =
    | Atom of string
    | List of t list
end

module Sexp_maybe = struct
  type nonrec 'a t = ('a, t * Error.t) Result.t [@@deriving bin_io, compare, hash]

  let sexp_of_t sexp_of_a t =
    match t with
    | Result.Ok a -> sexp_of_a a
    | Result.Error (sexp, err) ->
      List [ Atom "sexp_parse_error"; sexp; Error.sexp_of_t err ]
  ;;

  let t_of_sexp a_of_sexp sexp =
    match sexp with
    | List [ Atom "sexp_parse_error"; sexp; _ ] | sexp ->
      (try Result.Ok (a_of_sexp sexp) with
       | exn -> Result.Error (sexp, Error.of_exn exn))
  ;;

  let t_sexp_grammar (grammar : _ Sexplib.Sexp_grammar.t) : _ t Sexplib.Sexp_grammar.t =
    { untyped = Union [ grammar.untyped; Base.Sexp.t_sexp_grammar.untyped ] }
  ;;
end

module With_text = struct
  open Result.Export

  type 'a t =
    { value : 'a
    ; text : string
    }
  [@@deriving bin_io]

  let sexp_of_t _ t = Atom t.text

  let of_text value_of_sexp ?(filename = "") text =
    match Or_error.try_with (fun () -> of_string_conv text value_of_sexp) with
    | Ok (`Result value) -> Ok { value; text }
    | Error _ as err -> err
    | Ok (`Error (exn, annotated)) ->
      Error (Error.of_exn (Annotated.get_conv_exn annotated ~file:filename ~exc:exn))
  ;;

  let t_of_sexp a_of_sexp sexp =
    match sexp with
    | List _ ->
      of_sexp_error
        "With_text.t should be stored as an atom, but instead a list was found."
        sexp
    | Atom text -> of_text a_of_sexp text |> Or_error.ok_exn
  ;;

  let t_sexp_grammar _ = Sexplib.Sexp_grammar.coerce Base.String.t_sexp_grammar
  let text t = t.text
  let value t = t.value

  let of_value sexp_of_value value =
    let text = sexp_of_value value |> to_string_hum in
    { value; text }
  ;;
end

type 'a no_raise = 'a [@@deriving bin_io, sexp]

let sexp_of_no_raise sexp_of_a a =
  try sexp_of_a a with
  | exn ->
    (try List [ Atom "failure building sexp"; sexp_of_exn exn ] with
     | _ -> Atom "could not build sexp for exn raised when building sexp for value")
;;

include Comparable.Extend (Base.Sexp) (Base.Sexp)

let of_sexp_allow_extra_fields_recursively of_sexp sexp =
  let r = Sexplib.Conv.record_check_extra_fields in
  let prev = !r in
  Exn.protect
    ~finally:(fun () -> r := prev)
    ~f:(fun () ->
      r := false;
      of_sexp sexp)
;;

let quickcheck_generator = Base_quickcheck.Generator.sexp
let quickcheck_observer = Base_quickcheck.Observer.sexp
let quickcheck_shrinker = Base_quickcheck.Shrinker.sexp
