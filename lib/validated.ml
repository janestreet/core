open Std_internal

include Validated_intf

module Make (Raw : Raw) = struct

  type t = Raw.t with sexp_of

  let validation_failed t error =
    Error.create "validation failed" (t, error, Raw.here)
      <:sexp_of< Raw.t * Error.t * Source_code_position.t >>
  ;;

  let create_exn t =
    match Validate.result (Raw.validate t) with
    | Ok () -> t
    | Error error -> Error.raise (validation_failed t error)
  ;;

  let create t =
    match Validate.result (Raw.validate t) with
    | Ok () -> Ok t
    | Error error -> Error (validation_failed t error)
  ;;

  let t_of_sexp sexp = create_exn (Raw.t_of_sexp sexp)

  let raw t = t

end

module Make_binable (Raw : Raw_binable) = struct
  type t = Raw.t with bin_io
  include (Make (Raw) : Validated with type raw := Raw.t with type t := t)
end

TEST_MODULE = struct
  (* The [: Validated] is to remind us to add a unit test whenever the [Validated]
     interface changes. *)
  module M : Validated with type raw := int = struct

    module M = Make (struct
      type t = int with sexp
      let here = _here_
      let validate t =
        if t > 0
        then Validate.pass
        else Validate.fail "must be positive"
    end)

    open M

    type nonrec t = t

    let does_fail f = Result.is_error (Result.try_with f)

    let t_of_sexp = t_of_sexp
    let sexp_of_t = sexp_of_t

    TEST_UNIT = assert (does_fail (fun () -> t_of_sexp (<:sexp_of< int >> 0)))

    TEST_UNIT =
      let sexp = <:sexp_of< int >> 13 in
      assert (sexp_of_t (t_of_sexp sexp) = sexp);
    ;;

    let create     = create
    let create_exn = create_exn
    let raw        = raw

    TEST_UNIT = assert (does_fail (fun () -> create_exn 0))

    TEST_UNIT =
      match create 0 with
      | Error _ -> ()
      | Ok _ -> assert false
    ;;

    TEST_UNIT =
      let n = 13 in
      let t = create_exn n in
      assert (raw t = n);
    ;;

    TEST_UNIT =
      let n = 13 in
      match create n with
      | Error _ -> assert false
      | Ok t -> assert ((t :> int) = n);
    ;;

  end

end
