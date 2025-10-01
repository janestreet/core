open! Std_internal
open! Import
module String = Base.String

module Time = struct
  include Time.Make (Time_float0) ()
  include Time_float0
end

include Time

module Span = struct
  include Time.Span

  let%template arg_type = (Command.Arg_type.create [@mode portable]) of_string
end

module Zone = struct
  include Time.Zone

  include (
    Timezone :
    sig
    @@ portable
      include Timezone.Extend_zone with type t := t
    end)

  let%template arg_type = (Command.Arg_type.create [@mode portable]) of_string
end

module Ofday = struct
  include Time.Ofday

  let%template arg_type = (Command.Arg_type.create [@mode portable]) of_string
  let now ~zone = Time.to_ofday ~zone (Time.now ())

  module Zoned = struct
    type t =
      { ofday : Time.Ofday.t
      ; zone : Zone.t
      }
    [@@deriving
      bin_io, fields ~getters ~local_getters, compare ~localize, equal ~localize, hash]

    type sexp_repr = Time.Ofday.t * Zone.t [@@deriving sexp]

    let sexp_of_t t = [%sexp_of: sexp_repr] (t.ofday, t.zone)

    let t_of_sexp sexp =
      let ofday, zone = [%of_sexp: sexp_repr] sexp in
      { ofday; zone }
    ;;

    let to_time t date = Time.of_date_ofday ~zone:(zone t) date (ofday t)
    let create ofday zone = { ofday; zone }
    let create_local ofday = create ofday (Portable_lazy.force Zone.local_portable)

    let of_string string : t =
      match String.rsplit2 string ~on:' ' with
      | Some (ofday, zone) ->
        { ofday = Time.Ofday.of_string ofday; zone = Zone.of_string zone }
      | None -> failwithf "Ofday.Zoned.of_string %s" string ()
    ;;

    let to_string (t : t) : string =
      String.concat [ Time.Ofday.to_string t.ofday; " "; Zone.to_string t.zone ]
    ;;

    let to_string_trimmed (t : t) : string =
      String.concat [ Time.Ofday.to_string_trimmed t.ofday; " "; Zone.to_string t.zone ]
    ;;

    let%template arg_type = (Command.Arg_type.create [@mode portable]) of_string

    module With_nonchronological_compare = struct
      type nonrec t = t
      [@@deriving bin_io, compare ~localize, equal ~localize, sexp, hash]
    end

    include%template Pretty_printer.Register [@modality portable] (struct
        type nonrec t = t

        let to_string = to_string
        let module_name = "Core.Time_float.Ofday.Zoned"
      end)
  end
end

exception Time_string_not_absolute of string [@@deriving sexp]

module T_without_Map_and_Set = struct
  include (
    Time :
      module type of Time
      with module Zone := Time.Zone
       and module Ofday := Time.Ofday
       and module Span := Time.Span
       and module Stable := Time.Stable)

  let of_date_ofday_zoned date ofday_zoned = Ofday.Zoned.to_time ofday_zoned date

  let to_date_ofday_zoned t ~zone =
    let date, ofday = to_date_ofday t ~zone in
    date, Ofday.Zoned.create ofday zone
  ;;

  let to_ofday_zoned t ~zone =
    let ofday = to_ofday t ~zone in
    Ofday.Zoned.create ofday zone
  ;;

  let of_string_fix_proto utc str =
    try
      let expect_length = 21 in
      (* = 8 + 1 + 12 *)
      let expect_dash = 8 in
      if Char.( <> ) str.[expect_dash] '-'
      then failwithf "no dash in position %d" expect_dash ();
      let zone =
        match utc with
        | `Utc -> Zone.utc
        | `Local -> Portable_lazy.force Zone.local_portable
      in
      if Int.( > ) (String.length str) expect_length then failwithf "input too long" ();
      of_date_ofday
        ~zone
        (Date0.of_string_iso8601_basic str ~pos:0)
        (Ofday.of_string_iso8601_extended str ~pos:(expect_dash + 1))
    with
    | exn -> invalid_argf "Time.of_string_fix_proto %s: %s" str (Exn.to_string exn) ()
  ;;

  let to_string_fix_proto utc t =
    let zone =
      match utc with
      | `Utc -> Zone.utc
      | `Local -> Portable_lazy.force Zone.local_portable
    in
    let date, sec = to_date_ofday t ~zone in
    Date0.to_string_iso8601_basic date ^ "-" ^ Ofday.to_millisecond_string sec
  ;;

  let to_string t = to_string_abs t ~zone:(Portable_lazy.force Zone.local_portable)

  let ensure_colon_in_offset offset =
    if Char.( = ) offset.[1] ':' || Char.( = ) offset.[2] ':'
    then offset
    else (
      let offset_length = String.length offset in
      if Int.( < ) offset_length 3 || Int.( > ) offset_length 4
      then failwithf "invalid offset %s" offset ()
      else
        String.concat
          [ String.sub offset ~pos:0 ~len:(offset_length - 2)
          ; ":"
          ; String.sub offset ~pos:(offset_length - 2) ~len:2
          ])
  ;;

  let of_string_gen ~if_no_timezone s =
    let default_zone () =
      match if_no_timezone with
      | `Fail -> raise (Time_string_not_absolute s)
      | `Local -> Portable_lazy.force Zone.local_portable
      | `Use_this_one zone -> zone
    in
    of_string_gen ~default_zone ~find_zone:Zone.find_exn s
  ;;

  let of_string_abs s = of_string_gen ~if_no_timezone:`Fail s
  let of_string s = of_string_gen ~if_no_timezone:`Local s
  let%template arg_type = (Command.Arg_type.create [@mode portable]) of_string_abs

  include%template Pretty_printer.Register [@modality portable] (struct
      type nonrec t = t

      let to_string = to_string
      let module_name = "Core.Time_float"
    end)

  let sexp_zone = Atomic.make Zone.local_portable
  let get_sexp_zone () = Portable_lazy.force (Atomic.get sexp_zone)
  let set_sexp_zone zone = Atomic.set sexp_zone (Portable_lazy.from_val zone)

  let t_of_sexp_gen ~if_no_timezone sexp =
    try
      match sexp with
      | Sexp.List [ Sexp.Atom date; Sexp.Atom ofday; Sexp.Atom tz ] ->
        of_date_ofday
          ~zone:(Zone.find_exn tz)
          (Date0.of_string date)
          (Ofday.of_string ofday)
      (* This is actually where the output of [sexp_of_t] is handled, since that's e.g.
         (2015-07-06 09:09:44.787988+01:00). *)
      | Sexp.List [ Sexp.Atom date; Sexp.Atom ofday_and_possibly_zone ] ->
        of_string_gen ~if_no_timezone (date ^ " " ^ ofday_and_possibly_zone)
      | Sexp.Atom datetime -> of_string_gen ~if_no_timezone datetime
      | _ -> of_sexp_error "Time.t_of_sexp" sexp
    with
    | Of_sexp_error _ as e -> raise e
    | e -> of_sexp_error (sprintf "Time.t_of_sexp: %s" (Exn.to_string e)) sexp
  ;;

  let t_of_sexp sexp =
    t_of_sexp_gen sexp ~if_no_timezone:(`Use_this_one (get_sexp_zone ()))
  ;;

  let t_sexp_grammar : t Sexplib.Sexp_grammar.t =
    { untyped =
        Union
          [ String
          ; List (Cons (String, Cons (String, Empty)))
          ; List (Cons (String, Cons (String, Cons (String, Empty))))
          ]
    }
  ;;

  let t_of_sexp_abs sexp = t_of_sexp_gen sexp ~if_no_timezone:`Fail

  let sexp_of_t_abs t ~zone =
    Sexp.List (List.map (Time.to_string_abs_parts ~zone t) ~f:(fun s -> Sexp.Atom s))
  ;;

  let sexp_of_t t = sexp_of_t_abs ~zone:(get_sexp_zone ()) t

  module Exposed_for_tests = struct
    let ensure_colon_in_offset = ensure_colon_in_offset
  end
end

module T = struct
  include T_without_Map_and_Set

  include (
  struct
    module C = struct
      type nonrec t = t [@@deriving bin_io]
      type nonrec comparator_witness = comparator_witness

      let comparator = comparator
      let sexp_of_t = sexp_of_t

      (* In 108.06a and earlier, times in sexps of Maps and Sets were raw floats.  From
         108.07 through 109.13, the output format remained raw as before, but both the raw
         and pretty format were accepted as input.  From 109.14 on, the output format was
         changed from raw to pretty, while continuing to accept both formats.  Once we
         believe most programs are beyond 109.14, we will switch the input format to no
         longer accept raw. *)
      let t_of_sexp sexp =
        match
          Option.try_with (fun () ->
            of_span_since_epoch (Span.of_sec (Float.t_of_sexp sexp)))
        with
        | Some t -> t
        | None -> t_of_sexp sexp
      ;;
    end

    include C
    module%template Map = Map.Make_binable_using_comparator [@modality portable] (C)
    module%template Set = Set.Make_binable_using_comparator [@modality portable] (C)
  end :
  sig
  @@ portable
    include
      Comparable.Map_and_set_binable
      with type t := t
       and type comparator_witness := comparator_witness
  end)

  let%test _ =
    Set.equal
      (Set.of_list [ epoch ])
      (Set.t_of_sexp
         (Sexp.List [ Float.sexp_of_t (Span.to_sec (to_span_since_epoch epoch)) ]))
  ;;
end

include%template Diffable.Atomic.Make [@modality portable] (T)

(* Previous versions rendered hash-based containers using float serialization rather than
   time serialization, so when reading hash-based containers in we accept either
   serialization. *)
  include%template Hashable.Make_binable [@modality portable] (struct
      type t = Time.t [@@deriving bin_io, compare ~localize, hash]

      let sexp_of_t = T.sexp_of_t

      let t_of_sexp sexp =
        match Float.t_of_sexp sexp with
        | float -> Time.of_span_since_epoch (Time.Span.of_sec float)
        | exception _ -> T.t_of_sexp sexp
      ;;
    end)

module Stable = struct
  module V1_without_Map_and_Set = struct
    (* There is no simple, pristine implementation of "stable time", and in fact
       [Time.Stable.V1] has always called out to "unstable" string conversions.
       For a complicated "stable" story like this, we rely on comprehensive tests
       of stability; see [lib/core/test/src/test_time.ml]. *)
    include T_without_Map_and_Set

    include%template Diffable.Atomic.Make [@modality portable] (T)

    let stable_witness : t Stable_witness.t = Stable_witness.assert_stable
  end

  module V1 = struct
    include V1_without_Map_and_Set

    module Map = struct
      include T.Map

      let stable_witness _ = Stable_witness.assert_stable
    end

    module Set = struct
      include T.Set

      let stable_witness = Stable_witness.assert_stable
    end
  end

  module With_utc_sexp = struct
    module V1 = struct
      module C = struct
        include V1_without_Map_and_Set

        let sexp_of_t t = sexp_of_t_abs t ~zone:Zone.utc
      end

      include C
      module%template Map = Map.Make_binable_using_comparator [@modality portable] (C)
      module%template Set = Set.Make_binable_using_comparator [@modality portable] (C)
    end

    module V2 = struct
      module C = struct
        type nonrec t = t
        [@@deriving bin_io ~localize, compare ~localize, equal ~localize, hash]

        let sexp_of_t t = [%sexp (to_string_abs_parts t ~zone:Zone.utc : string list)]
        let stable_witness = Stable_witness.assert_stable

        let t_of_sexp sexp =
          try
            match sexp with
            | Sexp.List [ Sexp.Atom date; Sexp.Atom ofday_and_possibly_zone ] ->
              of_string_gen
                ~default_zone:(fun () -> Zone.utc)
                ~find_zone:(fun _ ->
                  of_sexp_error
                    "Time.Stable.With_utc.V2.t_of_sexp: unknown time zone"
                    sexp)
                (date ^ " " ^ ofday_and_possibly_zone)
            | _ -> of_sexp_error "Time.Stable.With_utc.V2.t_of_sexp" sexp
          with
          | Of_sexp_error _ as e -> raise e
          | e ->
            of_sexp_error
              (sprintf "Time.Stable.With_utc.V2.t_of_sexp: %s" (Exn.to_string e))
              sexp
        ;;

        let t_sexp_grammar = Sexplib.Sexp_grammar.coerce Sexplib.Sexp.t_sexp_grammar

        type comparator_witness = T.comparator_witness

        let comparator = T.comparator
      end

      include C

      include%template
        Comparable.Stable.V1.With_stable_witness.Make [@modality portable] (C)
    end
  end

  module With_t_of_sexp_abs = struct
    module V1 = struct
      include V1_without_Map_and_Set

      let t_of_sexp = t_of_sexp_abs
    end
  end

  module Span = Time.Stable.Span

  module Ofday = struct
    include Time.Stable.Ofday

    module Zoned = struct
      module V1 = struct
        open Ofday.Zoned

        type nonrec t = t [@@deriving hash]

        [%%rederive
          type t = With_nonchronological_compare.t
          [@@deriving compare ~localize ~portable, equal ~localize ~portable]]

        module Bin_repr = struct
          type t =
            { ofday : Time.Stable.Ofday.V1.t
            ; zone : Timezone.Stable.V1.t
            }
          [@@deriving bin_io ~localize, stable_witness]
        end

        let%template[@alloc a @ m = (heap_global, stack_local)] to_binable t : Bin_repr.t =
          { ofday = (ofday [@mode m]) t; zone = (zone [@mode m]) t } [@exclave_if_stack a]
        ;;

        let of_binable (repr : Bin_repr.t) = create repr.ofday repr.zone

        include%template
          Binable.Stable.Of_binable.V1
            [@mode local]
            [@modality portable]
            [@alert "-legacy"]
            (Bin_repr)
            (struct
              type nonrec t = t

              let to_binable = to_binable
              let%template[@mode local] to_binable = (to_binable [@alloc stack])
              let of_binable = of_binable
            end)

        let stable_witness =
          Stable_witness.of_serializable
            [%stable_witness: Bin_repr.t]
            of_binable
            to_binable
        ;;

        let%expect_test _ =
          print_endline [%bin_digest: t];
          [%expect {| 490573c3397b4fe37e8ade0086fb4759 |}]
        ;;

        type sexp_repr = Time.Stable.Ofday.V1.t * Timezone.Stable.V1.t
        [@@deriving sexp, sexp_grammar]

        let sexp_of_t t = [%sexp_of: sexp_repr] (ofday t, zone t)

        let t_of_sexp sexp =
          let ofday, zone = [%of_sexp: sexp_repr] sexp in
          create ofday zone
        ;;

        let t_sexp_grammar = Sexplib.Sexp_grammar.coerce sexp_repr_sexp_grammar
      end
    end
  end

  module Zone = Timezone.Stable
end

include T
