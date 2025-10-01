open! Import
open! Std_internal
include Timezone_intf
include Zone
module String = Base.String

module type Extend_zone = Timezone_intf.Extend_zone

module Zone_cache = struct
  type zone = t

  type t =
    { mutable full : bool
    ; basedir : string
    ; table : zone Hashtbl.M(String).t
    }

  let clear t =
    t.full <- false;
    Hashtbl.clear t.table
  ;;

  let find t zone = Hashtbl.find t.table zone

  let find_or_load t zonename =
    match find t zonename with
    | Some z -> Some z
    | None ->
      if t.full
      then None
      else (
        try
          let filename = t.basedir ^ "/" ^ zonename in
          let zone = input_tz_file ~zonename ~filename in
          Hashtbl.set t.table ~key:zonename ~data:zone;
          Some zone
        with
        | _ -> None)
  ;;

  let traverse basedir ~f =
    let skip_prefixes = [ "Etc/GMT"; "right/"; "posix/" ] in
    let maxdepth = 10 in
    let basedir_len = String.length basedir + 1 in
    let rec dfs dir depth =
      if depth < 1
      then ()
      else
        Array.iter (Stdlib.Sys.readdir dir) ~f:(fun fn ->
          let fn = dir ^ "/" ^ fn in
          let relative_fn = String.drop_prefix fn basedir_len in
          match Stdlib.Sys.is_directory fn with
          | true ->
            if not
                 (List.exists skip_prefixes ~f:(fun prefix ->
                    String.is_prefix ~prefix relative_fn))
            then dfs fn (depth - 1)
          | false -> f relative_fn)
    in
    dfs basedir maxdepth
  ;;

  let init_from_file_system t =
    if not t.full
    then (
      traverse t.basedir ~f:(fun zone_name ->
        ignore (find_or_load t zone_name : zone option));
      t.full <- true)
  ;;

  let to_alist t = Hashtbl.to_alist t.table

  let initialized_zones t =
    List.sort ~compare:(fun a b -> String.ascending (fst a) (fst b)) (to_alist t)
  ;;

  let find_or_load_matching t zone =
    let file_size filename =
      let c = Stdio.In_channel.create filename in
      let l = Stdio.In_channel.length c in
      Stdio.In_channel.close c;
      l
    in
    let t1_file_size = Option.map (original_filename zone) ~f:file_size in
    with_return (fun r ->
      let return_if_matches zone_name =
        let filename = String.concat ~sep:"/" [ t.basedir; zone_name ] in
        let matches =
          try
            [%compare.equal: int64 option] t1_file_size (Some (file_size filename))
            && [%compare.equal: Md5.t option]
                 (digest zone)
                 Option.(join (map (find_or_load t zone_name) ~f:digest))
          with
          | _ -> false
        in
        if matches then r.return (find_or_load t zone_name) else ()
      in
      List.iter (Atomic.get likely_machine_zones) ~f:return_if_matches;
      traverse t.basedir ~f:return_if_matches;
      None)
  ;;

  module The_one_and_only = struct
    module type M = sig
      type k

      val mutex : k Mutex.t
    end

    include
      (val let (P (type k) (key : k Capsule.Expert.Key.t)) = Capsule.Expert.create () in
           let mutex = Mutex.create key in
           (module struct
             type nonrec k = k

             let mutex = mutex
           end : M))

    let capsule : (t, k) Capsule.Data.t =
      Capsule.Data.create (fun () : t ->
        { full = false
        ; basedir = Option.value (Sys.getenv "TZDIR") ~default:"/usr/share/zoneinfo/"
        ; table = Hashtbl.create (module String)
        })
    ;;

    let with_the_one_and_only f =
      (Mutex.with_lock mutex ~f:(fun password ->
         Capsule.Expert.access ~password ~f:(fun access ->
           { contended = { aliased = f (Capsule.Data.unwrap ~access capsule) } })
         [@nontail]))
        .contended
        .aliased
    ;;

    let clear () = with_the_one_and_only (fun t -> clear t)
    let find zone = with_the_one_and_only (fun t -> find t zone)
    let find_or_load zonename = with_the_one_and_only (fun t -> find_or_load t zonename)

    let init_from_file_system () =
      with_the_one_and_only (fun t -> init_from_file_system t)
    ;;

    let initialized_zones () = with_the_one_and_only (fun t -> initialized_zones t)

    let find_or_load_matching zone =
      with_the_one_and_only (fun t -> find_or_load_matching t zone)
    ;;

    let init =
      (* On web we are relying on Timezone_js_loader and don't have access to zoneinfo files.
         This is especially problematic because in test environments on node we do have
         access to the files so the test/app behavior would be different if we allowed this.

         wasm_of_ocaml doesn't support file access at all (2024-05), and so the
         initialization always fails. *)
      match
        Timezone_js_loader.For_advanced_timezone_feature_detection
        .should_use_timezone_js_loader
          ()
      with
      | `Yes -> ignore
      | `Platform_not_supported | `Disabled -> init_from_file_system
    ;;
  end

  include The_one_and_only
end

let init = Zone_cache.init
let initialized_zones = Zone_cache.initialized_zones

let find zone =
  let zone =
    (* Some aliases for convenience *)
    match zone with
    (* case insensitivity *)
    | "utc" -> "UTC"
    | "gmt" -> "GMT"
    (* some aliases for common zones *)
    | "chi" -> "America/Chicago"
    | "nyc" -> "America/New_York"
    | "hkg" -> "Asia/Hong_Kong"
    | "lon" | "ldn" -> "Europe/London"
    | "tyo" -> "Asia/Tokyo"
    | "syd" -> "Australia/Sydney"
    (* catchall *)
    | _ -> zone
  in
  Zone_cache.find_or_load zone
;;

let find_exn zone =
  match find zone with
  | None -> Error.raise_s [%message "unknown zone" (zone : string)]
  | Some z -> z
;;

let local_portable =
  (* Load [TZ] immediately so that subsequent modifications to the environment cannot
     alter the result of [force local]. *)
  let local_zone_name = Sys.getenv "TZ" in
  let load () =
    match local_zone_name with
    | Some zone_name -> find_exn zone_name
    | None ->
      let localtime_t =
        input_tz_file ~zonename:"/etc/localtime" ~filename:"/etc/localtime"
      in
      (* Load the matching zone file from the real zone cache so that we can serialize it
         properly. The file loaded from /etc/localtime won't have a name we can use on the
         other side to find the right zone. *)
      (match Zone_cache.find_or_load_matching localtime_t with
       | Some t -> t
       | None -> localtime_t)
  in
  Portable_lazy.from_fun load
;;

(* See cr-someday in interface file. *)
let local = lazy (Portable_lazy.force local_portable)

module Stable = struct
  include Zone.Stable

  module V1 = struct
    type nonrec t = t

    let of_string name =
      match name with
      | "Local" -> Portable_lazy.force local_portable
      | name ->
        if String.equal name "UTC" || String.equal name "GMT"
        then of_utc_offset_explicit_name ~name ~hours:0
        else if (* This special handling is needed because the offset directionality of
                   the zone files in /usr/share/zoneinfo for GMT<offset> files is the
                   reverse of what is generally expected. That is, GMT+5 is what most
                   people would call GMT-5. *)
                String.is_prefix name ~prefix:"GMT-"
                || String.is_prefix name ~prefix:"GMT+"
                || String.is_prefix name ~prefix:"UTC-"
                || String.is_prefix name ~prefix:"UTC+"
        then (
          let offset =
            let base =
              Int.of_string (String.sub name ~pos:4 ~len:(String.length name - 4))
            in
            match name.[3] with
            | '-' -> -1 * base
            | '+' -> base
            | _ -> assert false
          in
          of_utc_offset_explicit_name ~name ~hours:offset)
        else find_exn name
    ;;

    let t_of_sexp sexp =
      match sexp with
      | Sexp.Atom name ->
        (try of_string name with
         | exc ->
           of_sexp_error (sprintf "Timezone.t_of_sexp: %s" (Exn.to_string exc)) sexp)
      | _ -> of_sexp_error "Timezone.t_of_sexp: expected atom" sexp
    ;;

    let%template check_name name =
      if (String.equal [@mode local]) name "/etc/localtime"
      then failwith "the local time zone cannot be serialized"
    ;;

    let%template[@alloc a @ m = (heap_global, stack_local)] to_string t =
      (let name = (name [@mode m]) t in
       check_name name;
       name)
      [@exclave_if_stack a]
    ;;

    let sexp_of_t t = Sexp.Atom (to_string t)

    let t_sexp_grammar : t Sexplib.Sexp_grammar.t =
      { untyped =
          Tagged
            { key = Sexplib.Sexp_grammar.type_name_tag
            ; value = Atom "Timezone.t"
            ; grammar = String
            }
      }
    ;;

    (* The correctness of these relies on not exposing raw loading/creation functions to
       the outside world that would allow the construction of two Zone's with the same
       name and different transitions. *)
    [%%template
    let[@mode local] to_string = (to_string [@alloc stack])

    [@@@mode.default m = (local, global)]

    let compare t1 t2 =
      (String.compare [@mode m])
        ((to_string [@mode m]) t1)
        ((to_string [@mode m]) t2) [@nontail]
    ;;

    let equal t1 t2 =
      (String.equal [@mode m])
        ((to_string [@mode m]) t1)
        ((to_string [@mode m]) t2) [@nontail]
    ;;]

    let hash_fold_t state t = String.hash_fold_t state (to_string t)
    let hash t = Ppx_hash_lib.Std.Hash.of_fold hash_fold_t t
    let%template to_binable = (to_string [@mode m]) [@@mode m = (local, global)]
    let of_binable s = t_of_sexp (Sexp.Atom s)

    include%template (
      Binable.Stable.Of_binable.V1 [@mode local] [@modality portable] [@alert "-legacy"]
        (Stable_string.V1)
        (struct
          type nonrec t = t

          let to_binable = to_binable
          let%template[@mode local] to_binable = (to_binable [@mode local])
          let of_binable = of_binable
        end) :
        sig
        @@ portable
          include Binable.S [@mode local] with type t := t
        end)

    let stable_witness =
      Stable_witness.of_serializable Stable_string.V1.stable_witness of_binable to_binable
    ;;

    include%template Diffable.Atomic.Make [@modality portable] (struct
        type nonrec t = t [@@deriving sexp, bin_io, equal ~localize]
      end)
  end

  module Current = V1
end

include%template Identifiable.Make [@mode local] [@modality portable] (struct
    let module_name = "Core.Timezone"

    include Stable.Current

    let of_string = of_string
    let to_string = to_string
  end)

include Stable.Current

module Private = struct
  module Zone_cache = Zone_cache
end
