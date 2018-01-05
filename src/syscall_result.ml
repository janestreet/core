open! Import

type 'a t = int

module type S   = Syscall_result_intf.S with type 'a syscall_result := 'a t
module type Arg = Syscall_result_intf.Arg

module Unix = UnixLabels

let create_error err = - (Unix_error.to_errno err)

let is_ok    t = Int.O.(t >= 0)
let is_error t = Int.O.(t <  0)

let error_code_exn t =
  if is_ok t
  then failwiths "Syscall_result.error_code_exn received success value"
         t [%sexp_of : int]
  else -t
;;

let error_exn t = Unix_error.of_errno (error_code_exn t)

module Make (M : Arg) () = struct
  (* The only reason to have one of these per functor invocation is to make it trivial to
     get the type right. *)
  let preallocated_errnos : (_, Unix_error.t) Result.t array =
    Array.init 64 ~f:(fun i -> Error (Unix_error.of_errno i))
  ;;
  (* Since we return [-errno] from C, we implicitly rely on there not being a 0 [errno].
     However, we have 0 in [preallocated_errnos], partly so we can index directly by
     [errno]. *)
  let%test "no 0 errno" = preallocated_errnos.(0) = Error (Unix_error.EUNKNOWNERR 0)
  let num_preallocated_errnos = Array.length preallocated_errnos

  type nonrec t = M.t t

  let compare = Int.compare
  let equal   = Int.equal

  let preallocated_ms =
    let rec loop i rev_acc =
      (* Preallocate at most a handful of Ms.  2048 is the first round binary number after
         1500, the likely maximum result for many network functions that use
         [Syscall_result.Int]. *)
      if i = 2048 then
        Array.of_list_rev rev_acc
      else
        match M.of_int_exn i with
        | exception _ -> Array.of_list_rev rev_acc
        | m -> loop (i + 1) (Ok m :: rev_acc)
    in
    loop 0 []
  ;;
  let num_preallocated_ms = Array.length preallocated_ms

  let create_ok x =
    let t = M.to_int x in
    if t < 0 then
      failwithf "Syscall_result.create_ok received negative value (%d)" t ()
    else
      t
  ;;

  let create_error = create_error

  let is_ok    = is_ok
  let is_error = is_error

  let to_result t =
    if is_ok t then
      if t < num_preallocated_ms
      then Array.unsafe_get preallocated_ms t
      else Ok (M.of_int_exn t)
    else
      (let errno = -t in
       if errno < num_preallocated_errnos
       then Array.unsafe_get preallocated_errnos errno
       else Error (Unix_error.of_errno errno))
  ;;
  (* We can't use [Core_bench] to do this inside [Core], which it depends on. *)
  let%test_unit "to_result doesn't allocate" =
    for _ = 1 to 1000 do
      for t = -(Array.length preallocated_errnos - 1) to Array.length preallocated_ms - 1
      do
        let before_minor = Gc.minor_words () in
        let before_major = Gc.major_words () in
        let result = to_result t in
        let after_minor  = Gc.minor_words () in
        let after_major  = Gc.major_words () in
        [%test_result: int] ~expect:0 (after_minor - before_minor);
        [%test_result: int] ~expect:0 (after_major - before_major);
        [%test_result: (M.t, Unix_error.t) Result.t] result
          ~expect:(if is_ok t
                   then Ok (M.of_int_exn t)
                   else Error (Unix_error.of_errno (-t)));
      done;
    done
  ;;

  let sexp_of_t t = [%sexp_of: (M.t, Unix_error.t) Result.t] (to_result t)

  let ok_exn t =
    if is_ok t then
      M.of_int_exn t
    else
      failwiths "Syscall_result.ok_exn received error value" t sexp_of_t
  ;;

  let error_code_exn t =
    if is_ok t then
      failwiths "Syscall_result.error_code_exn received success value"
        t sexp_of_t
    else
      -t
  ;;
  let error_exn t = Unix_error.of_errno (error_code_exn t)

  let reinterpret_error_exn t =
    if is_ok t then
      failwiths "Syscall_result.cast_error_exn received success value"
        t sexp_of_t
    else
      t
  ;;

  let ok_or_unix_error_exn t ~syscall_name =
    if is_ok t then
      M.of_int_exn t
    else
      raise (Unix.Unix_error (Unix_error.of_errno (-t), syscall_name, ""))
  ;;

  let ok_or_unix_error_with_args_exn t ~syscall_name x sexp_of_x =
    if is_ok t then
      M.of_int_exn t
    else
      raise (Unix.Unix_error (Unix_error.of_errno (-t), syscall_name,
                              Sexp.to_string (sexp_of_x x)))
  ;;

  let is_none t = is_error t
  let unchecked_value t = M.of_int_exn t

  module Optional_syntax = struct
    module Optional_syntax = struct
      let is_none = is_none
      let unsafe_value = unchecked_value
    end
  end

end

module Int = Make (Int) ()
module Unit = Make (struct
    type t = unit [@@deriving sexp_of, compare]
    let of_int_exn n = assert (n = 0)
    let to_int () = 0
  end) ()

let unit = Unit.create_ok ()
let ignore_ok_value t = Core_kernel.Int.min t 0

let%test_unit _ =
  for i = 1 to 10000 do
    let err = Unix_error.of_errno i in
    assert (err = Unit.error_exn (Unit.create_error err))
  done
;;
