open Core_kernel.Std

type 'a t = int

module type S   = Syscall_result_intf.S with type 'a syscall_result := 'a t
module type Arg = Syscall_result_intf.Arg

module Unix = UnixLabels

external unix_error_of_code : int -> Unix.error = "core_unix_error_of_code"
external code_of_unix_error : Unix.error -> int = "core_code_of_unix_error"

TEST_UNIT =
  for i = 1 to 10000 do
    <:test_result< int >> ~expect:i (code_of_unix_error (unix_error_of_code i))
  done;
;;

let create_error err = - (code_of_unix_error err)

module Make (M : Arg) () = struct
  type nonrec t = M.t t

  let compare = Int.compare

  let create_ok x =
    let t = M.to_int x in
    if t < 0 then
      failwithf "Syscall_result.create_ok received negative value (%d)" t ()
    else
      t
  ;;

  let create_error = create_error

  let is_ok    t = t >= 0
  let is_error t = t < 0

  let to_result t =
    if is_ok t then
      Ok (M.of_int_exn t)
    else
      Error (unix_error_of_code (-t))
  ;;

  let sexp_of_t t = <:sexp_of< (M.t, Unix_error.t) Result.t >> (to_result t)

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
  let error_exn t = unix_error_of_code (error_code_exn t)

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
      raise (Unix.Unix_error (unix_error_of_code (-t), syscall_name, ""))
  ;;

  let ok_or_unix_error_with_args_exn t ~syscall_name x sexp_of_x =
    if is_ok t then
      M.of_int_exn t
    else
      raise (Unix.Unix_error (unix_error_of_code (-t), syscall_name,
                              Sexp.to_string (sexp_of_x x)))
  ;;
end

module Int = Make (Int) ()
module Unit = Make (struct
  type t = unit with sexp_of
  let of_int_exn n = assert (n = 0)
  let to_int () = 0
end) ()

let unit = Unit.create_ok ()
let ignore_ok_value t = Core_kernel.Std.Int.min t 0

TEST_UNIT =
  for i = 1 to 10000 do
    let err = unix_error_of_code i in
    assert (err = Unit.error_exn (Unit.create_error err))
  done;
;;
