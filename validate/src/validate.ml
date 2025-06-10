open Base

[@@@warning "-incompatible-with-upstream"]

(** Each single_error is a path indicating the location within the datastructure in
    question that is being validated, along with an error message. *)
type single_error =
  { path : string list
  ; error : Error.t
  }

type t = single_error list
type ('a : any) check = 'a -> t

let[@inline] get_pass () : t = []
let pass = get_pass ()

let fails message a sexp_of_a =
  [ { path = []; error = Error.create message a sexp_of_a } ]
;;

let fail message = [ { path = []; error = Error.of_string message } ]
let failf format = Printf.ksprintf fail format
let fail_s sexp = [ { path = []; error = Error.create_s sexp } ]
let combine t1 t2 = t1 @ t2
let of_list = List.concat

let lazy_name name t =
  match t with
  | [] -> [] (* when successful, do not force the name *)
  | _ ->
    List.map t ~f:(stack_ fun { path; error } ->
      { path = Lazy.force name :: path; error })
    [@nontail]
;;

let[@inline] name name t = lazy_name (Lazy.from_val name) t
let name_list n l = name n (of_list l)
let lazy_name_list n l = lazy_name n (of_list l)
let fail_fn message _ = fail message
let pass_bool (_ : bool) = get_pass ()
let pass_unit (_ : unit) = get_pass ()

[%%template
[@@@kind.default
  k = (value_or_null, float64, bits32, bits64, word, immediate, immediate64)]

let protect (type a : k) (f : a check) (v : a) =
  try f v with
  | exn ->
    fail_s (Sexp.message "Exception raised during validation" [ "", sexp_of_exn exn ])
;;

let field_direct (type a : k) (check : a check) fld _record v =
  let result = (protect [@kind k]) check v in
  name (Field.name fld) result
;;

let field (type a : k) (check : a check) record fld =
  let v = (Field.get [@kind k]) fld record in
  (field_direct [@kind k]) check fld record v
;;]

let try_with f =
  protect
    (fun () ->
      f ();
      get_pass ())
    ()
;;

let path_string path = String.concat ~sep:"." path

let errors t =
  List.map t ~f:(fun { path; error } ->
    Error.to_string_hum (Error.tag error ~tag:(path_string path)))
;;

let result_fail t =
  Or_error.error
    "validation errors"
    (List.map t ~f:(fun { path; error } -> path_string path, error))
    [%sexp_of: (string * Error.t) List.t]
[@@cold]
;;

(** [result] is carefully implemented so that it can be inlined -- calling [result_fail],
    which is not inlineable, is key to this. *)
let result t = if List.is_empty t then Ok () else result_fail t

let maybe_raise t =
  (* [@zero_alloc] assume here because this function always raises  *)
  let[@zero_alloc assume] fail () = Or_error.ok_exn (result_fail t) in
  if List.is_empty t then () else fail ()
;;

let valid_or_error check x = Or_error.map (result (protect check x)) ~f:(fun () -> x)

let field_folder check record =
  ();
  fun acc fld -> field check record fld :: acc
;;

let field_direct_folder check =
  Staged.stage (fun acc fld record v ->
    match field_direct check fld record v with
    | [] -> acc (* Avoid allocating a new list in the success case *)
    | result -> result :: acc)
;;

let all checks v =
  let rec loop checks v errs =
    match checks with
    | [] -> errs
    | check :: checks ->
      (match protect check v with
       | [] -> loop checks v errs
       | err -> loop checks v (err :: errs))
  in
  of_list (List.rev (loop checks v []))
;;

let of_result f =
  protect (fun v ->
    match f v with
    | Ok () -> get_pass ()
    | Error error -> fail error)
;;

let of_error f =
  protect (fun v ->
    match f v with
    | Ok () -> get_pass ()
    | Error error -> [ { path = []; error } ])
;;

let lazy_booltest f ~if_false =
  protect (fun v -> if f v then get_pass () else fail (Lazy.force if_false))
;;

let[@inline] booltest f ~if_false = lazy_booltest f ~if_false:(Lazy.from_val if_false)

let pair ~fst ~snd (fst_value, snd_value) =
  of_list [ name "fst" (protect fst fst_value); name "snd" (protect snd snd_value) ]
;;

let list_indexed check list =
  List.concat_mapi list ~f:(fun i el ->
    match protect check el with
    | [] -> [] (* when successful, avoid the allocation of a string below *)
    | t -> name (Int.to_string (i + 1)) t)
;;

let list ~name:extract_name check list =
  List.concat_map list ~f:(fun el ->
    match protect check el with
    | [] -> []
    | t ->
      (* extra level of protection in case extract_name throws an exception *)
      protect (fun t -> name (extract_name el) t) t)
;;

let alist ~name f list' = list (fun (_, x) -> f x) list' ~name:(fun (key, _) -> name key)
let first_failure t1 t2 = if List.is_empty t1 then t2 else t1

let of_error_opt = function
  | None -> get_pass ()
  | Some error -> fail error
;;

let bounded ~name ~lower ~upper ~compare x =
  match Maybe_bound.compare_to_interval_exn ~lower ~upper ~compare x with
  | In_range -> get_pass ()
  | Below_lower_bound ->
    (match lower with
     | Unbounded -> assert false
     | Incl incl -> fail (Printf.sprintf "value %s < bound %s" (name x) (name incl))
     | Excl excl -> fail (Printf.sprintf "value %s <= bound %s" (name x) (name excl)))
  | Above_upper_bound ->
    (match upper with
     | Unbounded -> assert false
     | Incl incl -> fail (Printf.sprintf "value %s > bound %s" (name x) (name incl))
     | Excl excl -> fail (Printf.sprintf "value %s >= bound %s" (name x) (name excl)))
;;

module Infix = struct
  let ( ++ ) t1 t2 = combine t1 t2
end
