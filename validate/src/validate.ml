open Base

(** Each single_error is a path indicating the location within the datastructure in
    question that is being validated, along with an error message. *)
type single_error =
  { path : string list
  ; error : Error.t
  }

type t = single_error list
type 'a check = 'a -> t

let pass : t = []

let fails message a sexp_of_a =
  [ { path = []; error = Error.create message a sexp_of_a } ]
;;

let fail message = [ { path = []; error = Error.of_string message } ]
let failf format = Printf.ksprintf fail format
let fail_s sexp = [ { path = []; error = Error.create_s sexp } ]
let combine t1 t2 = t1 @ t2
let of_list = List.concat

let name name t =
  match t with
  | [] -> [] (* when successful, avoid the allocation of a closure for [~f], below *)
  | _ -> List.map t ~f:(fun { path; error } -> { path = name :: path; error })
;;

let name_list n l = name n (of_list l)
let fail_fn message _ = fail message
let pass_bool (_ : bool) = pass
let pass_unit (_ : unit) = pass

let protect f v =
  try f v with
  | exn ->
    fail_s (Sexp.message "Exception raised during validation" [ "", sexp_of_exn exn ])
;;

let try_with f =
  protect
    (fun () ->
      f ();
      pass)
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

let maybe_raise t = Or_error.ok_exn (result t)
let valid_or_error check x = Or_error.map (result (protect check x)) ~f:(fun () -> x)

let field_direct check fld _record v =
  let result = protect check v in
  name (Field.name fld) result
;;

let field check record fld =
  let v = Field.get fld record in
  field_direct check fld record v
;;

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
    | Ok () -> pass
    | Error error -> fail error)
;;

let of_error f =
  protect (fun v ->
    match f v with
    | Ok () -> pass
    | Error error -> [ { path = []; error } ])
;;

let booltest f ~if_false = protect (fun v -> if f v then pass else fail if_false)

let pair ~fst ~snd (fst_value, snd_value) =
  of_list [ name "fst" (protect fst fst_value); name "snd" (protect snd snd_value) ]
;;

let list_indexed check list =
  List.mapi list ~f:(fun i el -> name (Int.to_string (i + 1)) (protect check el))
  |> of_list
;;

let list ~name:extract_name check list =
  List.map list ~f:(fun el ->
    match protect check el with
    | [] -> []
    | t ->
      (* extra level of protection in case extract_name throws an exception *)
      protect (fun t -> name (extract_name el) t) t)
  |> of_list
;;

let alist ~name f list' = list (fun (_, x) -> f x) list' ~name:(fun (key, _) -> name key)
let first_failure t1 t2 = if List.is_empty t1 then t2 else t1

let of_error_opt = function
  | None -> pass
  | Some error -> fail error
;;

let bounded ~name ~lower ~upper ~compare x =
  match Maybe_bound.compare_to_interval_exn ~lower ~upper ~compare x with
  | In_range -> pass
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
