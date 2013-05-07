module List = Core_list
module String = StringLabels
open Fieldslib
open Result.Export
open Sexplib.Conv

(** Each single_error is a path indicating the location within the datastructure in
    question that is being validated, along with an error message. *)
type single_error =
  { path : string list;
    error : Error.t;
  }

type t = single_error list

type 'a check = 'a -> t

let pass : t = []

let fails message a sexp_of_a =
  [ { path = [];
      error = Error.create message a sexp_of_a;
    } ]
;;

let fail message = [ { path = []; error = Error.of_string message } ]

let of_list = List.concat

let name name t =
  List.map t ~f:(fun { path; error } -> { path = name :: path; error })
;;

let name_list n l = name n (of_list l)

let fail_fn message _ = fail message

let pass_bool (_:bool) = pass
let pass_unit (_:unit) = pass

let protect f v =
  try
    f v
  with exn ->
    fails "Exception raised during validation" exn <:sexp_of< exn >>
;;

let path_string path = String.concat ~sep:"." path

let errors t =
  List.map t ~f:(fun { path; error } ->
    (String.concat ~sep:"" [ path_string path; ": "; Error.to_string_hum error ]))
;;

let result t =
  if List.is_empty t then
     Ok ()
  else
    Or_error.error "validation errors"
      (List.map t ~f:(fun { path; error } -> (path_string path, error)))
      <:sexp_of< (string * Error.t) list >>
;;

let maybe_raise t = Or_error.ok_exn (result t)

let valid_or_error x check =
  Or_error.map (result (protect check x)) ~f:(fun () -> x)
;;

let field record fld f =
  let v = Field.get fld record in
  let result = protect f v in
  name (Field.name fld) result
;;

let field_folder record check = (); fun acc fld -> field record fld check :: acc

let all checks v = of_list (List.map checks ~f:(fun check -> protect check v))

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

let pair ~fst ~snd (fst_value,snd_value) =
  of_list [ name "fst" (protect fst fst_value);
            name "snd" (protect snd snd_value);
          ]
;;

let list_indexed check list =
  List.mapi list ~f:(fun i el ->
    name (string_of_int (i+1)) (protect check el))
  |! of_list
;;

let list ~name:extract_name check list =
  List.map list ~f:(fun el ->
    match protect check el with
    | [] -> []
    | t ->
      (* extra level of protection in case extract_name throws an exception *)
      protect (fun t -> name (extract_name el) t) t)
  |! of_list
;;

let alist ~name f list' =
  list (fun (_, x) -> f x) list'
    ~name:(fun (key, _) -> name key)
;;

let first_failure t1 t2 = if List.is_empty t1 then t2 else t1

let of_error_opt = function
  | None -> pass
  | Some error -> fail error
;;

TEST_MODULE = struct
  TEST = first_failure pass (fail "foo") = fail "foo"
  TEST = first_failure (fail "foo") (fail "bar") = fail "foo"
  let two_errors = of_list [fail "foo"; fail "bar"]
  TEST = first_failure two_errors (fail "snoo") = two_errors
  TEST = first_failure (fail "snoo") two_errors = fail "snoo"
end
