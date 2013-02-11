open Std_internal

module Id = Type_equal.Id

module Constr = struct

  type 'a t =
    { id : 'a Id.t;
      to_sexp : 'a -> Sexp.t;
    }
  with fields, sexp_of

  let create name to_sexp = { id = Id.create ~name; to_sexp }

  let hash t = Id.hash t.id
  let name t = Id.name t.id
end

type t = T : 'a Constr.t * 'a -> t

let create constr value = T (constr, value)

let constr_name (T (constr, _)) = Constr.name constr

let sexp_of_t (T (constr, value)) = Constr.to_sexp constr value

let does_match (T (constr1, _)) constr2 = Id.same (Constr.id constr1) (Constr.id constr2)

let match_ (type a) (T (constr1, value)) (constr2 : a Constr.t) =
  let id1 = Constr.id constr1 in
  let id2 = Constr.id constr2 in
  if not (Id.same id1 id2) then
    None
  else
    let Type_equal.T = Id.same_witness_exn id1 id2 in
    Some (value : a)
;;

let match_exn (type a) (T (constr1, value) as t) (constr2 : a Constr.t) =
  let id1 = Constr.id constr1 in
  let id2 = Constr.id constr2 in
  if not (Id.same id1 id2) then
    failwiths "Univ.match_exn called with mismatched value and constructor" (t, constr2)
      (<:sexp_of< t * _ Constr.t >>)
  else
    let Type_equal.T = Id.same_witness_exn id1 id2 in
    (value : a)
;;

TEST_MODULE = struct

  let c1 = Constr.create "c1" Int.sexp_of_t
  let c2 = Constr.create "c2" Int.sexp_of_t
  let t1 = create c1 13
  let t2 = create c2 13

  TEST_UNIT = ignore (<:sexp_of< _ Constr.t >> c1 : Sexp.t)
  TEST_UNIT = ignore (<:sexp_of< t >> t1 : Sexp.t)

  TEST = constr_name t1 = Constr.name c1

  TEST = does_match t1 c1
  TEST = not (does_match t1 c2)
  TEST = not (does_match t2 c1)
  TEST = does_match t2 c2

  TEST =
    match match_ t1 c1 with
    | None -> false
    | Some v -> v = 13
  ;;

  TEST = Option.is_none (match_ t1 c2)

  TEST = match_exn t1 c1 = 13
  TEST = Result.is_error (Result.try_with (fun () -> match_exn t1 c2))

end
