open Sexplib

type t = {
  exn : exn;                    (* where we stash the raw value *)
  constr_name : string;
  to_sexp : unit -> Sexp.t;
}

module Constr = struct
  type 'a t = {
    emb : 'a -> exn;
    prj : exn  -> 'a option;
    to_sexp : 'a -> Sexp.t;
    name : string;
  }

  let create (type a) name to_sexp =
    let module M = struct
      exception Z of a
      let emb x = Z x
      let prj = function Z x -> Some x | _ -> None
      let z = { emb; prj; name; to_sexp }
    end
    in
    M.z
end

let constr_name t = t.constr_name
let sexp_of_t t = t.to_sexp ()

let create constr x = {
  exn         = constr.Constr.emb x;
  constr_name = constr.Constr.name;
  to_sexp     = (fun () -> constr.Constr.to_sexp x);
}

let match_ t constr = constr.Constr.prj t.exn


