open! Core
open! Import

module%test [@name "Result.V1"] _ =
  Stable_unit_test.Make (Result.Stable.V1_stable_unit_test)

module%test [@name "can contain nulls"] _ = struct
  (* no annotation *)
  let _ = Ok Null
  let _ = Error Null

  (* In Core, but not Base, the [Result.Export] module is also included at toplevel. This
     is the inferred type if you use the constructor without annotations. *)
  let _ : _ _result = Ok Null
  let _ : _ _result = Error Null

  (* [result] is via [Core_pervasives], not a re-export of [Base.result]. *)
  let _ : _ result = Ok Null
  let _ : _ result = Error Null
  let _ : _ Result.t = Ok Null
  let _ : _ Result.t = Error Null
  let _ : _ Result.Export._result = Ok Null
  let _ : _ Result.Export._result = Error Null

  (* check that [open]ing [Result] doesn't ruin the visible constructors *)
  open! Result

  let _ = Ok Null
  let _ = Error Null
  let _ : _ t = Ok Null
  let _ : _ t = Error Null
end
