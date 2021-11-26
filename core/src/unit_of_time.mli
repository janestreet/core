(** Represents a unit of time, e.g., that used by [Time.Span.to_string_hum]. Comparison
    respects Nanosecond < Microsecond < Millisecond < Second < Minute < Hour < Day. *)

open! Import

type t =
  | Nanosecond
  | Microsecond
  | Millisecond
  | Second
  | Minute
  | Hour
  | Day
[@@deriving sexp, compare, enumerate, hash]
