open! Import
open Std_internal

let randomize span ~percent ~scale =
  let mult = Percent.to_mult percent in
  if Float.( < ) mult 0. || Float.( > ) mult 1.
  then
    raise_s
      [%message "Span.randomize: percent is out of range [0x, 1x]" (percent : Percent.t)];
  let factor = Random.float_range (1. -. mult) (Float.one_ulp `Up (1. +. mult)) in
  scale span factor
;;

let format_decimal n tenths units =
  assert (tenths >= 0 && tenths < 10);
  if n < 10 && tenths <> 0
  then sprintf "%d.%d%s" n tenths units
  else sprintf "%d%s" n units
;;

let short_string ~sign ~hr ~min ~sec ~ms ~us ~ns =
  let s =
    if hr >= 24
    then format_decimal (hr / 24) (Int.of_float (Float.of_int (hr % 24) /. 2.4)) "d"
    else if hr > 0
    then format_decimal hr (min / 6) "h"
    else if min > 0
    then format_decimal min (sec / 6) "m"
    else if sec > 0
    then format_decimal sec (ms / 100) "s"
    else if ms > 0
    then format_decimal ms (us / 100) "ms"
    else if us > 0
    then format_decimal us (ns / 100) "us"
    else sprintf "%ins" ns
  in
  match (sign : Sign.t) with
  | Neg -> "-" ^ s
  | Zero | Pos -> s
;;
