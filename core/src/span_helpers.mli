@@ portable

open! Import

val randomize
  :  'span
  -> Random.State.t
  -> percent:Percent.t
  -> scale:('span -> float -> 'span)
  -> 'span

val short_string
  :  sign:Sign.t
  -> hr:int
  -> min:int
  -> sec:int
  -> ms:int
  -> us:int
  -> ns:int
  -> string
