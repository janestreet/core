(* Open this in modules where you don't want to accidentally use polymorphic comparison.
   Then, use Pervasives.(<), for example, where needed. *)

type compare =
  [`no_polymorphic_compare]
  -> [`no_polymorphic_compare]
  -> [`no_polymorphic_compare]

val compare : compare
val (<)     : compare
val (<=)    : compare
val (>)     : compare
val (>=)    : compare
val (=)     : compare
val (<>)    : compare
val equal   : compare
val min     : compare
val max     : compare

(* [_squelch_unused_module_warning_] helps avoid spurious unused-[open] warnings in
   OCaml 4.0.  If one does:

   |  open No_polymorphic_compare

   then the whole point is that the values imported will *not* be used.  But then OCaml
   will complain about [No_polymorphic_compare] not being used.  So, we write:

   |  open No_polymorphic_compare  let _ = _squelch_unused_module_warning_

   which silences the warning, and is hopefully self-documenting. *)
val _squelch_unused_module_warning_ : unit
