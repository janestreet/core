open! Import

type compare =
  [ `no_polymorphic_compare ]
  -> [ `no_polymorphic_compare ]
  -> [ `no_polymorphic_compare ]

let compare _ _ = `no_polymorphic_compare
let ( < ) _ _ = `no_polymorphic_compare
let ( <= ) _ _ = `no_polymorphic_compare
let ( > ) _ _ = `no_polymorphic_compare
let ( >= ) _ _ = `no_polymorphic_compare
let ( = ) _ _ = `no_polymorphic_compare
let ( <> ) _ _ = `no_polymorphic_compare
let equal _ _ = `no_polymorphic_compare
let min _ _ = `no_polymorphic_compare
let max _ _ = `no_polymorphic_compare
