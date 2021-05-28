open! Core
module Thread = Core_thread

let ( @? ) name bool = if not bool then raise_s [%message (name : string)]
