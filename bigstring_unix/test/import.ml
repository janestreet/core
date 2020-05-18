open! Core

let ( @? ) name bool = if not bool then raise_s [%message (name : string)]
