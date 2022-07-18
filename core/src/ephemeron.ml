open! Import
open! Std_internal
module Ephemeron = Caml.Ephemeron.K1

type ('a, 'b) t = ('a Heap_block.t, 'b Heap_block.t) Ephemeron.t
