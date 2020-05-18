open! Core

val fdpair_test
  :  n:string
  -> (unit -> Unix.File_descr.t * Unix.File_descr.t)
  -> ('a -> Unix.File_descr.t -> unit)
  -> (n:string -> 'a -> Unix.File_descr.t -> unit)
  -> 'a
  -> unit

val socketpair : unit -> Unix.File_descr.t * Unix.File_descr.t
