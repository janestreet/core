(** Provides generic signatures for container data structures.

    These signatures include functions ([iter], [fold], [exists], [for_all], ...) that you
    would expect to find in any container. Used by including [Container.S0] or
    [Container.S1] in the signature for every container-like data structure ([Array],
    [List], [String], ...) to ensure a consistent interface.

    These signatures extend signatures exported by {!Base.Container_intf}.
*)

include Container_intf.Container (** @inline *)
