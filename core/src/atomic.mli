(** @inline *)
include module type of struct
  include Portable.Atomic
end

[%%rederive: type nonrec 'a t = 'a t [@@deriving bin_shape, stable_witness]]
[%%rederive: type nonrec 'a t = 'a t [@@deriving bin_write]]
[%%rederive: type nonrec 'a t = 'a t [@@deriving bin_read]]
[%%rederive: type nonrec 'a t = 'a t [@@deriving bin_type_class]]
