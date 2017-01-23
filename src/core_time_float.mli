open! Import
open Core_kernel.Core_kernel_private

include Core_time_intf.S
  with module Time0 := Time_float0
   and module Time := Time
