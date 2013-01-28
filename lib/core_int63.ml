INCLUDE "config.mlh"
IFDEF ARCH_SIXTYFOUR THEN
include Core_int
let to_int x = Some x
ELSE
include Core_int64
ENDIF

let () = assert (Core_int.(>=) num_bits 63);
