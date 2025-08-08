open! Import
open Unique_id_intf

module type Id = Id

module Int () = struct
  include Int

  let current = Atomic.make zero
  let create () = Atomic.fetch_and_add current 1

  module For_testing = struct
    let reset_counter () = Atomic.set current zero
  end
end

module Int63 () = struct
  include Int63

  let current = Atomic.make zero
  let create () = Atomic.update_and_return current ~pure_f:[%eta1 succ]

  module For_testing = struct
    let reset_counter () = Atomic.set current zero
  end
end
