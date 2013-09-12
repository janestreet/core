module Zone = Zone
module Span = Span

module Ofday = struct
  include (Ofday : (module type of Ofday
                     with type t = Ofday.t
                     with module Zoned := Ofday.Zoned))

  module Zoned = struct
    include Ofday.Zoned

    let to_time t date = Time0.of_date_ofday (zone t) date (ofday t)
  end

  (* can't be defined in Ofday directly because it would create a circular reference *)
  let now () = snd (Time0.to_local_date_ofday (Time0.now ()))
end

include Time0
