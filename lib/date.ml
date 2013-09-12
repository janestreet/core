include Date0

let of_time time = Time.to_local_date time

let today () = of_time (Time.now ())

let format date pat =
  let time = Time.of_local_date_ofday date Ofday.start_of_day in
  Time.format time pat
;;
