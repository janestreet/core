include Date0

let of_time time ~zone = Time.to_date ~zone time

let today ~zone = of_time (Time.now ()) ~zone

let format date pat =
  let time = Time.of_date_ofday ~zone:Zone.local date Ofday.start_of_day in
  Time.format time pat
;;
