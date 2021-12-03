include Date0

let of_time time ~zone = Time_float.to_date ~zone time
let today ~zone = of_time (Time_float.now ()) ~zone
let format = `Use_Date_unix
let of_tm = `Use_Date_unix
let parse = `Use_Date_unix
