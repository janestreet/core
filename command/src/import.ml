include struct
  open Stdio

  let eprintf = eprintf
  let printf = printf
  let print_s = print_s
  let print_string = print_string
  let print_endline = print_endline
  let prerr_endline = prerr_endline

  module In_channel = In_channel
end

include struct
  open Base.Printf

  let sprintf = sprintf
  let failwithf = failwithf
  let ksprintf = ksprintf
end
