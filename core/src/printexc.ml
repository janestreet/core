open! Import

let to_string _ = `Deprecated_use_Exn_to_string_instead
let print _ = `Deprecated_use_Exn_to_string_instead
let catch _ _ = `Deprecated_use_Exn_handle_uncaught_instead
let print_backtrace = Caml.Printexc.print_backtrace
let get_backtrace = Caml.Printexc.get_backtrace
let record_backtrace = Caml.Printexc.record_backtrace
let backtrace_status = Caml.Printexc.backtrace_status
