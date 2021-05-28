include Core_kernel.Signal

type pid_spec = [ `Use_Signal_unix ]
type sigprocmask_command = [ `Use_Signal_unix ]

let can_send_to = `Use_Signal_unix
let of_system_int = `Use_Signal_unix
let send = `Use_Signal_unix
let send_exn = `Use_Signal_unix
let send_i = `Use_Signal_unix
let sexp_of_pid_spec = `Use_Signal_unix
let sigpending = `Use_Signal_unix
let sigprocmask = `Use_Signal_unix
let sigsuspend = `Use_Signal_unix
let to_system_int = `Use_Signal_unix
