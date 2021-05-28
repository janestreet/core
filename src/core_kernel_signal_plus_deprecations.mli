include module type of struct
  include Core_kernel.Signal
end

type pid_spec = [ `Use_Signal_unix ] [@@deprecated "[since 2021-04] Use [Signal_unix]"]

type sigprocmask_command = [ `Use_Signal_unix ]
[@@deprecated "[since 2021-04] Use [Signal_unix]"]

val can_send_to : [ `Use_Signal_unix ] [@@deprecated "[since 2021-04] Use [Signal_unix]"]

val of_system_int : [ `Use_Signal_unix ]
[@@deprecated "[since 2021-04] Use [Signal_unix]"]

val send : [ `Use_Signal_unix ] [@@deprecated "[since 2021-04] Use [Signal_unix]"]
val send_exn : [ `Use_Signal_unix ] [@@deprecated "[since 2021-04] Use [Signal_unix]"]
val send_i : [ `Use_Signal_unix ] [@@deprecated "[since 2021-04] Use [Signal_unix]"]

val sexp_of_pid_spec : [ `Use_Signal_unix ]
[@@deprecated "[since 2021-04] Use [Signal_unix]"]

val sigpending : [ `Use_Signal_unix ] [@@deprecated "[since 2021-04] Use [Signal_unix]"]
val sigprocmask : [ `Use_Signal_unix ] [@@deprecated "[since 2021-04] Use [Signal_unix]"]
val sigsuspend : [ `Use_Signal_unix ] [@@deprecated "[since 2021-04] Use [Signal_unix]"]

val to_system_int : [ `Use_Signal_unix ]
[@@deprecated "[since 2021-04] Use [Signal_unix]"]
