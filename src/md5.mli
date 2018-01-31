(** Extends {{!Core_kernel.Md5}[Core_kernel.Md5]}. *)

include module type of struct include Core_kernel.Md5 end (** @open *)

(** This is similar to [digest_channel_blocking_without_releasing_runtime_lock] with a
    difference that this releases the OCaml lock for its whole duration. Therefore, this
    can run in parallel with other OCaml threads and can be meaningfully used in
    [In_thread.run].

    The file descriptor must be open for reading and not be nonblocking, otherwise the
    function might fail non-deterministically. *)
val digest_fd_blocking : Core_unix.File_descr.t -> t

(** This function is equivalent in behavior to
    [digest_file_blocking_without_releasing_runtime_lock], except this releases the OCaml
    lock for its whole duration. Therefore, this can run in parallel with other OCaml
    threads and can be meaningfully used in [In_thread.run]. *)
val digest_file_blocking : string -> t
