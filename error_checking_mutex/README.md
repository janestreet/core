# Error_checking_mutex

An error checking mutex is like the OCaml standard library's `Mutex`,
but locking and unlocking perform additional checks that can raise.
Locking a mutex twice from the same thread, unlocking an unlocked
mutex, or unlocking a mutex not held by the thread will result in a
`Sys_error` exception.
