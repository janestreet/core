# Sys_unix

`Sys_unix` is a single-module library with Unix functionality not in
`Base.Sys` or `Core.Sys`.

Idiomatic usage is to refer explicitly to `Sys_unix`,
e.g. `Sys_unix.executable_name`.

Prior to 2021-04, `Sys_unix` was `Core.Sys`.
