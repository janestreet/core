# Core_thread

`Core_thread` is a single-module library that is an alternative to the
OCaml stdlib's `Thread` module.  Prior to 2021-04, `Core_thread` was
`Core.Thread`.

Idiomatic usage is to put a module alias in `import.ml` or near the
top of a file:

    module Thread = Core_thread
