# Time_unix

`Time_unix` is a standalone library that extends `Core.Time` and
depends on `Core_unix`.  Prior to 2020-03, `Time_unix` was
`Core.Time`.

Much functionality in `Time_unix` doesn't actually need to depend
on `Core_unix`.  We hope to over time move such functionality into
`Core.Time`.

Idiomatic usage is to put a module alias in `import.ml` or near the
top of a file:

    module Time = Time_unix

For stable types, idiomatic usage is to add an alias to the stable
submodule:

    module Time = Time_unix.Stable
