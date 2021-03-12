# Time_ns_unix

`Time_ns_unix` is a standalone library that extends `Core.Time_ns` and
depends on `Core_unix`.  Prior to 2020-03, `Time_ns_unix` was
`Core.Time_ns`.

Much functionality in `Time_ns_unix` doesn't actually need to depend
on `Core_unix`.  We hope to over time move such functionality into
`Core.Time_ns`.

For stable types, idiomatic usage is to add an alias to the stable
submodule:

    module Time_ns = Time_ns_unix.Stable
