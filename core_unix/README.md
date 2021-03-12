# Core_unix

Jane Street's version of the OCaml stdlib's `UnixLabels` module.  This
wraps functions from the `Unix` module and adds new functionality.
Prior to 2021-03, this was `Core.Unix`.

For stable types, idiomatic usage is to add an alias to the stable
submodule:

     module Unix = Core_unix.Stable
