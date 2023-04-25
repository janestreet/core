Portable standard library for OCaml
===================================

Core is an industrial-strength alternative to the OCaml standard
library.  It was developed by Jane Street, which is the largest
industrial user of OCaml.  Core works with Javascript.  It provides an
overlay on the usual namespace, so the best way to use it is to start
your file with:

```ocaml
open! Core
```

## Relationship to Core and Base

In sum:

- **Base**: Minimal stdlib replacement. Portable and lightweight and
  intended to be highly stable.

- **Core**: Extension of Base. More fully featured, with more code and
  dependencies, and APIs that evolve more quickly. Portable, and works
  on Javascript.

Many of Core's modules are extensions of modules in Base, where the
Core version adds `bin_io` support or locks in an API with
`Stable`. Some modules, like `Core.Map`, extend their Base equivalents
to follow Core conventions for the use of comparators.

------

Please report bugs and feature requests on
[GitHub](https://github.com/janestreet/core). For everything else you
can contact us at <opensource-contacts@janestreet.com>.

You can find all of Jane Street's open-source libraries on
[GitHub](https://github.com/janestreet).

Documentation can be found
[here](https://ocaml.janestreet.com/ocaml-core/latest/doc/core/index.html).
