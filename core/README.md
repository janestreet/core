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

## Working with times and durations

Core separates absolute points in time from durations and local calendar
values. The following public modules are useful entry points when navigating
the time interfaces:

| Value | Public type | Interface |
| --- | --- | --- |
| An absolute point in time, represented using floating-point seconds since the Unix epoch | `Time_float.t` | [Time_float](src/time_float.mli) |
| An absolute point in time, represented using integer nanoseconds since the Unix epoch | `Time_ns.t` | [Time_ns](src/time_ns_intf.ml) |
| A duration, rather than an absolute time | `Time_float.Span.t` or `Time_ns.Span.t` | [Float spans](src/span_float.mli), [nanosecond spans](src/time_ns_intf.ml) |
| A time of day, without a date or time zone | `Time_float.Ofday.t` or `Time_ns.Ofday.t` | [Float ofdays](src/ofday_float.mli), [nanosecond ofdays](src/time_ns_intf.ml) |
| A calendar date | `Date.t` | [Date](src/date.mli) |
| Time-zone rules used to interpret local calendar values | `Timezone.t` (also exposed through the time modules' `Zone` submodules) | [Timezone](src/timezone.mli) |

Choose a time representation and use its corresponding `Span` and `Ofday`
submodules. For example, `Time_ns.diff` produces a `Time_ns.Span.t`, while
`Time_ns.add` takes an absolute time and a span. `Time_ns` has a narrower
representable range than `Time_float`; its interface documents the range and
overflow limitations.

A date and an ofday do not by themselves identify an unambiguous absolute
time. Converting between them and an absolute time requires a time zone.
Around daylight-saving transitions, a local time can occur twice or not at
all. The shared [conversion interface](src/time_intf.ml) documents
`of_date_ofday`, `to_date_ofday`, and `of_date_ofday_precise`, including how
the last reports ambiguous or skipped local times.

------

Please report bugs and feature requests on
[GitHub](https://github.com/janestreet/core). For everything else you
can contact us at <opensource-contacts@janestreet.com>.

You can find all of Jane Street's open-source libraries on
[GitHub](https://github.com/janestreet).

Documentation can be found
[here](https://ocaml.janestreet.com/ocaml-core/latest/doc/core/index.html).
