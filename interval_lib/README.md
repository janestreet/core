# Interval_lib

A single-module library for simple closed intervals over arbitrary
types.  Prior to 2021-02, this was `Core.Interval`.

This library is not named `Interval` due to a conflict with an OCaml
compiler library of that name.  As of 2021-02, that conflict prevents
us from having a library named `Interval`.

Idiomatic usage of this library in an `.ml` file is:

    module Interval = Interval_lib.Interval

Idiomatic usage of this library in an `.mli` file is:

    module Interval := Interval_lib.Interval

See `Interval_unix` for Unix specific intervals, like `Time_unix` and
`Time_ns_unix` based intervals.
