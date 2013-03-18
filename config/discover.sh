#!/bin/sh

set -e

if [ $# -lt 2 ]; then
    echo "Usage: discover.sh OCAMLC ML_OUTFILE C_OUTFILE" >&2
    exit 2
fi

OCAMLC="$1"
ML_OUTFILE="$2"
C_OUTFILE="$3"
shift 3

if [ ! -e setup.data ]; then
    echo "setup.data missing, run ./configure first."
    exit 2
fi

OCAML_CFLAGS=

. ./setup.data
[ "$linux" = true ] && OCAML_CFLAGS="$OCAML_CFLAGS -ccopt -DLINUX_EXT"
[ "$posix_timers" = true ] && OCAML_CFLAGS="$OCAML_CFLAGS -ccopt -DPOSIX_TIMERS"

MAKEFILE_CONFIG=`ocamlc -where`/Makefile.config
if [ ! -e $MAKEFILE_CONFIG ]; then
    echo "Makefile.config missing in ocaml standard library path."
    echo 2
fi

ARCH=`cat $MAKEFILE_CONFIG | grep '^ARCH=' | cut -d= -f2`

SRC=config/test.c
OUT=config/test.out
trap "rm -f $OUT" EXIT

$OCAMLC -ccopt -E $OCAML_CFLAGS -c $SRC | grep '^"OUT:[^"]*"$' | sed 's/"OUT:\([^"]*\)"/\1/' | tee > $OUT

echo "DEFINE ARCH_$ARCH" >> $OUT

case "`ocamlc -version`" in
    4*)
        echo "DEFINE OCAML_4" >> $OUT
esac

mv "$OUT" "$ML_OUTFILE"


{
    sentinel="CORE_`basename "$C_OUTFILE" | tr a-z. A-Z_`"
    cat  <<EOF
#ifndef $sentinel
#define $sentinel
EOF
    sed 's/^DEFINE */#define JSC_/' "$ML_OUTFILE"
    cat  <<EOF
#endif /* $sentinel */
EOF
} > "$C_OUTFILE"
