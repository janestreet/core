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
WORDEXP=`{ echo '#include <wordexp.h>' | cpp &> /dev/null && echo yes; } || echo no`

SRC=config/test.c
OUT=config/test.out
trap "rm -f $OUT" EXIT

$OCAMLC -ccopt -E $OCAML_CFLAGS -c $SRC | grep '^"OUT:[^"]*"$' | sed 's/"OUT:\([^"]*\)"/\1/' | tee > $OUT

echo "DEFINE ARCH_$ARCH" >> $OUT

if [ "$ARCH" = amd64 ]; then
    echo "DEFINE ARCH_x86_64" >> $OUT
fi

if [ "$WORDEXP" = yes ]; then
    echo "DEFINE WORDEXP" >> $OUT
fi

OCAML_VERSION="`ocamlc -version`"
major=`echo $OCAML_VERSION | cut -d. -f1`
minor=`echo $OCAML_VERSION | cut -d. -f2`
if [ $major -ge 4 ]; then
    echo "DEFINE OCAML_4" >> $OUT
    [ $minor -ge 1 ] && echo "DEFINE OCAML_4_01" >> $OUT
    [ $minor -ge 2 ] && echo "DEFINE OCAML_4_02" >> $OUT
fi

# The recvmmsg system call was added in Linux 2.6.32
if cc config/test_recvmmsg.c -o /dev/null; then
    echo "DEFINE RECVMMSG" >> $OUT;
fi

if cc config/test_timerfd.c -o /dev/null; then
    echo "DEFINE TIMERFD" >> $OUT;
fi

for i in 1 2 3; do
    if cc -I lib -DJSC_STAT_NANOSEC_METHOD=$i config/test_nanosecond_stat.c -o /dev/null 2> /dev/null; then
        echo "DEFINE STAT_NANOSEC_METHOD = $i" >> $OUT
        break
    fi
done

mv "$OUT" "$ML_OUTFILE"

{
    sentinel="CORE_`basename "$C_OUTFILE" | tr a-z. A-Z_`"
    cat  <<EOF
#ifndef $sentinel
#define $sentinel
EOF
    sed 's/^DEFINE */#define JSC_/;s/=//' "$ML_OUTFILE"
    cat  <<EOF
#endif /* $sentinel */
EOF
} > "$C_OUTFILE"
