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
# it doesn't really matter whether this is bytecomp_c_compiler or native_c_compiler, it
# just needs to be a C compiler
CC="$bytecomp_c_compiler"

SRC=config/test.c
OUT=config/test.out
trap "rm -f $OUT" EXIT

$OCAMLC -ccopt -E $OCAML_CFLAGS -c $SRC | grep '^"OUT:[^"]*"$' | sed 's/"OUT:\([^"]*\)"/\1/' | tee > $OUT

ARCH=`ocamlc -config | sed -nr 's/^architecture: *(.*)$/\1/p'`
[ "$ARCH" = amd64 ] && ARCH=x86_64
for arch in x86_64 i386; do
    if [ "$ARCH" = "$arch" ]; then
        echo "#let JSC_ARCH_$arch = true" >> $OUT
    else
        echo "#let JSC_ARCH_$arch = false" >> $OUT
    fi
done

WORDEXP=`{ echo '#include <wordexp.h>' | cpp &> /dev/null && echo true; } || echo false`
echo "#let JSC_WORDEXP = $WORDEXP" >> $OUT

# The recvmmsg system call was added in Linux 2.6.32
if $CC config/test_recvmmsg.c -o /dev/null 2> /dev/null; then
    echo "#let JSC_RECVMMSG = true" >> $OUT
else
    echo "#let JSC_RECVMMSG = false" >> $OUT
fi

if $CC config/test_timerfd.c -o /dev/null 2> /dev/null; then
    echo "#let JSC_TIMERFD = true" >> $OUT
else
    echo "#let JSC_TIMERFD = false" >> $OUT
fi

for i in 1 2 3; do
    if $CC -I src -DJSC_STAT_NANOSEC_METHOD=$i config/test_nanosecond_stat.c -o /dev/null 2> /dev/null; then
        echo "#let JSC_STAT_NANOSEC_METHOD = $i" >> $OUT
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
    sed -r 's|^#let *([A-Za-z_0-9]+) *= *true *$|#define \1|;
            s|^#let *([A-Za-z_0-9]+) *= *false *$|#undef \1|;
            s|^#let *([A-Za-z_0-9]+) *= *([^ ])* *$|#define \1 \2|' "$ML_OUTFILE"
    cat  <<EOF
#endif /* $sentinel */
EOF
} > "$C_OUTFILE"
