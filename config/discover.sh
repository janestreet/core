#!/bin/sh


set -e

if [ $# -ne 2 ]; then
    echo "Usage: discover.sh OCAMLC OUTFILE" >&2
    exit 2
fi

OCAMLC="$1"
OUTFILE="$2"
shift 2

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
OUT=config/result.h
trap "rm -f $OUT" EXIT

sentinel="CORE_`basename "$OUTFILE" | tr a-z. A-Z_`"
cat > $OUT  <<EOF
#ifndef $sentinel
#define $sentinel
EOF


$OCAMLC -ccopt -E $OCAML_CFLAGS -c $SRC | grep '^"OUT:[^"]*"$' | sed 's/"OUT:\([^"]*\)"/\1/' | tee >> $OUT

ARCH=`ocamlc -config | sed -n 's/^architecture: *\(.*\)$/\1/p'`
[ "$ARCH" = amd64 ] && ARCH=x86_64
for arch in x86_64 i386; do
    if [ "$ARCH" = "$arch" ]; then
        echo "#define JSC_ARCH_$arch" >> $OUT
    else
        echo "#undef JSC_ARCH_$arch" >> $OUT
    fi
done

if echo '#include <wordexp.h>' | cpp > /dev/null 2> /dev/null; then
    echo "#define JSC_WORDEXP" >> $OUT
else
    echo "#undef JSC_WORDEXP" >> $OUT
fi

# The recvmmsg system call was added in Linux 2.6.32
if $CC config/test_recvmmsg.c -o /dev/null 2> /dev/null; then
    echo "#define JSC_RECVMMSG" >> $OUT
else
    echo "#undef JSC_RECVMMSG" >> $OUT
fi

if $CC config/test_timerfd.c -o /dev/null 2> /dev/null; then
    echo "#define JSC_TIMERFD" >> $OUT
else
    echo "#undef JSC_TIMERFD" >> $OUT
fi

if $CC config/test_eventfd.c -o /dev/null 2> /dev/null; then
    echo "#define JSC_EVENTFD" >> $OUT
else
    echo "#undef JSC_EVENTFD" >> $OUT
fi

for i in 1 2 3; do
    if $CC -I src -DJSC_STAT_NANOSEC_METHOD=$i config/test_nanosecond_stat.c -o /dev/null 2> /dev/null; then
        echo "#define JSC_STAT_NANOSEC_METHOD $i" >> $OUT
        break
    fi
done

cat >> $OUT  <<EOF
#endif
EOF

mv $OUT $OUTFILE
