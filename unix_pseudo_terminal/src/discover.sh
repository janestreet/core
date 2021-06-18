#!/usr/bin/env bash
set -e -u -o pipefail

if [ $# -lt 2 ]; then
    echo "Usage: discover.sh CONFIG_H CORE_CONFIG_H" >&2
    exit 2
fi

CONFIG_H="$1"
CORE_CONFIG_H="$2"
shift 2

if getconf GNU_LIBC_VERSION | \
    awk -F '[ .]' '{ exit ($2 > 2 || ($2 == 2 && $3 >= 8) ? 0 : 1) }'; then
    set -- -DTIMERFD "$@"
fi

SRC="$(x=$(mktemp "./discover_src.XXXXXXX") && mv "$x"{,.c} && echo "$x".c)"
PGM="$(mktemp "./discover.XXXXXXX")"
OUT="$(mktemp "./discover.out.XXXXXXX")"

trap "rm -f $SRC $PGM $OUT" EXIT

sentinel="CORE_$(basename "$CONFIG_H" | tr a-z. A-Z_)"
cat > $OUT <<EOF
#ifndef $sentinel
#define $sentinel
EOF

cat > "$SRC" <<EOF
#define _GNU_SOURCE
#define _XOPEN_SOURCE 600

#ifndef __USE_BSD
#define __USE_BSD
#endif

#include <fcntl.h>
#include <stdlib.h>
#include <termios.h>

#if defined(__sun)
#define NO_POSIX_OPENPT
#endif

int main () { grantpt(0); unlockpt(0); ptsname(0); }
EOF
if gcc "$SRC" -o "$PGM" "$@"; then
    echo '#define JSC_UNIX_PTY' >>"$OUT";
else
    echo '#undef JSC_UNIX_PTY' >>"$OUT";
fi
rm "$SRC"

cat >> $OUT <<EOF
#endif
EOF

cat "$CORE_CONFIG_H" >> $OUT

rm -f "$PGM"
mv "$OUT" "$CONFIG_H"
