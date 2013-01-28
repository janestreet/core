#!/usr/bin/env bash
set -e -u -o pipefail

ML_OUTFILE="${1:?Usage: discover.sh ML_OUTFILE C_OUTFILE}"
C_OUTFILE="${2:?Usage: discover.sh ML_OUTFILE C_OUTFILE}"
shift 2

if [[ -e setup.data ]]; then
    . setup.data
    if ${linux-false}; then set -- -DLINUX_EXT "$@"; fi
    if ${posix_timers-false}; then set -- -DPOSIX_TIMERS "$@"; fi
fi

function cpp_test () {
    local name="$1"
    local cond="$2"
    if [[ "$#" == 3 ]]; then
        local warning="$3"
    else
        local warning="Feature $1 will not be availlable"
    fi
    cat <<EOF
#if ${cond}
printf ("DEFINE $name\n");
EOF
if [[ "$warning" != "" ]]; then
cat <<EOF
#else
#  warning "cpp test --${cond}-- was false"
#  warning "$warning"
EOF
fi
echo "#endif"
}

WORD_SIZE="$(ocaml <( echo "print_int Sys.word_size;;"))"
SRC="$(mktemp "./discover_src.XXXXXXX.c")"
PGM="$(mktemp "./discover.XXXXXXX")"
OUT="$(mktemp "./discover.out.XXXXXXX")"

trap "rm -f $SRC $PGM $OUT" EXIT
cat > "$SRC" <<EOF
#include <stdio.h>
#include <unistd.h>
#include <sys/socket.h>
# $LINENO "$(basename "${BASH_SOURCE[0]}")"
int main () {
  $(cpp_test LINUX_EXT    "defined(LINUX_EXT)")
  $(cpp_test POSIX_TIMERS "defined(POSIX_TIMERS)")
  $(if [[ ${WORD_SIZE} = 64 ]]; then
       echo 'printf ("DEFINE ARCH_SIXTYFOUR\n");';
    fi)
  $(if [[ $(uname -p) = "x86_64" ]]; then
       echo 'printf ("DEFINE ARCH_x86_64\n");';
    fi)
  $(if [[ $(uname -p) = "i386" ]]; then
       echo 'printf ("DEFINE ARCH_i386\n");';
    fi)
  $(cpp_test MSG_NOSIGNAL "defined MSG_NOSIGNAL" \
     "Bigstring.(unsafe_|really_)?send(to)?(_noblocking)?_no_sigpipe\
 will not be availlable")
  $(cpp_test MUTEX_TIMED_LOCK \
     "defined(_POSIX_TIMEOUTS) && (_POSIX_TIMEOUTS > 0)")

  $(cpp_test FDATASYNC \
     "defined(_POSIX_SYNCHRONIZED_IO) && _POSIX_SYNCHRONIZED_IO > 0")

  return 0;
}
EOF
# Maybe we should leverage the ocaml compiler in here instead of hardcoding
# gcc
gcc "$SRC" -o "$PGM" "$@"
rm "$SRC"
"$PGM" > "$OUT"
rm "$PGM"
mv "$OUT" "$ML_OUTFILE"
cat "$ML_OUTFILE" \
    | sed -e 's|^DEFINE *|#define JSC_|' \
    | sed -e 's|\(#define JSC_[^ ]*\) *=|\1 |' > "$C_OUTFILE"
