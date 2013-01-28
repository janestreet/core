#!/usr/bin/env bash

# Detect supported features and put the result in setup.data

set -e -u -o pipefail

if uname | grep -q -i linux; then
    linux_possible=true
else
    linux_possible=false
fi

if [[ $(getconf _POSIX_TIMERS) -ge 200112 ]]; then
    posix_timers_possible=true
else
    posix_timers_possible=false
fi

if [[ -e setup.data ]]; then
    sed -i '/^\(linux\|posix_timers\)_possible=/d' setup.data
fi

cat >> setup.data <<EOF
linux_possible="$linux_possible"
posix_timers_possible="$posix_timers_possible"
EOF
