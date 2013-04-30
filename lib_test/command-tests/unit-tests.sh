#!/bin/bash

here=$(dirname $(readlink -f $0))
cd $here

cmd=./main.exe
out=./actual-output
std=./expected-output

rm -rf $out
mkdir -p $out

export COLUMNS=90

function run {
  tok=$(echo "$@" | tr ' ' '_')
  "$@" >$out/$tok.stdout 2>$out/$tok.stderr
}

function group-cmd {
  foo="$1"; shift
  run $foo
  run $foo help help
  for sub in '' 'help' "$@"; do
    run $foo -help $sub
    run $foo $sub -help
    run $foo help $sub
    run $foo help $sub
    run $foo help $sub -recursive
    run $foo help $sub -recursive -expand-dots
    run $foo help $sub -recursive -expand-dots
    run $foo help $sub -recursive -expand-dots -flags
    run $foo help $sub -recursive              -flags
  done
}

function version-cmd {
  foo="$1"

  set -e
  diff <(./$foo version -version) <(sort $(hg root)/hg_version.out)
  # add a trailing newline to build_info.sexp
  diff <(./$foo version -build-info) <(sed -e '$a\' $foo.build_info.sexp)
}

group-cmd './main.exe' jab adverb
group-cmd './main.exe adverb' nemeses
version-cmd 'main.exe'
run ./main.exe jab -help

diff -r $out $std
