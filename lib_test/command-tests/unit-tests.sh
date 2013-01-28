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

group-cmd './main.exe' jab adverb
group-cmd './main.exe adverb' nemeses
run ./main.exe jab -help

diff -r $out $std
