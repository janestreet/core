#!/bin/bash

here=$(dirname $(readlink -f $0))
cd $here

cmd=./main.exe
out=./actual-output
std=./expected-output

mkdir -p $out
rm -rf $out/*

export COLUMNS=90

function run {
  tok=$(echo "$@" | tr ' ' '_' | sed -r 's|([^.])/|\1_|g')
  "$@" >$out/$tok.stdout 2>$out/$tok.stderr
  # sanitize out line numbers in output, if any, because they make the test brittle.
  sed -i -r 's/((Called from|Raised at|Re-raised at) file .* line) [0-9]+/\1 @@@/' \
    $out/$tok.stderr 
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

function command-output-help-sexp {
  version_string="$1"; shift
  foo="$1"

  export COMMAND_OUTPUT_HELP_SEXP="($version_string)"
  tok="$(echo "$foo $version_string" | tr ' ' '_' | sed -r 's|([^.])/|\1_|g')-command-output-help-sexp"
  ("$foo" || echo) >"$out/$tok-ugly.stdout" 2>"$out/$tok.stderr"
  unset COMMAND_OUTPUT_HELP_SEXP

  sed -i 's|(path_to_exe '$(readlink -f nested.exe)')|(path_to_exe XPWDX)|' "$out/$tok-ugly.stdout" $out/$tok.stderr

  cat "$out/$tok-ugly.stdout" | sexp print >$out/$tok.stdout
  rm "$out/$tok-ugly.stdout"
}

group-cmd './main.exe' jab adverb nested
group-cmd './main.exe adverb' nemeses
version-cmd './main.exe'
run ./main.exe jab -help
command-output-help-sexp "1" './main.exe'
command-output-help-sexp "1 2" './main.exe'
command-output-help-sexp "-1" './main.exe'

# check we get a nice sexp when an exception is raised during command line parsing
run ./main.exe parse-sexp-file input-files/malformed-sexp || true

# check that commands actually get run once they get arguments
run ./main.exe jab 123

# check that aborting no-arg flag works
run ./main.exe jab -happy
run ./main.exe jab 123 -happy
run ./main.exe jab -fondue -happy


function cleanup-std {
    hg status -ni0 $std | xargs -0 -r rm -f
    find $std -type d | tac | xargs -d $'\n' -r rmdir --ignore-fail-on-non-empty
}

cleanup-std
diff -r $out $std
