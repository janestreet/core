#!/bin/bash

diff -u just_raise.expected-output <(./just_raise.exe 2>&1)
exit $?
