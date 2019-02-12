#!/bin/bash

nasm "$1" -o "/tmp/$1.tmp"
xxd -i "/tmp/$1.tmp" - | sed 's/_tmp_\(.*\)_asm_tmp/\1/'
rm "/tmp/$1.tmp"
