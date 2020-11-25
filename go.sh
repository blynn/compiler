#!/bin/bash

set -x

mkdir -p bin

M2-Planet --architecture x86 \
    -f functions/file.c \
    -f functions/exit.c \
    -f functions/malloc.c \
    -f functions/calloc.c \
    -f functions/file_print.c \
    -f functions/in_set.c \
    -f functions/numerate_number.c \
    -f functions/match.c \
    -f vm.c \
    --debug \
    -o vm.M1

blood-elf -f vm.M1 --entry _start -o vm-footer.M1

M1 -f test/common_x86/x86_defs.M1 \
   -f test/common_x86/libc-core.M1 \
   -f vm.M1 \
   -f vm-footer.M1 \
   --LittleEndian \
   --architecture x86 \
   -o vm.hex2

hex2 -f test/common_x86/ELF-i386-debug.hex2 \
    -f vm.hex2 \
    --LittleEndian \
    --architecture x86 \
    --BaseAddress 0x8048000 \
    -o bin/vm2 --exec_enable

./bin/vm2
