#!/bin/bash

set -eux

# Where we put our binaries
mkdir -p bin

# Compile to assembly vm.c
M2-Planet --architecture x86 \
	-f functions/file.c \
	-f functions/exit.c \
	-f functions/malloc.c \
	-f functions/calloc.c \
	-f functions/file_print.c \
	-f functions/in_set.c \
	-f functions/numerate_number.c \
	-f functions/match.c \
	-f functions/require.c \
	-f vm.c \
	--debug \
	-o bin/vm.M1

# Create dwarf stubs for vm
blood-elf -f bin/vm.M1 --entry _start -o bin/vm-footer.M1

# Convert to hex2 linker format
M1 -f test/common_x86/x86_defs.M1 \
	-f test/common_x86/libc-core.M1 \
	-f bin/vm.M1 \
	-f bin/vm-footer.M1 \
	--LittleEndian \
	--architecture x86 \
	-o bin/vm.hex2

# Link into final static binary
hex2 -f test/common_x86/ELF-i386-debug.hex2 \
	-f bin/vm.hex2 \
	--LittleEndian \
	--architecture x86 \
	--BaseAddress 0x8048000 \
	-o bin/vm --exec_enable

# Generate raw file needed
./bin/vm rpg > bin/raw

# Place to put generated source files
mkdir -p generated

# Make lonely
cp rts.c generated/lonely.c
./bin/vm run effectively.hs < lonely.hs >> generated/lonely.c
#TODO Steps to compile lonely.c into bin/lonely
