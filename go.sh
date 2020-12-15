#!/usr/bin/env bash

set -eux

# Where we put our binaries
mkdir -p bin

# Place to put generated source files
mkdir -p generated

# compile pack_blobs.c
M2-Planet --architecture x86 \
	-f functions/file.c \
	-f functions/exit.c \
	-f functions/malloc.c \
	-f functions/calloc.c \
	-f functions/file_print.c \
	-f functions/match.c \
	-f functions/require.c \
	-f pack_blobs.c \
	--debug \
	-o bin/pack_blobs.M1

# Create dwarf stubs for pack_blobs
blood-elf -f bin/pack_blobs.M1 --entry _start -o bin/pack_blobs-footer.M1

# Convert to hex2 linker format
M1 -f test/common_x86/x86_defs.M1 \
	-f test/common_x86/libc-core.M1 \
	-f bin/pack_blobs.M1 \
	-f bin/pack_blobs-footer.M1 \
	--LittleEndian \
	--architecture x86 \
	-o bin/pack_blobs.hex2

# Link into final static binary
hex2 -f test/common_x86/ELF-i386-debug.hex2 \
	-f bin/pack_blobs.hex2 \
	--LittleEndian \
	--architecture x86 \
	--BaseAddress 0x8048000 \
	-o bin/pack_blobs --exec_enable

# Build blobs
./bin/pack_blobs -f blob/parenthetically.source -o generated/parenthetically
./bin/pack_blobs -f blob/exponentially.source -o generated/exponentially
./bin/pack_blobs -f blob/practically.source -o generated/practically
./bin/pack_blobs -f blob/singularity.source -o generated/singularity_blob

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
./bin/vm --bootstrap \
		-lf generated/parenthetically \
		-lf generated/exponentially \
		-lf generated/practically \
		-lf generated/singularity_blob \
		-lf singularity \
		-lf semantically \
		-lf stringy \
		-lf binary \
		-lf algebraically \
		-lf parity.hs \
		-lf fixity.hs \
		-lf typically.hs \
		-lf classy.hs \
		-lf barely.hs \
		-lf barely.hs \
		-lfr barely.hs \
		-o bin/raw

# Make lonely
./bin/vm -f lonely.hs -l bin/raw run effectively.hs -o generated/lonely.c

# Make patty
./bin/vm -f patty.hs --rts_c generated/lonely.c -o generated/patty.c

# Make guardedly
./filter_haskell_output generated/patty.c generated/patty_raw.txt
./bin/vm -f guardedly.hs --rts_c generated/patty_raw.txt -o generated/guardedly.c

# Make assembly
./filter_haskell_output generated/guardedly.c generated/guardedly_raw.txt
./bin/vm -f assembly.hs --rts_c generated/guardedly_raw.txt -o generated/assembly.c

# Make mutually
./filter_haskell_output generated/assembly.c generated/assembly_raw.txt
./bin/vm -f mutually.hs --foreign 2 --rts_c generated/assembly_raw.txt -o generated/mutually.c

# Make uniquely
./filter_haskell_output generated/mutually.c generated/mutually_raw.txt
./bin/vm -f uniquely.hs --foreign 2 --rts_c generated/mutually_raw.txt -o generated/uniquely.c

# Make virtually
./filter_haskell_output generated/uniquely.c generated/uniquely_raw.txt
./bin/vm -f virtually.hs --foreign 2 --rts_c generated/uniquely_raw.txt -o generated/virtually.c

# Make marginally
./filter_haskell_output generated/virtually.c generated/virtually_raw.txt
./bin/vm -f marginally.hs --foreign 2 --rts_c generated/virtually_raw.txt -o generated/marginally.c
