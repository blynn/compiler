##
## Copyright © 2019 Ben Lynn
## This file is part of blynn-compiler.
##
## blynn-compiler is free software: you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation, only under version 3 of
## the License.
##
## blynn-compiler is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with blynn-compiler.  If not, see
## <https://www.gnu.org/licenses/>.
##

# Prevent rebuilding
VPATH = bin:generated

CC?=clang
CFLAGS:=$(CFLAGS) -D_GNU_SOURCE -std=c99 -ggdb -D WITH_GLIBC=1 -O2

ALL: precisely

vm: vm.c functions/match.c functions/in_set.c functions/numerate_number.c functions/file_print.c functions/require.c | bin
	$(CC) $(CFLAGS) vm.c functions/file_print.c functions/require.c functions/match.c functions/in_set.c functions/numerate_number.c -o bin/vm

pack_blobs: pack_blobs.c functions/match.c functions/file_print.c functions/require.c | bin
	$(CC) $(CFLAGS) pack_blobs.c functions/match.c functions/file_print.c functions/require.c -o bin/pack_blobs

parenthetically: pack_blobs | generated
	./bin/pack_blobs -f blob/parenthetically.source -o generated/parenthetically

exponentially: pack_blobs | generated
	./bin/pack_blobs -f blob/exponentially.source -o generated/exponentially

practically: pack_blobs | generated
	./bin/pack_blobs -f blob/practically.source -o generated/practically

singularity_blob: pack_blobs | generated
	./bin/pack_blobs -f blob/singularity.source -o generated/singularity_blob

raw: vm parenthetically exponentially practically singularity_blob | bin
	./bin/vm --bootstrap \
		-pb bootstrap \
		-lf generated/parenthetically \
		-pb generated/parenthetically \
		-lf generated/exponentially \
		-pb generated/exponentially \
		-lf generated/practically \
		-pb generated/practically \
		-lf generated/singularity_blob \
		-pb generated/singularity_blob \
		-lf singularity \
		-pb singularity \
		-lf semantically \
		-pb semantically \
		-lf stringy \
		-pb stringy \
		-lf binary \
		-pb binary \
		-lf algebraically \
		-pb algebraically \
		-lf parity.hs \
		-pb parity.hs \
		-lf fixity.hs \
		-pb fixity.hs \
		-lf typically.hs \
		-pb typically.hs \
		-lf classy.hs \
		-pb classy.hs \
		-lf barely.hs \
		-pb barely.hs \
		-lf barely.hs \
		--redo \
		-lf barely.hs \
		-o bin/raw

lonely_raw.txt: vm effectively.hs lonely.hs raw | generated
	./bin/vm -f lonely.hs -l bin/raw -lf effectively.hs --redo run -o generated/lonely_raw.txt

patty_raw.txt: patty.hs lonely_raw.txt | generated
	./bin/vm -f patty.hs --rts_c generated/lonely_raw.txt -o generated/patty_raw.txt

guardedly_raw.txt: guardedly.hs patty_raw.txt | generated
	./bin/vm -f guardedly.hs --rts_c generated/patty_raw.txt -o generated/guardedly_raw.txt

assembly_raw.txt: assembly.hs guardedly_raw.txt | generated
	./bin/vm -f assembly.hs --rts_c generated/guardedly_raw.txt -o generated/assembly_raw.txt

mutually_raw.txt: mutually.hs assembly_raw.txt | generated
	./bin/vm -f mutually.hs --foreign 2 --rts_c generated/assembly_raw.txt -o generated/mutually_raw.txt

uniquely_raw.txt: uniquely.hs mutually_raw.txt | generated
	./bin/vm -f uniquely.hs --foreign 2 --rts_c generated/mutually_raw.txt -o generated/uniquely_raw.txt

virtually_raw.txt: virtually.hs uniquely_raw.txt | generated
	./bin/vm -f virtually.hs --foreign 2 --rts_c generated/uniquely_raw.txt -o generated/virtually_raw.txt

marginally: marginally.c | bin
	$(CC) $(CFLAGS) generated/marginally.c -o bin/marginally

marginally.c:marginally.hs virtually_raw.txt | generated
	./bin/vm -f marginally.hs --foreign 2 --rts_c generated/virtually_raw.txt -o generated/marginally.c

methodically: methodically.c | bin
	$(CC) $(CFLAGS) generated/methodically.c -o bin/methodically

methodically.c:methodically.hs marginally | generated
	./bin/marginally < methodically.hs > generated/methodically.c

crossly: crossly.c | bin
	$(CC) $(CFLAGS) generated/crossly.c -o bin/crossly

crossly.c:crossly.hs methodically | generated
	./bin/methodically < crossly.hs > generated/crossly.c

precisely: precisely.c | bin
	$(CC) $(CFLAGS) generated/precisely.c -o bin/precisely

precisely.c:precisely.hs crossly | generated
	./bin/crossly < precisely.hs > generated/precisely.c

# Directories
bin:
	mkdir -p bin

generated:
	mkdir -p generated

.PHONY: clean
clean:
	rm -rf bin/ generated/
