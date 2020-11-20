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

vm: vm.c functions/match.c functions/file_print.c | bin
	$(CC) $(CFLAGS) vm.c functions/file_print.c functions/match.c -o bin/vm

raw: vm | bin
	./bin/vm > bin/raw

lonely:lonely.c | bin
	$(CC) $(CFLAGS) generated/lonely.c -o bin/lonely

lonely.c: vm effectively.hs lonely.hs rts.c raw | generated
	cp rts.c generated/lonely.c
	./bin/vm run effectively.hs < lonely.hs >> generated/lonely.c

patty: patty.c | bin
	$(CC) $(CFLAGS) generated/patty.c -o bin/patty

patty.c: patty.hs lonely | generated
	cp rts.c generated/patty.c
	./bin/lonely < patty.hs >> generated/patty.c

guardedly: guardedly.c | bin
	$(CC) $(CFLAGS) generated/guardedly.c -o bin/guardedly

guardedly.c: guardedly.hs patty | generated
	cp rts.c generated/guardedly.c
	./bin/patty < guardedly.hs >> generated/guardedly.c

assembly: assembly.c | bin
	$(CC) $(CFLAGS) generated/assembly.c -o bin/assembly

assembly.c: assembly.hs guardedly | generated
	cp rts.c generated/assembly.c
	./bin/guardedly < assembly.hs >> generated/assembly.c

mutually: mutually.c | bin
	$(CC) $(CFLAGS) generated/mutually.c -o bin/mutually

mutually.c: mutually.hs assembly | generated
	cp rts.c generated/mutually.c
	./bin/assembly < mutually.hs >> generated/mutually.c

uniquely: uniquely.c | bin
	$(CC) $(CFLAGS) generated/uniquely.c -o bin/uniquely

uniquely.c: uniquely.hs mutually | generated
	cp rts.c generated/uniquely.c
	./bin/mutually < uniquely.hs >> generated/uniquely.c

virtually: virtually.c | bin
	$(CC) $(CFLAGS) generated/virtually.c -o bin/virtually

virtually.c: virtually.hs uniquely | generated
	cp rts.c generated/virtually.c
	./bin/uniquely < virtually.hs >> generated/virtually.c

marginally: marginally.c | bin
	$(CC) $(CFLAGS) generated/marginally.c -o bin/marginally

marginally.c:marginally.hs virtually | generated
	./bin/virtually < marginally.hs > generated/marginally.c

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
