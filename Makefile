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

raw_l: vm parenthetically | bin
	./bin/vm --raw blob/root -pb bootstrap -lf generated/parenthetically -o bin/raw_l

raw_m: vm exponentially raw_l | bin
	./bin/vm --raw bin/raw_l -pb generated/parenthetically -lf generated/exponentially -o bin/raw_m

raw_n: vm practically raw_m | bin
	./bin/vm --raw bin/raw_m -pb generated/exponentially -lf generated/practically -o bin/raw_n

raw_o: vm singularity_blob raw_n | bin
	./bin/vm --raw bin/raw_n -pb generated/practically -lf generated/singularity_blob -o bin/raw_o

raw_p: vm singularity raw_o | bin
	./bin/vm --raw bin/raw_o -pb generated/singularity_blob -lf singularity -o bin/raw_p

raw_q: vm semantically raw_p | bin
	./bin/vm --raw bin/raw_p -pb singularity -lf semantically -o bin/raw_q

raw_r: vm stringy raw_q | bin
	./bin/vm --raw bin/raw_q -pb semantically -lf stringy -o bin/raw_r

raw_s: vm binary raw_r | bin
	./bin/vm --raw bin/raw_r -pb stringy -lf binary -o bin/raw_s

raw_t: vm algebraically raw_s | bin
	./bin/vm --raw bin/raw_s -pb binary -lf algebraically -o bin/raw_t

raw_u: vm parity.hs raw_t | bin
	./bin/vm --raw bin/raw_t -pb algebraically -lf parity.hs -o bin/raw_u

raw_v: vm fixity.hs raw_u | bin
	./bin/vm --raw bin/raw_u -pb parity.hs -lf fixity.hs -o bin/raw_v

raw_w: vm typically.hs raw_v | bin
	./bin/vm --raw bin/raw_v -pb fixity.hs -lf typically.hs -o bin/raw_w

raw_x: vm classy.hs raw_w | bin
	./bin/vm --raw bin/raw_w -pb typically.hs -lf classy.hs -o bin/raw_x

raw_y: vm barely.hs raw_x | bin
	./bin/vm --raw bin/raw_x -pb classy.hs -lf barely.hs -o bin/raw_y

raw_z: vm barely.hs raw_y | bin
	./bin/vm --raw bin/raw_y -pb barely.hs -lf barely.hs -o bin/raw_z

raw: vm barely.hs raw_z | bin
	./bin/vm -l bin/raw_z -lf barely.hs -o bin/raw

lonely_raw.txt: vm effectively.hs lonely.hs raw | generated
	./bin/vm -l bin/raw -lf effectively.hs --redo -lf lonely.hs -o generated/lonely_raw.txt

patty_raw.txt: patty.hs lonely_raw.txt | generated
	./bin/vm -f patty.hs --raw generated/lonely_raw.txt --rts_c run -o generated/patty_raw.txt

guardedly_raw.txt: guardedly.hs patty_raw.txt | generated
	./bin/vm -f guardedly.hs --raw generated/patty_raw.txt --rts_c run -o generated/guardedly_raw.txt

assembly_raw.txt: assembly.hs guardedly_raw.txt | generated
	./bin/vm -f assembly.hs --raw generated/guardedly_raw.txt --rts_c run -o generated/assembly_raw.txt

mutually_raw.txt: mutually.hs assembly_raw.txt | generated
	./bin/vm -f mutually.hs --foreign 2 --raw generated/assembly_raw.txt --rts_c run -o generated/mutually_raw.txt

uniquely_raw.txt: uniquely.hs mutually_raw.txt | generated
	./bin/vm -f uniquely.hs --foreign 2 --raw generated/mutually_raw.txt --rts_c run -o generated/uniquely_raw.txt

virtually_raw.txt: virtually.hs uniquely_raw.txt | generated
	./bin/vm -f virtually.hs --foreign 2 --raw generated/uniquely_raw.txt --rts_c run -o generated/virtually_raw.txt

marginally: marginally.c | bin
	$(CC) $(CFLAGS) generated/marginally.c -o bin/marginally

marginally.c:marginally.hs virtually_raw.txt | generated
	./bin/vm -f marginally.hs --foreign 2 --raw generated/virtually_raw.txt --rts_c run -o generated/marginally.c

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
