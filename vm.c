/*
 * Copyright (c) 2019 Ben Lynn
 * This file is part of blynn-compiler.
 *
 * blynn-compiler is free software: you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation, only under version 3 of
 * the License.
 *
 * blynn-compiler is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with blynn-compiler.  If not, see
 * <https://www.gnu.org/licenses/>.
 */
#include <stdio.h>
#include <stdlib.h>
#include "gcc_req.h"

// CONSTANT FALSE 0
#define FALSE 0
// CONSTANT TRUE 1
#define TRUE 1

//CONSTANT FORWARD 27
#define FORWARD 27
//CONSTANT REDUCING 9
#define REDUCING 9

//CONSTANT TOP    16777216
#define TOP       16777216
//CONSTANT TABMAX 1024
#define TABMAX    1024
//CONSTANT BUFMAX 1048576
#define BUFMAX    1048576

//CONSTANT CELL_SIZE sizeof(unsigned)
#define CELL_SIZE 1

int match(char* a, char* b);
void file_print(char* s, FILE* f);
char* numerate_number(int a);
void require(int bool, char* error);

unsigned* mem;
unsigned* altmem;
unsigned* sp;
unsigned* spTop;
unsigned hp;
unsigned* tab;
unsigned tabn;
FILE* destination_file;
FILE* input_file;

/* Trying to get vm.c and rts.c syncronized */
unsigned root_size;
unsigned* root;
int rts_c;
unsigned starting_address;

/* Stupid global to shutup warnings; should be removed when readers are unified */
unsigned failure;

void stats()
{
	file_print("[HP = ",stdout);
	file_print("$hp",stdout);
	file_print(", stack usage = ",stdout);
	file_print("$spTop",stdout);
	file_print("$sp",stdout);
	file_print("]\n",stdout);
}

unsigned isAddr(unsigned n)
{
	return n >= 128;
}

unsigned evac(unsigned n)
{
	if(!isAddr(n))
	{
		return n;
	}

	unsigned x = mem[n];

	while(isAddr(x) && mem[x] == 'T')
	{
		mem[n] = mem[n + 1];
		mem[n + 1] = mem[x + 1];
		x = mem[n];
	}

	if(isAddr(x) && mem[x] == 'K')
	{
		mem[n + 1] = mem[x + 1];
		x = mem[n] = 'I';
	}

	unsigned y = mem[n + 1];

	if(FORWARD == x)
	{
		return y;
	}
	else if(REDUCING == x)
	{
		mem[n] = FORWARD;
		mem[n + 1] = hp;
		hp = hp + 2;
		return mem[n + 1];
	}
	else if('I' == x)
	{
		mem[n] = REDUCING;
		y = evac(y);

		if(mem[n] == FORWARD)
		{
			altmem[mem[n + 1]] = 'I';
			altmem[mem[n + 1] + 1] = y;
		}
		else
		{
			mem[n] = FORWARD;
			mem[n + 1] = y;
		}

		return mem[n + 1];
	}

	unsigned z = hp;
	hp = hp + 2;
	mem[n] = FORWARD;
	mem[n + 1] = z;
	altmem[z] = x;
	altmem[z + 1] = y;
	return z;
}

/* Garbage collection */
void gc()
{
	/* Reset the heap pointer */
	hp = 128;
	unsigned di = hp;
	/* Set the stack pointer to point to the top of altmem */
	sp = altmem + (TOP * CELL_SIZE) - CELL_SIZE;


	unsigned i;
	for(i = 0; i < root_size; i = i + 1)
	{
		root[i] = evac(root[i]);
	}

	sp[0] = evac(spTop[0]);

	/* fprintf(stderr, "GC %u\n", hp - 128); */
	unsigned x;
	while(di < hp)
	{
		altmem[di] = evac(altmem[di]);
		x = altmem[di];
		di = di + 1;

		if(!rts_c)
		{
			if(x != 'a' && x != '#')
			{
				altmem[di] = evac(altmem[di]);
			}
		}
		else
		{
			if(x != 'F' && x != '#')
			{
				altmem[di] = evac(altmem[di]);
			}
		}

		di = di + 1;
	}

	spTop = sp;
	/* Swap the addresses of mem and altmem */
	unsigned *tmp = mem;
	mem = altmem;
	altmem = tmp;
}

/* An application of two nodes is represented by putting them in
   adjacent memory locations */
unsigned app(unsigned f, unsigned x)
{
	mem[hp] = f;
	mem[hp + 1] = x;
	hp = hp + 2;
	return hp - 2;
}

unsigned root_memo;
void reset(unsigned root)
{
	root_memo = root;
	sp = spTop;
	sp[0] = app(app(app(root, app('0', '?')), '.'), app('T', '1'));
}

void loadRaw(FUNCTION get)
{
	hp = 127;
	unsigned c;
	unsigned n;

	while(TRUE)
	{
		do
		{
			c = get(0);
		} while(c != 0 && (c < '0' || c > '9'));

		if(c == 0)
		{
			break;
		}

		n = 0;

		while(TRUE)
		{
			if(c < '0' || c > '9')
			{
				break;
			}

			n = 10 * n + c - '0';
			c = get(0);
		}

		mem[hp] = n;
		hp = hp + 1;
	}

	reset(mem[127]);
}

unsigned parseTerm(FUNCTION get)
{
	unsigned n;
	unsigned c;

	do
	{
		c = get(0);
	} while(c == '\n');

	if('`' == c)
	{
		c = parseTerm(get);
		return app(c, parseTerm(get));
	}
	else if('#' == c)
	{
		return app('#', get(0));
	}
	else if('@' == c)
	{
		return tab[get(0) - ' '];
	}
	else if('(' == c)
	{
		n = 0;

		while((c = get(0)) != ')')
		{
			n = 10 * n + c - '0';
		}

		return app('#', n);
	}
	else if('[' == c)
	{
		n = 0;

		while((c = get(0)) != ']')
		{
			n = 10 * n + c - '0';
		}

		return tab[n];
	}

	return c;
}

void parseMore(FUNCTION get)
{
	unsigned c;

	while(TRUE)
	{
		c = parseTerm(get);

		if(0 == c)
		{
			reset(tab[tabn - 1]);
			return;
		}

		require(tabn < TABMAX, "error: table overflow\n");

		tab[tabn] = c;
		tabn = tabn + 1;

		require(get(0) == ';', "error: expected ';'\n");
	}
}

char *str;

unsigned str_get(unsigned f)
{
	failure = f;
	unsigned str_get_c = str[0] & 0xFF;
	str = str + 1;
	return str_get_c;
}

void parse(char *s)
{
	hp = 128;
	tabn = 0;
	str = s;
	parseMore(str_get);
}

/* Since application nodes are stored in adjacent memory locations, we
   can get the nth argument. */
unsigned arg(unsigned n)
{
	return mem[sp [n] + 1];
}

/* If the argument is a number, then we call arg to get the pointer to
   the number in memory, then find its value by indexing into mem. */
unsigned num(unsigned n)
{
	return mem[arg(n) + 1];
}

unsigned lazy(unsigned height, unsigned f, unsigned x)
{
	unsigned* p;
	p = mem + (sp[height] * CELL_SIZE);
	p[0] = f;
	p = p + CELL_SIZE;
	p[0] = x;
	sp = sp + (height * CELL_SIZE) - CELL_SIZE;
	sp[0] = f;
	return 0;
}

unsigned lazy3(unsigned height, unsigned x1, unsigned x2, unsigned x3)
{
	unsigned* p;
	p = mem + (sp[height] * CELL_SIZE);
	p[0] = app(x1, x2);
	sp[height - 1] = p[0];
	p[1] = x3;
	sp = sp + (height * CELL_SIZE) - (2 * CELL_SIZE);
	sp[0] = x1;
	return 0;
}

unsigned apparg(unsigned i, unsigned j)
{
	return app(arg(i), arg(j));
}

void foreign(FUNCTION get, FUNCTION put, unsigned n)
{
	if(!rts_c)
	{
		if(1 == n)
		{
			fputc(num(2), stdout);
			lazy(4, app(arg(4), 'K'), arg(3));
		}
		return;
	}

	if(1 == n)
	{
		lazy(4, app(arg(4), app('#', put(num(2)))), arg(3));
	}
	else if(0 == n)
	{
		lazy(3, app(arg(3), app('#', get(0))), arg(2));
	}
}

void run(FUNCTION get, FUNCTION put)
{
	unsigned c;
	unsigned x;
	unsigned mnt;
	unsigned m;
	unsigned n;
	unsigned t;
	unsigned f;

	while(TRUE)
	{
		/* int ctr; if (++ctr == (1<<25)) stats(), ctr = 0; */
		/* int gctr; if ((*sp == 'Y' || *sp == 'S') && ++gctr == (1<<20)) gc(), gctr = 0; */
		if(mem + (hp * CELL_SIZE) > sp - (8 * CELL_SIZE))
		{
			gc();
		}

		x = sp[0];

		if(isAddr(x))
		{
			sp = sp - CELL_SIZE;
			sp[0] = mem[x];
		}
		else if(FORWARD == x)
		{
			if(rts_c) return;
			stats();
			require(FALSE, "error: stray forwarding pointer\n");
		}
		else if('Q' == x)
		{
			lazy(3, arg(3), apparg(2, 1));
		}
		else if('.' == x)
		{
			return;
		}
		/* fix */
		/* Y f = let x = f x in x */
		else if('Y' == x)
		{
			lazy(1, arg(1), sp[1]);
		}
		/* ap */
		/* S x y z = x z (y z) */
		else if('S' == x)
		{
			if(rts_c) lazy3(3, arg(1), arg(3), apparg(2, 3));
			else lazy(3, apparg(1, 3), apparg(2, 3));
		}
		/* (.) */
		/* B x y z = x (y z) */
		else if('B' == x)
		{
			lazy(3, arg(1), apparg(2, 3));
		}
		/* flip */
		/* C x y z = x z y */
		else if('C' == x)
		{
			if(rts_c) lazy3(3, arg(1), arg(3), arg(2));
			else lazy(3, apparg(1, 3), arg(2));
		}
		/* flip flip */
		/* R x y z = y z x */
		else if('R' == x)
		{
			if(rts_c) lazy3(3, arg(2), arg(3), arg(1));
			else lazy(3, apparg(2, 3), arg(1));
		}
		else if('V' == x)
		{
			if(rts_c) lazy3(3, arg(3), arg(1), arg(2));
			else lazy(3, apparg(3,1), arg(2));
		}
		/* id */
		/* I x = x */
		else if('I' == x)
		{
			sp[1] = arg(1);
			sp = sp + CELL_SIZE;
		}
		/* (&) */
		/* T x y = y x */
		else if('T' == x)
		{
			lazy(2, arg(2), arg(1));
		}
		/* const */
		/* K x y = x */
		else if('K' == x)
		{
			lazy(2, 'I', arg(1));
		}
		/* cons */
		/* : a b c d = (d a) b */
		else if(':' == x)
		{
			if(rts_c) lazy3(4, arg(4), arg(1), arg(2));
			else lazy(4, apparg(4, 1), arg(2));
		}
		/* Read a character c from the input */
		/* If c == 0, then I K (represents nil) */
		/* else : (# c) (0 ?)  (represents a list of the first */
		/*                      character and the rest of the input) */
		else if(!rts_c && ('0' == x))
		{
			c = get(0);

			if(0 == c)
			{
				lazy(1, 'I', 'K');
			}
			else
			{
				lazy(1, app(':', app('#', c)), app('0', '?'));
			}
		}
		/* numeric combinator */
		/* # n f = f (# n) */
		else if('#' == x)
		{
			lazy(2, arg(2), sp[1]);
		}
		else if(!rts_c && ('1' == x))
		{
			put(num(1));
			lazy(2, app(arg(2), '.'), app('T', '1'));
		}
		else if('=' == x)
		{
			if(num(1) == num(2))
			{
				lazy(2, 'I', 'K');
			}
			else
			{
				lazy(2, 'K', 'I');
			}
		}
		else if('L' == x)
		{
			if (num(1) == -1)
			{
				lazy(2, 'K', 'I');
			}
			else if(num(1) <= num(2))
			{
				lazy(2, 'I', 'K');
			}
			else
			{
				lazy(2, 'K', 'I');
			}
		}
		else if('*' == x)
		{
			lazy(2, '#', num(1) * num(2));
		}
		else if('/' == x)
		{
			lazy(2, '#', num(1) / num(2));
		}
		else if('%' == x)
		{
			lazy(2, '#', num(1) % num(2));
		}
		else if('+' == x)
		{
			lazy(2, '#', num(1) + num(2));
		}
		else if('-' == x)
		{
			lazy(2, '#', num(1) - num(2));
		}
		else if(!rts_c && ('a' == x))
		{
			mnt = arg(1);
			m = mnt >> 16;
			n = (mnt >> 8) & 255;
			t = mnt & 255;
			sp = sp + (2 * CELL_SIZE);
			f = arg(m);

			while(n)
			{
				sp = sp + CELL_SIZE;
				f = app(f, mem[(sp[1])]);
				n = n - 1;
			}

			sp = sp + (t * CELL_SIZE);
			mem[sp[0]] = 'I';
			mem[sp[1]] = f;
		}
		else if('F' == x)
		{
			foreign(get, put, arg(1));
		}
		else
		{
			file_print("?", stderr);
			file_print(numerate_number(x),stderr);
			file_print("\n",stderr);
			require(FALSE, "error: unknown combinator\n");
		}
	}
}

char* buf;
char* bufptr;
char* buf_end;

void load(char* blob)
{
	FILE* f = fopen(blob, "r");
	require(NULL != f, "failed to load file\n");

	int i = 0;
	int c;
	do
	{
		c = fgetc(f);
		if(EOF != c) buf[i] = c;
		i = i + 1;
	} while(EOF != c);

	fclose(f);
}

unsigned buf_put(unsigned c)
{
	require(bufptr != buf_end, "error: buffer overflow\n");

	bufptr[0] = c;
	bufptr = bufptr + 1;
	return 0;
}

FILE *fp;
void fp_reset(char *f)
{
	fp = fopen(f, "r");
	require(fp != NULL, "error: fopen failed\n");
}


unsigned fp_get(unsigned f)
{
	failure = f;
	int fp_c = fgetc(fp);

	if(fp_c == EOF)
	{
		fclose(fp);
		return 0;
	}

	return fp_c;
}


unsigned pc(unsigned c)
{
	fputc(c, destination_file);
	/* fflush(destination_file); */
	return 0;
}

unsigned ioget(unsigned f)
{
	failure = f;
	int c = fgetc(input_file);

	return c;
}

void lvlup_file(char *filename, int raw)
{
	if(raw)
	{
		str = buf;
		loadRaw(str_get);
	}
	else parse(buf);
	fp_reset(filename);
	bufptr = buf;
	run(fp_get, buf_put);
	bufptr[0] = 0;
}

unsigned rts_reduce(unsigned n)
{
	sp = spTop;
	spTop[0] = app(app(n, '?'), '.');
	bufptr = buf;
	run(ioget, buf_put);
	return 0;
}

void rts_init(FUNCTION get)
{
	hp = 128;
	unsigned c;
	unsigned n = 0;
	unsigned i = 0;

	while(TRUE)
	{
		c = get(0);
		if (c == 0 || (c < '0' || c > '9'))
			break;
		n = 10 * n + c - '0';
	}

	starting_address = n;

	while(TRUE)
	{
		do
		{
			c = get(0);
			i = i + 1;
		} while(c != 0 && (c < '0' || c > '9'));

		if(c == 0)
		{
			break;
		}

		n = 0;

		while(TRUE)
		{
			if(c < '0' || c > '9')
			{
				break;
			}

			n = 10 * n + c - '0';
			c = get(0);
			i = i + 1;
		}

		mem[hp] = n;
		hp = hp + 1;
	}

	spTop = mem + (TOP * CELL_SIZE) - CELL_SIZE;
}

void iotest()
{
	str =
	    "ioBind2 m k = ioBind m (\\_ -> k);"
	    "flst xs n c = case xs of { [] -> n; (:) h t -> c h t };"
	    "foldr c n l = flst l n (\\h t -> c h(foldr c n t));"
	    "(.) f g x = f (g x);"
	    "data Unit = Unit;"
	    "flip f x y = f y x;"
	    "map = flip (foldr . ((:) .)) [];"
	    "mapM_ f = foldr (ioBind2 . f) (ioPure Unit);"
	    "main = mapM_ putChar \"Hello, World!\\n\""
	    ;
	bufptr = buf;
	run(str_get, buf_put);
	bufptr[0] = 0;
	str = buf;
	loadRaw(str_get);
	sp[0] = app(app(root_memo, '?'), '.');
	str = "";
	run(str_get, pc);
}

int main(int argc, char **argv)
{
	mem = malloc(TOP * sizeof(unsigned));
	altmem = malloc(TOP * sizeof(unsigned));
	buf_end = buf + BUFMAX;
	spTop = mem + (TOP * CELL_SIZE) - CELL_SIZE;
	tab = calloc(TABMAX, sizeof(unsigned));
	buf = calloc(BUFMAX, sizeof(char));
	destination_file = stdout;
	input_file = stdin;
	rts_c = FALSE;
	root_size = 0;

	int option_index = 1;
	while(option_index <= argc)
	{
		if(NULL == argv[option_index])
		{
			option_index = option_index + 1;
		}
		else if(match(argv[option_index], "-l") || match(argv[option_index], "--load-rom"))
		{
			fp_reset(argv[option_index+1]);
			loadRaw(fp_get);
			option_index = option_index + 2;
		}
		else if(match(argv[option_index], "run"))
		{
			fp_reset(argv[option_index+1]);
			bufptr = buf;
			run(fp_get, buf_put);
			bufptr[0] = 0;
			str = buf;
			loadRaw(str_get);
			bufptr = buf;
			run(ioget, buf_put);
			option_index = option_index + 2;
		}
		else if(match(argv[option_index], "--rts_c"))
		{
			rts_c = TRUE;
			load(argv[option_index+1]);
			str = buf;
			rts_init(str_get);
			rts_reduce(starting_address);
			option_index = option_index + 2;
		}
		else if(match(argv[option_index], "--bootstrap"))
		{
			buf[0] = 'I';
			buf[1] = ';';
			bufptr = buf + 2;
			option_index = option_index + 1;
		}
		else if(match(argv[option_index], "-lf") || match(argv[option_index], "--levelup-file"))
		{
			file_print("loading ", stderr);
			file_print(argv[option_index + 1], stderr);
			file_print("...\n", stderr);
			lvlup_file(argv[option_index + 1], FALSE);
			option_index = option_index + 2;
		}
		else if(match(argv[option_index], "-lfr") || match(argv[option_index], "--levelup-file-raw"))
		{
			lvlup_file(argv[option_index + 1], TRUE);
			option_index = option_index + 2;
		}
		else if(match(argv[option_index], "-o") || match(argv[option_index], "--output"))
		{
			destination_file = fopen(argv[option_index + 1], "w");

			if(NULL == destination_file)
			{
				file_print("The file: ", stderr);
				file_print(argv[option_index + 1], stderr);
				file_print(" can not be opened!\n", stderr);
				exit(EXIT_FAILURE);
			}
			option_index = option_index + 2;
		}
		else if(match(argv[option_index], "-f") || match(argv[option_index], "--file"))
		{
			input_file = fopen(argv[option_index + 1], "r");
			if(NULL == input_file)
			{
				file_print("The file: ", stderr);
				file_print(argv[option_index + 1], stderr);
				file_print(" can not be opened!\n", stderr);
				exit(EXIT_FAILURE);
			}
			option_index = option_index + 2;
		}
		else
		{
			file_print("bad command: ", stdout);
			file_print(argv[option_index], stdout);
			file_print("\n", stdout);
			exit(EXIT_FAILURE);
		}
	}

	file_print(buf, destination_file);
	file_print("\n", destination_file);
	return EXIT_SUCCESS;
}
