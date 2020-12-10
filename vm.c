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
int numerate_string(char* a);
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
unsigned foreign_version;

/* Trying to get vm.c and rts.c syncronized */
unsigned root_size;
unsigned* root;
int rts_c;
unsigned starting_address;

/* Stupid global to shutup warnings; should be removed when readers are unified */
unsigned failure;

/* Our annoying buffer */
char* buf;
char* bufptr;
char* buf_end;


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

void load(char* filename)
{
	FILE* fp = fopen(filename, "r");
	require(fp != NULL, "error: fopen failed\n");

	int c = fgetc(fp);
	int i = 0;
	while(EOF != c)
	{
		buf[i] = c;
		i = i + 1;
		c = fgetc(fp);
	}

	fclose(fp);
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
		c = get(0);
		require(' ' <= c, "parseTerm tab underflow\n");
		return tab[c - ' '];
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

		require(n < TABMAX, "error: table overflow\n");
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
			require(1 <= tabn, "parseMore tabn underflow\n");
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

void foreign2(FUNCTION get, FUNCTION put, unsigned n)
{
	if(3 == n)
	{
		lazy(4, app(arg(4), app('#', put(num(2)))), arg(3));
	}
	else if (2 == n)
	{
		lazy(3, app(arg(3), app('#', get(0))), arg(2));
	}
	else if (1 == n)
	{
		/* lazy(3, app(arg(3), app('#', getargcount())), arg(2)); */
		lazy(3, app(arg(3), app('#', 1)), arg(2));
	}
	else if (0 == n)
	{
		lazy(5, app(arg(5), app('#', 0)), arg(4));
	}
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
			require(FALSE, "error: stray forwarding pointer\n");
		}
		else if('Q' == x)
		{
			lazy(3, arg(3), app(arg(2), arg(1)));
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
			if(rts_c) lazy3(3, arg(1), arg(3), app(arg(2), arg(3)));
			else lazy(3, app(arg(1), arg(3)), app(arg(2), arg(3)));
		}
		/* (.) */
		/* B x y z = x (y z) */
		else if('B' == x)
		{
			lazy(3, arg(1), app(arg(2), arg(3)));
		}
		/* flip */
		/* C x y z = x z y */
		else if('C' == x)
		{
			if(rts_c) lazy3(3, arg(1), arg(3), arg(2));
			else lazy(3, app(arg(1), arg(3)), arg(2));
		}
		/* flip flip */
		/* R x y z = y z x */
		else if('R' == x)
		{
			if(rts_c) lazy3(3, arg(2), arg(3), arg(1));
			else lazy(3, app(arg(2), arg(3)), arg(1));
		}
		else if('V' == x)
		{
			if(rts_c) lazy3(3, arg(3), arg(1), arg(2));
			else lazy(3, app(arg(3),arg(1)), arg(2));
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
			else lazy(4, app(arg(4), arg(1)), arg(2));
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
			if (foreign_version == 1)
				foreign(get, put, arg(1));
			else if (foreign_version == 2)
				foreign2(get, put, arg(1));
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

unsigned buf_put(unsigned c)
{
	require(bufptr != buf_end, "error: buffer overflow\n");

	bufptr[0] = c;
	bufptr = bufptr + 1;
	return 0;
}

FILE *fp;


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

void rts_init()
{
	hp = 128;
	unsigned c;
	unsigned n = 0;
	unsigned i = 0;

	while(TRUE)
	{
		c = str_get(0);
		if (c == 0 || (c < '0' || c > '9'))
			break;
		n = 10 * n + c - '0';
	}

	starting_address = n;

	while(TRUE)
	{
		do
		{
			c = str_get(0);
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
			c = str_get(0);
			i = i + 1;
		}

		mem[hp] = n;
		hp = hp + 1;
	}

	spTop = mem + (TOP * CELL_SIZE) - CELL_SIZE;
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
	foreign_version = 1;
	rts_c = FALSE;
	root_size = 0;

	int option_index = 1;
	while(option_index <= argc)
	{
		if(NULL == argv[option_index])
		{
			option_index = option_index + 1;
		}
		else if(match(argv[option_index], "--redo"))
		{
			str = buf;
			loadRaw(str_get);
			option_index = option_index + 1;
		}
		else if(match(argv[option_index], "-l") || match(argv[option_index], "--load-rom"))
		{
			fp = fopen(argv[option_index + 1], "r");
			require(fp != NULL, "error: fopen failed\n");
			loadRaw(fp_get);
			option_index = option_index + 2;
		}
		else if(match(argv[option_index], "run"))
		{
			bufptr = buf;
			run(ioget, buf_put);
			bufptr[0] = 0;
			option_index = option_index + 1;
		}
		else if(match(argv[option_index], "--rts_c"))
		{
			rts_c = TRUE;
			str = buf;
			rts_init();
			sp = spTop;
			spTop[0] = app(app(starting_address, '?'), '.');
			option_index = option_index + 1;
		}
		else if(match(argv[option_index], "-pb") || match(argv[option_index], "--parse-buffer"))
		{
			file_print("parsing buffered ", stderr);
			file_print(argv[option_index + 1], stderr);
			file_print("...\n", stderr);
			hp = 128;
			tabn = 0;
			str = buf;
			parseMore(str_get);
			option_index = option_index + 2;
		}
		else if(match(argv[option_index], "-lf") || match(argv[option_index], "--levelup-file"))
		{
			fp = fopen(argv[option_index + 1], "r");
			require(fp != NULL, "error: fopen failed\n");
			bufptr = buf;
			run(fp_get, buf_put);
			bufptr[0] = 0;
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
		else if(match(argv[option_index], "--foreign"))
		{
			foreign_version = numerate_string(argv[option_index + 1]);
			option_index = option_index + 2;
		}
		else if(match(argv[option_index], "--raw"))
		{
			load(argv[option_index + 1]);
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
	return EXIT_SUCCESS;
}
