/*
 * Copyright © 2019 Ben Lynn
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
void *malloc(unsigned long);

unsigned prog[];
unsigned prog_size;
unsigned root[];
unsigned root_size;

// CONSTANT FORWARD 27
#define FORWARD 27
// CONSTANT REDUCING 9
#define REDUCING 9

// CONSTANT TOP 16777216
#define TOP 16777216

unsigned* mem;
unsigned* altmem;
unsigned* sp;
unsigned* spTop;
unsigned hp;

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

	switch(x)
	{
		case FORWARD:
			return y;

		case REDUCING:
			mem[n] = FORWARD;
			mem[n + 1] = hp;
			hp += 2;
			return mem[n + 1];

		case 'I':
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

		default:
			break;
	}

	unsigned z = hp;
	hp += 2;
	mem[n] = FORWARD;
	mem[n + 1] = z;
	altmem[z] = x;
	altmem[z + 1] = y;
	return z;
}

static void gc()
{
	hp = 128;
	unsigned di = hp;
	sp = altmem + TOP - 1;

	for(unsigned i = 0; i < root_size; i++)
	{
		root[i] = evac(root[i]);
	}

	*sp = evac(*spTop);

	while(di < hp)
	{
		unsigned x = altmem[di] = evac(altmem[di]);
		di++;

		if(x != 'F' && x != '#')
		{
			altmem[di] = evac(altmem[di]);
		}

		di++;
	}

	spTop = sp;
	unsigned *tmp = mem;
	mem = altmem;
	altmem = tmp;
}

unsigned app(unsigned f, unsigned x)
{
	mem[hp] = f;
	mem[hp + 1] = x;
	hp += 2;
	return hp - 2;
}

unsigned arg(unsigned n)
{
	return mem[sp [n] + 1];
}

unsigned num(unsigned n)
{
	return mem[arg(n) + 1];
}

unsigned lazy(unsigned height, unsigned f, unsigned x)
{
	unsigned *p = mem + sp[height];
	*p = f;
	*++p = x;
	sp += height - 1;
	*sp = f;
	return 0;
}

unsigned lazy3(unsigned height, unsigned x1, unsigned x2, unsigned x3)
{
	unsigned *p = mem + sp[height];
	sp[height - 1] = *p = app(x1, x2);
	*++p = x3;
	*(sp += height - 2) = x1;
	return 0;
}

unsigned apparg(unsigned i, unsigned j)
{
	return app(arg(i), arg(j));
}

unsigned foreign(unsigned n);

unsigned run()
{
	for(;;)
	{
		if(mem + hp > sp - 8)
		{
			gc();
		}

		unsigned x = *sp;

		if(isAddr(x))
		{
			*--sp = mem[x];
		}
		else switch(x)
			{
				case 'F':
					foreign(arg(1));
					break;

				case 'Y':
					lazy(1, arg(1), sp[1]);
					break;

				case 'Q':
					lazy(3, arg(3), apparg(2, 1));
					break;

				case 'S':
					lazy3(3, arg(1), arg(3), apparg(2, 3));
					break;

				case 'B':
					lazy(3, arg(1), apparg(2, 3));
					break;

				case 'C':
					lazy3(3, arg(1), arg(3), arg(2));
					break;

				case 'R':
					lazy3(3, arg(2), arg(3), arg(1));
					break;

				case 'V':
					lazy3(3, arg(3), arg(1), arg(2));
					break;

				case 'I':
					sp[1] = arg(1);
					sp++;
					break;

				case 'T':
					lazy(2, arg(2), arg(1));
					break;

				case 'K':
					lazy(2, 'I', arg(1));
					break;

				case ':':
					lazy3(4, arg(4), arg(1), arg(2));
					break;

				case '#':
					lazy(2, arg(2), sp[1]);
					break;

				case '=':
					num(1) == num(2) ? lazy(2, 'I', 'K') : lazy(2, 'K', 'I');
					break;

				case 'L':
					num(1) <= num(2) ? lazy(2, 'I', 'K') : lazy(2, 'K', 'I');
					break;

				case '*':
					lazy(2, '#', num(1) * num(2));
					break;

				case '/':
					lazy(2, '#', num(1) / num(2));
					break;

				case '%':
					lazy(2, '#', num(1) % num(2));
					break;

				case '+':
					lazy(2, '#', num(1) + num(2));
					break;

				case '-':
					lazy(2, '#', num(1) - num(2));
					break;

				case '.':
					return 0;

				case FORWARD:
					return 0;  // die("stray forwarding pointer");

				default:
					return 0;  // printf("?%u\n", x); die("unknown combinator");
			}
	}
}

unsigned rts_reduce(unsigned n)
{
	*(sp = spTop) = app(app(n, '?'), '.');
	run();
	return 0;
}

unsigned rts_init()
{
	mem = malloc(TOP * sizeof(unsigned));
	altmem = malloc(TOP * sizeof(unsigned));
	hp = 128;

	for(unsigned i = 0; i < prog_size; i++)
	{
		mem[hp++] = prog[i];
	}

	spTop = mem + TOP - 1;
}

int env_argc;
int getargcount()
{
	return env_argc;
}
char **env_argv;
char getargchar(int n, int k)
{
	return env_argv[n][k];
}

#define EXPORT(f, sym, n) void f() asm(sym) __attribute__((visibility("default"))); void f(){rts_reduce(root[n]);}
