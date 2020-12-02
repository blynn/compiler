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

//CONSTANT TOP    8388608
#define TOP       8388608
//CONSTANT TABMAX 1024
#define TABMAX    1024
//CONSTANT BUFMAX 1048576
#define BUFMAX    1048576

//CONSTANT CELL_SIZE sizeof(unsigned)
#define CELL_SIZE 1

int match(char* a, char* b);
void file_print(char* s, FILE* f);
char* numerate_number(int a);

void die(char *s)
{
	file_print("error: ", stderr);
	file_print(s, stderr);
	file_print(" \n", stderr);
	exit(EXIT_FAILURE);
}

unsigned* mem;
unsigned* altmem;
unsigned* sp;
unsigned* spTop;
unsigned hp;
unsigned* tab;
unsigned tabn;

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

unsigned gccount;

/* Garbage collection */
void gc()
{
	gccount = gccount + 1;
	/* Reset the heap pointer */
	hp = 128;
	unsigned di = hp;
	/* Set the stack pointer to point to the top of altmem */
	sp = altmem + (TOP * CELL_SIZE) - CELL_SIZE;
	sp[0] = evac(spTop[0]);

	/* fprintf(stderr, "GC %u\n", hp - 128); */
	unsigned x;
	while(di < hp)
	{
		x = altmem[di];
		altmem[di] = evac(altmem[di]);
		di = di + 1;

		if(x != 'a' && x != '#')
		{
			altmem[di] = evac(altmem[di]);
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

	reset(mem[128 - 1]);
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

		if(tabn == TABMAX)
		{
			die("table overflow");
		}

		tab[tabn] = c;
		tabn = tabn + 1;

		if(get(0) != ';')
		{
			die("expected ';'");
		}
	}
}

char *str;

unsigned str_get(unsigned f)
{
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

void parseRaw(char *s)
{
	str = s;
	loadRaw(str_get);
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

void lazy(unsigned height, unsigned f, unsigned x)
{
	unsigned* p;
	p = mem + (sp[height] * CELL_SIZE);
	p[0] = f;
	p = p + CELL_SIZE;
	p[0] = x;
	sp = sp + (height * CELL_SIZE) - CELL_SIZE;
	sp[0] = f;
}

unsigned apparg(unsigned i, unsigned j)
{
	return app(arg(i), arg(j));
}

void foreign(unsigned n)
{
	if(1 == n)
	{
		fputc(num(2), stdout);
		lazy(4, app(arg(4), 'K'), arg(3));
	}
}

void run(FUNCTION get, FUNCTION put)
{
	gccount = 0;
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
			stats();
			die("stray forwarding pointer");
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
			lazy(3, apparg(1, 3), apparg(2, 3));
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
			lazy(3, apparg(1, 3), arg(2));
		}
		/* flip flip */
		/* R x y z = y z x */
		else if('R' == x)
		{
			lazy(3, apparg(2, 3), arg(1));
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
			lazy(4, apparg(4, 1), arg(2));
		}
		/* Read a character c from the input */
		/* If c == 0, then I K (represents nil) */
		/* else : (# c) (0 ?)  (represents a list of the first */
		/*                      character and the rest of the input) */
		else if('0' == x)
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
		else if('1' == x)
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
			if(num(1) <= num(2))
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
		else if('a' == x)
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
			foreign(arg(1));
		}
		else
		{
			file_print("?", stderr);
			fputc(x,stderr);
			file_print("\n",stderr);
			die("unknown combinator");
		}
	}
}

char* buf;
char* bufptr;
char* buf_end;

void buf_reset()
{
	bufptr = buf;
}

unsigned buf_put(unsigned c)
{
	if(bufptr == buf_end)
	{
		die("buffer overflow");
	}

	bufptr[0] = c;
	bufptr = bufptr + 1;
	return 0;
}

void testCmp(char *inp, char *want)
{
	str = inp;
	buf_reset();
	run(str_get, buf_put);
	bufptr[0] = 0;

	if(!match(buf, want))
	{
		file_print("FAIL: got '",stderr);
		file_print(buf,stderr);
		file_print("', want '",stderr);
		file_print(want,stderr);
		file_print("'\n",stderr);
	}
}

void testCase(char *prog, char *inp, char *want)
{
	parse(prog);
	testCmp(inp, want);
}

void testCaseMore(char *prog, char *more, char *inp, char *want)
{
	parse(prog);
	str = more;
	parseMore(str_get);
	testCmp(inp, want);
}


/* Big ugly RAW data */
char* parenthetically;
char* exponentially;
char* practically;
char* singularity;

void setup_raws()
{
	parenthetically =
    /*
    uncurry x y = y x;
    (.) x y z = x (y z);

    pair x y f = f x y;
    (||) f g x y = f x (g x y);
    (++) xs ys = xs ys (\x xt -> x : (xt ++ ys));
    ifNull xs a b = xs a (\_ _ -> b);
    add r acc p = r (ifNull acc p ('`':(acc ++ p)));
    isPre h = h('#'(==)) || h('@'(==));
    suffix f h t = isPre h (t undefined (\a b -> pair (a:[]) b)) (pair [] t) (\x y -> f (h:x) y);
    atom r h acc t = suffix (add r acc) h t;
    sub r acc = uncurry (add r acc) . r "";
    closes h = h(';'(==)) || h(')'(==));
    if3 h x y z = closes h x (h('('(==)) y z);
    switch r a h t = if3 h pair (sub r) (atom r h) a t;
    term acc s = s undefined (\h t -> switch term acc h t);
    parse s = s "" (\_ _ -> term "" s (\p t -> p ++ (';':parse t)));
    */
    "``BCT;"
    "``BS`BB;"
    "`Y``B`CS``B`B`C``BB:C;"
    "``B`R``BKK`BB;"
    "``C``BBB``S``BS@#``B`B`:#`@\";"
    "``S``B@!`T`##=`T`#@=;"
    "``B`S``BC``C``BS``C``BB@%``C`T?``B@ ``C:K`@ K``C``BBB:;"
    "``BC``B`B@&@$;"
    "``S``BC``B`BB``B`BT@$`TK;"
    "``S``B@!`T`#;=`T`#)=;"
    "``S``BC``B`BB``B`BB@)`T`#(=;"
    "``BC``S``BS``B`C``C@*@ @(@';"
    "`Y``B`B`C`T?@+;"
    "`Y``B`S`TK``B`BK``B`BK``B`C`@,K``B`C``BB@\"`B`:#;;"
    ;

	exponentially =
    /*
    id x = x;
    const x _ = x;
    (&) x f = f x;
    flip f x y = f y x;
    fix x = x (fix x);
    Nothing x _ = x;
    Just x f g = g x;
    P x y f = f x y;
    (||) f g x y = f x (g x y);
    (++) xs ys = xs ys (\x xt -> x : (xt ++ ys));
    pure x inp = Just (P x inp);
    bind f m = m Nothing (\x -> x f);
    (<*>) x y = \inp -> bind (\a t -> bind (\b u -> pure (a b) u) (y t)) (x inp);
    (<$>) f x = pure f <*> x;
    (*>) p q = (\_ x -> x) <$> p <*> q;
    (<*) p q = (\x _ -> x) <$> p <*> q;
    (<|>) x y = \inp -> (x inp) (y inp) Just;
    R s   = \a b c d -> a s;
    V v   = \a b c d -> b v;
    A x y = \a b c d -> c x y;
    L x y = \a b c d -> d x y;
    sat f inp = inp Nothing (\h t -> f h (pure h t) Nothing);
    char c = sat (\x -> x(c(==)));
    var = sat (\c -> flip (c(';'(==)) || c(')'(==))));
    pre = (:) <$> (char '#' <|> char '@') <*> (flip (:) const <$> sat (const const));
    atom r = (char '(' *> (r <* char ')')) <|> (char '\\' *> (L <$> var) <*> (char '.' *> r)) <|> (R <$> pre) <|> (V <$> var);
    apps r = (((&) <$> atom r) <*> ((\vs v x -> vs (A x v)) <$> apps r)) <|> pure id;
    expr = ((&) <$> atom expr) <*> apps expr;
    show t = t id (\v -> v:[])(\x y -> '`':(show x ++ show y)) undefined;
    unlam v = fix (\r t -> t (\x -> A (V 'K') (R x)) (\x -> x(v(==)) (V 'I') (A (V 'K') (V x))) (\x y -> A (A (V 'S') (r x)) (r y)) undefined);
    babs t = t R V (\x y -> A (babs x) (babs y)) (\x y -> unlam x (babs y));
    main s = (expr <* char ';') s "" (\p -> p (\x t -> show (babs x) ++ ";" ++ main t)));
    */
    "BKT;"
    "BCT;"
    "BS(BB);"
    "Y(B(CS)(B(B(C(BB:)))C));"
    "B(B@ )@!;"
    "B(C(TK))T;"
    "C(BB(B@%(C(BB(B@%(B@$))))));"
    "B@&@$;"
    "B@&(@'(KI));"
    "B@&(@'K);"
    "B(B(R@ ))S;"
    "B(BK)(B(BK)(B(BK)T));"
    "BK(B(BK)(B(BK)T));"
    "B(BK)(B(BK)(B(B(BK))(BCT)));"
    "B(BK)(B(BK)(B(BK)(BCT)));"
    "B(C(TK))(B(B(RK))(C(BS(BB))@$));"
    "B@/(BT(T=));"
    "@/(BC(S(B@\"(T(#;=)))(T(#)=))));"
    "@&(@':(@*(@0##)(@0#@)))(@'(C:K)(@/(KK)));"
    "C(B@*(C(B@*(S(B@*(B(@((@0#())(C@)(@0#)))))(B(@&(@((@0#\\)(@'@.@1)))(@((@0#.)))))(@'@+@2)))(@'@,@1);"
    "Y(B(R(@$I))(B(B@*)(B(S(B@&(B(@'T)@3)))(B(@'(C(BBB)(C@-)))))));"
    "Y(S(B@&(B(@'T)@3))@4);"
    "Y(B(R?)(B(C(C(TI)(C:K)))(B(B(B(:#`)))(S(BC(B(BB)(B@#)))I))));"
    "BY(B(B(R?))(C(BB(BC(B(C(T(B(@-(@,#K))@+)))(C(BS(B(R(@,#I))(BT(T=))))(B(@-(@,#K))@,)))))(S(BC(B(BB)(B(B@-)(B(@-(@,#S))))))I)));"
    "Y(S(BC(B(C(C(T@+)@,))(S(BC(B(BB)(B@-)))I)))(C(BB@7)));"
    "Y(B(C(C(@)@5(@0#;))K))(BT(C(BB(B@#(C(B@#(B@6@8))(:#;K)))))));"
    ;

	practically =
    /* Same as above, except: */
    /*
    occurs v t = t (\x -> (\_ y -> y)) (\x -> x(v(==))) (\x y -> occurs v x || occurs v y) undefined;
    unlam v t = occurs v t (t undefined (const (V 'I')) (\x y -> A (A (V 'S') (unlam v x)) (unlam v y)) undefined) (A (V 'K') t);
    */
    "BKT;"
    "BCT;"
    "BS(BB);"
    "Y(B(CS)(B(B(C(BB:)))C));"
    "B(B@ )@!;"
    "B(C(TK))T;"
    "C(BB(B@%(C(BB(B@%(B@$))))));"
    "B@&@$;"
    "B@&(@'(KI));"
    "B@&(@'K);"
    "B(B(R@ ))S;"
    "B(BK)(B(BK)(B(BK)T));"
    "BK(B(BK)(B(BK)T));"
    "B(BK)(B(BK)(B(B(BK))(BCT)));"
    "B(BK)(B(BK)(B(BK)(BCT)));"
    "B(C(TK))(B(B(RK))(C(BS(BB))@$));"
    "B@/(BT(T=));"
    "@/(BC(S(B@\"(T(#;=)))(T(#)=))));"
    "@&(@':(@*(@0##)(@0#@)))(@'(C:K)(@/(KK)));"
    "C(B@*(C(B@*(S(B@*(B(@((@0#())(C@)(@0#)))))(B(@&(@((@0#\\)(@'@.@1)))(@((@0#.)))))(@'@+@2)))(@'@,@1);"
    "Y(B(R(@$I))(B(B@*)(B(S(B@&(B(@'T)@3)))(B(@'(C(BBB)(C@-)))))));"
    "Y(S(B@&(B(@'T)@3))@4);"
    "Y(B(R?)(B(C(C(TI)(C:K)))(B(B(B(:#`)))(S(BC(B(BB)(B@#)))I))));"
    "Y\\a.\\b.\\c.c(\\d.KI)(\\d.d(b=))(\\d.\\e.@\"(abd)(abe))?;"
    "Y\\a.\\b.\\c.@7bc(c?(K(@,#I))(\\d.\\e.@-(@-(@,#S)(abd))(abe))?)(@-(@,#K)c);"
    "Y(S(BC(B(C(C(T@+)@,))(S(BC(B(BB)(B@-)))I)))(C(BB@8)));"
    "Y(B(C(C(@)@5(@0#;))K))(BT(C(BB(B@#(C(B@#(B@6@9))(:#;K)))))));"
    ;

	singularity =
    "\\a.\\b.\\c.\\d.ac(bcd);"
    "Y\\a.\\b.\\c.\\d.\\e.b(cd\\f.\\g.e)\\f.\\g.ce\\h.\\i.f(h=)(agide)e;"
    "Y\\a.\\b.\\c.bc\\d.\\e.:d(aec);"
    "\\a.\\b.\\c.cab;"
    "\\a.\\b.\\c.ca;"
    "\\a.\\b.@$(@#ab);"
    "\\a.\\b.bK\\c.\\d.ac(@%cd)K;"
    "\\a.\\b.bK\\c.ca;"
    "\\a.\\b.\\c.@'(\\d.\\e.@'(\\f.\\g.@%(df)g)(be))(ac);"
    "\\a.\\b.@((@%a)b;"
    "\\a.\\b.\\c.ac(bc)@$;"
    "Y\\a.\\b.\\c.\\d.dc\\e.\\f.be(abcf);"
    "\\a.\\b.\\c.@((@)ab)c;"
    "Y\\a.\\b.@*(@,:b(ab))(@%K);"
    "\\a.@,:a(@-a);"
    "\\a.@&\\b.b(a=);"
    "@,(KI);"
    "@,K;"
    "@0(@/#-)(@0(@/#-)(@0(@-(@&\\a.C(a(#\n=))))(@/#\n)));"
    "@-(@*(@&\\a.@ (a(# =))(a(#\n=)))@2);"
    "\\a.@1a@3;"
    "B@4@/;"
    "\\a.\\b.\\c.\\d.Cad(bcd);"
    "@4(@.(@&\\a.@6(#z(aL))(a(#aL))));"
    "\\a.\\b.\\c.\\d.\\e.ba;"
    "\\a.\\b.\\c.\\d.\\e.ca;"
    "\\a.\\b.\\c.\\d.\\e.\\f.eab;"
    "\\a.\\b.\\c.\\d.\\e.\\f.fab;"
    "@)(C:K)(@4(@&(KK)));"
    "@*(@0(@/#@)@<)(@,:(@/##)@<);"
    "\\a.@0(@5#\\)(@,(C(@+@;))(@.@7)(@0(@/#-)(@0(@5#>)a)));"
    "\\a.@*(@*(@*(@0(@5#()(@1a(@5#))))(@>a))(@)@8@=))(@)@9@7);"
    "Y\\a.\\b.@*(@,T(@?b)(@)(\\c.\\d.\\e.c(@:ed))(ab)))(@%I);"
    "Y\\a.@,T(@?a)(@@a);"
    "@,@#@7(@,(C(@+@;))(@-@7)(@0(@5#=)@A));"
    "@0@3(@.(@1@B(@5#;)));"
    "\\a.\\b.@+(\\c.\\d.@!a(cK)(\\e.:#@(:eK))(Bd\\e.# (#!-)(e+)))(Ka)b# ;"
    "Y\\a.\\b.\\c.cI(\\d.@Ddb)(\\d.\\e.:#`(@\"(abd)(abe)))?;"
    "Y\\a.\\b.\\c.c(\\d.KI)(\\d.@!bd)(\\d.\\e.@ (abd)(abe))\\d.\\e.C(@ (@!bd)(C(abe)));"
    "Y\\a.\\b.\\c.@Fbc(c?(K(@8(:#IK)))(\\d.\\e.@:(@:(@9(:#SK))(abd))(abe))?)(@:(@9(:#KK))c);"
    "Y\\a.\\b.b@8@9(\\c.\\d.@:(ac)(ad))\\c.\\d.@Gc(ad);"
    "Y\\a.\\b.\\c.cK\\d.\\e.@\"(@Eb(@H(d(KI))))(:#;(abe));"
    "\\a.@Ca(:#?K)(B(\\b.@Ibb)(TK));"
    ;
}

void runTests()
{
	testCase("`KK;", "ignoreme", "");
	testCase("I;", "Hello, World!\n", "Hello, World!\n");
	testCase("``C`T?`KI;", "tail", "ail");
	testCase("`K``:#O``:#KK;", "", "OK");
	/* xs ++ ys = case xs of { [] -> ys ; (x:xt) -> x : xt ++ ys } */
	/* f = fix \r xs ys -> xs ys (\x xt -> (:) x (r xt ys))        */
	/*   = Y(B(CS)(B(B(C(BB(:))))C))                               */
	/* because B(B(C(BB(:))))C = \r ys -> \x xt -> (:) x (r xt ys) */
	testCase(
	    "`Y``B`CS``B`B`C``BB:C;"  /* (++) */
	    "`K``@ " "``:#B``:#eK" "``:#nK;",
	    "",
	    "Ben");
	testCase(
	    "`Y``B`CS``B`B`C``BB:C;"  /* (++) */
	    "``SS``B`BK``B`BK``B`B`:#`@ ;"  /* \acc p = acc p (\_ _ -> '`':acc ++ p) */
	    "`@!``:#xK;",
	    "y",
	    "`xy");
	testCase(
	    "`Y``B`CS``B`B`C``BB:C;"  /* (++) */
	    "``SS``B`BK``B`BK``B`B`:#`[0];"  /* \acc p = acc p (\_ _ -> '`':acc ++ p) */
	    "`[1]``:#xK;",
	    "y",
	    "`xy");
	testCase("`K``:#f``:#xK;", "", "fx");
	/* atom id 'x' "f" */
	testCaseMore(parenthetically, "`K```@'I#x``:#fK;", "", "`fx");
	/* if3 ')' "1" "2" "3" */
	testCaseMore(parenthetically, "`K````@*#)``:#1K``:#2K``:#3K;", "", "1");
	/* fst . term */
	testCaseMore(parenthetically, "``B`TK`@,K;",
	             "just(one);not(two);", "````just``one");
	testCase(parenthetically, "par(en);(t(he)(ses));K;(I);", "```par`en;``t`he``ses;K;I;");
}

FILE *fp;
void fp_reset(char *f)
{
	fp = fopen(f, "r");

	if(fp == NULL)
	{
		die("fopen failed");
	}
}


unsigned fp_get(unsigned f)
{
	int fp_c = fgetc(fp);

	if(fp_c == EOF)
	{
		fclose(fp);
		return 0;
	}

	return fp_c;
}

char* iocccp;
void ioccc_reset(char *f)
{
	fp_reset(f);
	iocccp = "infixr 5 ++;(<=) = intLE;";
}

unsigned ioccc_get(unsigned f)
{
	if(0 == iocccp[0])
	{
		unsigned r = iocccp[0];
		iocccp = iocccp + 1;
		return r;
	}

	return fp_get(f);
}

unsigned pc(unsigned c)
{
	fputc(c, stdout);
	fflush(stdout);
	return 0;
}

unsigned ioget(unsigned f)
{
	int ioget_c = fgetc(stdin);

	if(ioget_c == EOF)
	{
		return 0;
	}

	return ioget_c;
}

void lvlup(char *prog)
{
	parse(buf);
	str = prog;
	buf_reset();
	run(str_get, buf_put);
	bufptr[0] = 0;
}

void lvlup_file(char *filename)
{
	file_print("loading ", stderr);
	file_print(filename, stderr);
	file_print("...\n", stderr);
	parse(buf);
	fp_reset(filename);
	buf_reset();
	run(fp_get, buf_put);
	bufptr[0] = 0;
}

void lvlup_file_raw(char *filename)
{
	file_print("loading ", stderr);
	file_print(filename, stderr);
	file_print("...\n", stderr);
	parseRaw(buf);
	fp_reset(filename);
	buf_reset();
	run(fp_get, buf_put);
	bufptr[0] = 0;
}

void rpg()
{
	buf[0] = 'I';
	buf[1] = ';';
	bufptr = buf + 2;
	lvlup(parenthetically);
	lvlup(exponentially);
	lvlup(practically);
	lvlup(singularity);
	lvlup_file("singularity");
	lvlup_file("semantically");
	lvlup_file("stringy");
	lvlup_file("binary");
	lvlup_file("algebraically");
	lvlup_file("parity.hs");
	lvlup_file("fixity.hs");
	lvlup_file("typically.hs");
	lvlup_file("classy.hs");
	lvlup_file("barely.hs");
	lvlup_file("barely.hs");
	lvlup_file_raw("barely.hs");
}

void dis(char *file)
{
	fp_reset("bin/raw");
	loadRaw(fp_get);
	fp_reset("disassembly.hs");
	buf_reset();
	run(fp_get, buf_put);
	parseRaw(buf);
	fp_reset(file);
	file_print("disassembling ", stderr);
	file_print(file, stderr);
	file_print("\n", stderr);
	run(fp_get, pc);
}

void runFile(char *f)
{
	fp_reset("bin/raw");
	loadRaw(fp_get);
	fp_reset(f);
	buf_reset();
	run(fp_get, buf_put);
	bufptr[0] = 0;
	parseRaw(buf);
	run(ioget, pc);
}

void ioccc(char *f)
{
	fp_reset("bin/raw");
	loadRaw(fp_get);
	ioccc_reset(f);
	buf_reset();
	run(ioccc_get, buf_put);
	bufptr[0] = 0;
	parseRaw(buf);
	run(ioget, pc);
}

void iotest()
{
	fp_reset("bin/raw");
	loadRaw(fp_get);
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
	buf_reset();
	run(str_get, buf_put);
	bufptr[0] = 0;
	parseRaw(buf);
	sp[0] = app(app(root_memo, '?'), '.');
	str = "";
	run(str_get, pc);
}

int main(int argc, char **argv)
{
	setup_raws();
	mem = malloc(TOP * sizeof(unsigned));
	altmem = malloc(TOP * sizeof(unsigned));
	buf_end = buf + BUFMAX;
	spTop = mem + (TOP * CELL_SIZE) - CELL_SIZE;
	tab = calloc(TABMAX, sizeof(unsigned));
	buf = calloc(BUFMAX, sizeof(char));

	if(argc > 1)
	{
		if(match(argv[1], "test"))
		{
			runTests();
			return 0;
		}

		if(match(argv[1], "iotest"))
		{
			iotest();
			return 0;
		}

		if(match(argv[1], "rawck"))
		{
			rpg();
			fp_reset("bin/raw");
			str = buf;
			unsigned c;

			while((c = str_get(0)))
			{
				if(c != fp_get(0))die("raw check failed!");
			}

			file_print("OK", stdout);
			return 0;
		}

		if(match(argv[1], "run"))
		{
			runFile(argv[2]);
			return 0;
		}

		if(match(argv[1], "ioccc"))
		{
			ioccc(argv[2]);
			return 0;
		}

		if(match(argv[1], "testdis"))
		{
			dis("disassembly.hs");
			return 0;
		}

		if(match(argv[1], "dis"))
		{
			dis(argv[2]);
			return 0;
		}

		if(match(argv[1], "asm"))
		{
			fp_reset("bin/raw");
			loadRaw(fp_get);
			run(ioget, pc);
			return EXIT_SUCCESS;
		}

		if(match(argv[1], "asmWith"))
		{
			fp_reset(argv[2]);
			loadRaw(fp_get);
			run(ioget, pc);
			return EXIT_SUCCESS;
		}

		file_print("bad command", stdout);
		return 0;
	}

	rpg();
	file_print(buf, stdout);
	file_print("\n", stdout);
	return EXIT_SUCCESS;
}
