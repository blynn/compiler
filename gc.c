void *malloc(unsigned long);
typedef unsigned u;

static const u prog[];
static const u prog_size;
static u root[];
static const u root_size;

enum { FORWARD = 27, REDUCING = 9 };
enum { TOP = 1<<24 };
static u *mem, *altmem, *sp, *spTop, hp;

static u isAddr(u n) { return n>=128; }

static u evac(u n) {
  if (!isAddr(n)) return n;
  u x = mem[n];
  while (isAddr(x) && mem[x] == 'T') {
    mem[n] = mem[n + 1];
    mem[n + 1] = mem[x + 1];
    x = mem[n];
  }
  if (isAddr(x) && mem[x] == 'K') {
    mem[n + 1] = mem[x + 1];
    x = mem[n] = 'I';
  }
  u y = mem[n + 1];
  switch(x) {
    case FORWARD: return y;
    case REDUCING:
      mem[n] = FORWARD;
      mem[n + 1] = hp;
      hp += 2;
      return mem[n + 1];
    case 'I':
      mem[n] = REDUCING;
      y = evac(y);
      if (mem[n] == FORWARD) {
        altmem[mem[n + 1]] = 'I';
        altmem[mem[n + 1] + 1] = y;
      } else {
        mem[n] = FORWARD;
        mem[n + 1] = y;
      }
      return mem[n + 1];
    default: break;
  }
  u z = hp;
  hp += 2;
  mem[n] = FORWARD;
  mem[n + 1] = z;
  altmem[z] = x;
  altmem[z + 1] = y;
  return z;
}

static void gc() {
  hp = 128;
  u di = hp;
  sp = altmem + TOP - 1;
  for(u i = 0; i < root_size; i++) root[i] = evac(root[i]);
  *sp = evac(*spTop);
  while (di < hp) {
    u x = altmem[di] = evac(altmem[di]);
    di++;
    if (x != 'F' && x != '#') altmem[di] = evac(altmem[di]);
    di++;
  }
  spTop = sp;
  u *tmp = mem;
  mem = altmem;
  altmem = tmp;
}

static u app(u f, u x) {
  mem[hp] = f;
  mem[hp + 1] = x;
  hp += 2;
  return hp - 2;
}
static u arg(u n) { return mem[sp [n] + 1]; }
static u num(u n) { return mem[arg(n) + 1]; }
static void foreign(u n);
static void run();
static void lazy(u height, u f, u x) {
  u *p = mem + sp[height];
  *p = f;
  *++p = x;
  sp += height - 1;
  *sp = f;
}

void rts_reduce(u) __attribute__((visibility("default")));
void rts_reduce(u n) {
  *(sp = spTop) = app(app(n, '?'), '.');
  run();
}

void rts_init() __attribute__((visibility("default")));
void rts_init() {
  mem = malloc(TOP * 2 * sizeof(u)); altmem = mem + TOP;
  hp = 128;
  for (u i = 0; i < prog_size; i++) mem[hp++] = prog[i];
  spTop = mem + TOP - 1;
}

static u pro_offset;

void rts_pro_init() __attribute__((visibility("default")));
void rts_pro_init() { pro_offset = hp - 128; }

void rts_pro(u) __attribute__((visibility("default")));
void rts_pro(u n) { mem[hp++] = n < 128 ? n : n + pro_offset; }

void rts_pro_end(u) __attribute__((visibility("default")));
void rts_pro_end(u n) { rts_reduce(n < 128 ? n : n + pro_offset); }

#define EXPORT(f, sym, n) void f() asm(sym) __attribute__((visibility("default"))); void f(){rts_reduce(root[n]);}
