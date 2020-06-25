// Wasm runtime for our compiler.
// Expects:
//   ROOT_BASE: exported functions, followed by 0
//   HEAP_BASE - 4: program size
//   HEAP_BASE: program
typedef unsigned u;
enum{_UNDEFINED=0,_F,_Y,_Q,_S,_B,_C,_R,_V,_T,_K,_I,_CONS,_NUM,_ADD,_SUB,_MUL,_DIV,_MOD,_EQ,_LE,_NEWREF,_READREF,_WRITEREF,_END, FORWARD = 127, REDUCING = 126 };
#define IMPORT(m,n) __attribute__((import_module(m))) __attribute__((import_name(n)));
void putchar(int) IMPORT("env", "putchar");
int getchar(void) IMPORT("env", "getchar");
int eof(void) IMPORT("env", "eof");

enum {
  ROOT_BASE = 1<<9,
  HEAP_BASE = (1<<20) - 128 * sizeof(u),
  TOP = 1<<22
};
static u *mem, *altmem, *sp, *spTop, hp, *root = (u*) ROOT_BASE;
static inline u isAddr(u n) { return n>=128; }
static u evac(u n) {
  if (!isAddr(n)) return n;
  u x = mem[n];
  while (isAddr(x) && mem[x] == _T) {
    mem[n] = mem[n + 1];
    mem[n + 1] = mem[x + 1];
    x = mem[n];
  }
  if (isAddr(x) && mem[x] == _K) {
    mem[n + 1] = mem[x + 1];
    x = mem[n] = _I;
  }
  u y = mem[n + 1];
  switch(x) {
    case FORWARD: return y;
    case REDUCING:
      mem[n] = FORWARD;
      mem[n + 1] = hp;
      hp += 2;
      return mem[n + 1];
    case _I:
      mem[n] = REDUCING;
      y = evac(y);
      if (mem[n] == FORWARD) {
        altmem[mem[n + 1]] = _I;
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
  for(u *r = root; *r; r++) *r = evac(*r);
  *sp = evac(*spTop);
  while (di < hp) {
    u x = altmem[di] = evac(altmem[di]);
    di++;
    if (x != _F && x != _NUM) altmem[di] = evac(altmem[di]);
    di++;
  }
  spTop = sp;
  u *tmp = mem;
  mem = altmem;
  altmem = tmp;
}

static inline u app(u f, u x) { mem[hp] = f; mem[hp + 1] = x; return (hp += 2) - 2; }
static inline u arg(u n) { return mem[sp [n] + 1]; }
static inline int num(u n) { return mem[arg(n) + 1]; }
static inline void lazy2(u height, u f, u x) {
  u *p = mem + sp[height];
  *p = f;
  *++p = x;
  sp += height - 1;
  *sp = f;
}
static void lazy3(u height,u x1,u x2,u x3){u*p=mem+sp[height];sp[height-1]=*p=app(x1,x2);*++p=x3;*(sp+=height-2)=x1;}
static void foreign(u n) {
  switch(n) {
case 2: putchar(num(2));lazy2(4, app(arg(4), _K), arg(3)); break;case 1: lazy2(3, app(arg(3), app(_NUM, getchar())), arg(2)); break;case 0: lazy2(3, app(arg(3), app(_NUM, eof())), arg(2)); break;
  }
}
static void run() {
  for(;;) {
    if (mem + hp > sp - 8) gc();
    u x = *sp;
    if (isAddr(x)) *--sp = mem[x]; else switch(x) {
case _F:foreign(arg(1));break;
case _Y:lazy2(1,arg(1),sp[1]);break;
case _Q:lazy2(3,arg(3),app(arg(2),arg(1)));break;
case _S:lazy3(3,arg(1),arg(3),app(arg(2),arg(3)));break;
case _B:lazy2(3,arg(1),app(arg(2),arg(3)));break;
case _C:lazy3(3,arg(1),arg(3),arg(2));break;
case _R:lazy3(3,arg(2),arg(3),arg(1));break;
case _V:lazy3(3,arg(3),arg(1),arg(2));break;
case _T:lazy2(2,arg(2),arg(1));break;
case _K:lazy2(2,_I,arg(1));break;
case _I:sp[1] = arg(1); sp++;break;
case _CONS:lazy3(4,arg(4),arg(1),arg(2));break;
case _NUM:lazy2(2,arg(2),sp[1]);break;
case _ADD:lazy2(2,_NUM,num(1) + num(2));break;
case _SUB:lazy2(2,_NUM,num(1) - num(2));break;
case _MUL:lazy2(2,_NUM,num(1) * num(2));break;
case _DIV:lazy2(2,_NUM,num(1) / num(2));break;
case _MOD:lazy2(2,_NUM,num(1) % num(2));break;
case _EQ:num(1) == num(2) ? lazy2(2, _I, _K) : lazy2(2, _K, _I);break;
case _LE:num(1) <= num(2) ? lazy2(2, _I, _K) : lazy2(2, _K, _I);break;
case _NEWREF:lazy2(2,arg(2),sp[1]);break;
case _READREF:lazy3(3,arg(3),num(1),arg(2));break;
case _WRITEREF:lazy3(4,arg(4),((mem[arg(2) + 1] = arg(1)), _K),arg(3));break;
case _END:return;break;
    }
  }
}
void rts_init() {
  mem = (u*) HEAP_BASE; altmem = (u*) (HEAP_BASE + (TOP - 128) * sizeof(u));
  hp = 128 + mem[127];
  spTop = mem + TOP - 1;
}
void rts_reduce(u n) {
  static u ready;if (!ready){ready=1;rts_init();}
  *(sp = spTop) = app(app(n, _UNDEFINED), _END);
  run();
}
void fun(void) asm("fun") __attribute__((visibility("default")));
void fun(void) { rts_reduce(*((u*)512)); }
