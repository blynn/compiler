void *malloc(unsigned long);
typedef unsigned u;

static const u prog[];
static const u prog_size;
static u root[];
static const u root_size;

enum { FORWARD = 27, REDUCING = 9 };

enum { TOP = 1<<24 };
u *mem, *altmem, *sp, *spTop, hp;

static inline u isAddr(u n) { return n>=128; }

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

static inline u app(u f, u x) {
  mem[hp] = f;
  mem[hp + 1] = x;
  hp += 2;
  return hp - 2;
}

static inline u arg(u n) { return mem[sp [n] + 1]; }
static inline u num(u n) { return mem[arg(n) + 1]; }

static inline void lazy(u height, u f, u x) {
  u *p = mem + sp[height];
  *p = f;
  *++p = x;
  sp += height - 1;
  *sp = f;
}

static void lazy3(u height,u x1,u x2,u x3){u*p=mem+sp[height];sp[height-1]=*p=app(x1,x2);*++p=x3;*(sp+=height-2)=x1;}

static inline u apparg(u i, u j) { return app(arg(i), arg(j)); }

static void foreign(u n);

static void run() {
  for(;;) {
    if (mem + hp > sp - 8) gc();
    u x = *sp;
    if (isAddr(x)) *--sp = mem[x]; else switch(x) {
      case 'F': foreign(arg(1)); break;
      case 'Y': lazy(1, arg(1), sp[1]); break;
      case 'Q': lazy(3, arg(3), apparg(2, 1)); break;
      case 'S': lazy3(3, arg(1), arg(3), apparg(2, 3)); break;
      case 'B': lazy(3, arg(1), apparg(2, 3)); break;
      case 'C': lazy3(3, arg(1), arg(3), arg(2)); break;
      case 'R': lazy3(3, arg(2), arg(3), arg(1)); break;
      case 'V': lazy3(3, arg(3), arg(1), arg(2)); break;
      case 'I': sp[1] = arg(1); sp++; break;
      case 'T': lazy(2, arg(2), arg(1)); break;
      case 'K': lazy(2, 'I', arg(1)); break;
      case ':': lazy3(4, arg(4), arg(1), arg(2)); break;
      case '#': lazy(2, arg(2), sp[1]); break;
      case '=': num(1) == num(2) ? lazy(2, 'I', 'K') : lazy(2, 'K', 'I'); break;
      case 'L': num(1) <= num(2) ? lazy(2, 'I', 'K') : lazy(2, 'K', 'I'); break;
      case '*': lazy(2, '#', num(1) * num(2)); break;
      case '/': lazy(2, '#', num(1) / num(2)); break;
      case '%': lazy(2, '#', num(1) % num(2)); break;
      case '+': lazy(2, '#', num(1) + num(2)); break;
      case '-': lazy(2, '#', num(1) - num(2)); break;
      case '.': return;
      case FORWARD: return;  // die("stray forwarding pointer");
      default: return;  // printf("?%u\n", x); die("unknown combinator");
    }
  }
}

void rts_reduce(u) __attribute__((visibility("default")));
void rts_reduce(u n) {
  *(sp = spTop) = app(app(n, '?'), '.');
  run();
}

void rts_init() __attribute__((visibility("default")));
void rts_init() {
  mem = malloc(TOP * sizeof(u)); altmem = malloc(TOP * sizeof(u));
  hp = 128;
  for (u i = 0; i < prog_size; i++) mem[hp++] = prog[i];
  spTop = mem + TOP - 1;
}

static int env_argc;
int getargcount() { return env_argc; }
static char **env_argv;
int getargchar(int n, int k) { return env_argv[n][k]; }

#define EXPORT(f, sym, n) void f() asm(sym) __attribute__((visibility("default"))); void f(){rts_reduce(root[n]);}
