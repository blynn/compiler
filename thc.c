typedef unsigned u;
#include <stdio.h>
enum { TOP = 1<<26 };
u heap[TOP], *sp, hp = 128;

u heapOn(u f, u x) {
  heap[hp] = f;
  heap[hp + 1] = x;
  hp += 2;
  return hp - 2;
}

//char *s = "`I`I`#66``C.`#101``C.0";
//char *s = "````:#B``:#eK0`T>";
char *s = "`$``:#```:#```:#```:#```:#:``:##``:#B``:#```:#```:#:``:##``:#e``:#K``:#0``:#```:#T``:#>K";
int readS() { return *s++; }

u parse(int (*next)()) {
  u c = next();
  printf("parsed: %c\n", c);
  u t;
  switch(c) {
    case '0': return 0;
    case '#': return heapOn('#', next());
    case '`':
      t = hp;
      hp += 2;
      heap[t] = parse(next);
      heap[t + 1] = parse(next);
      return t;
    default: return c;
  }
}

void reset(int (*next)()) {
  sp = heap + TOP - 3;
  *sp = parse(next);
}

u arg(u n) { return heap[sp [n] + 1]; }
u num(u n) { return heap[arg(n) + 1]; }

void lazy(u height, u f, u x) {
  u *p = heap + sp[height];
  *p = f;
  *++p = x;
  sp += height;
}

u harg(u i, u j) { return heapOn(arg(i), arg(j)); }

int run() {
  int ch;
  for(;;) {
    u x = *sp;
    if (!x) break;
    if (x < 128) switch(x) {
      case 'C': lazy(3, harg(1, 3), arg(2)); break;
      case 'I': lazy(2, arg(1), arg(2)); break;
      case 'T': lazy(2, arg(2), arg(1)); break;
      case 'K': lazy(2, 'I', arg(1)); break;
      //case '.': putchar(num(1)); lazy(2, 'I', arg(2)); break;
      case ':': lazy(4, harg(4, 1), arg(2)); break;
      case '>': putchar(num(1)); lazy(2, harg(2, 0), heapOn('T', '>')); break;
      case '<': ch = getchar(); ch < 0 ? lazy(1, 9, 8) : lazy(1, heapOn(':', heapOn('#', ch)), heapOn('<', 0)); break;
      case '#': lazy(2, arg(2), sp[1]); break;
      case 1: ch = num(1); lazy(2, harg(2, 0), heapOn('T', 1)); return ch;
      case '$': lazy(1, heapOn(arg(1), 0), heapOn('T', 1)); reset(run); break;
    } else {
      *--sp = heap[x];
    }
  }
  return 0;
}

int main() {
  reset(readS);
  for (u i = 128; i < hp; i++) printf(" %u", heap[i]); puts("");
  return run();
}
